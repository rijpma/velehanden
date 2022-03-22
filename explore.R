setwd("~/data/citsci")

library("data.table")
library("stringi")
library("anytime")
library("ggplot2")
library("readxl")
library("texreg")
library("alluvial")
library("RColorBrewer")

data.table::setDTthreads(2)

source("~/repos/citsci/functions.R")

mypar = function(...){
    par(..., 
        bty = "l", 
        mar = c(3, 3, 2, 1), 
        mgp = c(1.7, .5, 0), 
        tck=-.01,
        font.main = 1)
}

# project-level info
# for organisations
lijst = readxl::read_xlsx("~/repos/citsci/Data1_projecten_9.xlsx",
    sheet = "Complete lijst")
setDT(lijst)

projecten = fread("/Users/aukerijpma/data/citsci/20200210_portal_projecten.csv")
projecten = projecten[naam != "naam"]
projecten[, project_id := id]
projecten[, punten_bij_invoeren := as.numeric(punten_bij_invoeren)]
projecten[, punten_bij_controle := as.numeric(punten_bij_controle)]
projecten = projecten[naam != "demo"] # also not in idxr etc.

# forum data
users_projects <- fread("20200210_portal_gebruikers_projecten.csv")
users_projects <- users_projects[project_id != "project_id"]
users_projects[, saldo := as.numeric(saldo)]
users_projects[, spent_points := as.numeric(spent_points)]

messages <- fread("20200210_portal_messages.csv")
messages <- messages[project_id != "project_id"]

# merge project roles into the messages
# check whether the user ids of projects and messages are the same (maybe check using jiw where we can know this 100%)
messages = merge(
    messages, 
    users_projects[, list(project_id, user_id = gebruiker_id, rol)],
    by = c("project_id", "user_id"), all.x = TRUE, all.y = FALSE)
messages[, created_at := as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%OS")]

# entry data
ctrl = readRDS("allctrl.rds.gz")
idxr = readRDS("allidxr.rds.gz")

# remove headers inserted as data in 1st 5k rows
idxr = lapply(idxr, function(x) x[gebruiker_id != "gebruiker_id"])
ctrl = lapply(ctrl, function(x) x[gebruiker_id != "gebruiker_id"])

# entered separately and not yet processed, so select correct columns manually
ctrl$`20200211_lkm_index_controle_data.csv` = 
    ctrl$`20200211_lkm_index_controle_data.csv`[, .(gebruiker_id, scan_id, aangemaakt_op)]
idxr$`20200211_brd_indexeer_data.csv` = 
    idxr$`20200211_brd_indexeer_data.csv`[, .(gebruiker_id, scan_id, aangemaakt_op)]

# simplify names of data sets
names(idxr) = stri_replace_all_regex(names(idxr), "_indexeer_data\\.csv", "")
names(idxr) = stri_replace_all_regex(names(idxr), "^\\d+_", "")

names(ctrl) = stri_replace_all_regex(names(ctrl), "_controle_data\\.csv", "")
names(ctrl) = stri_replace_all_regex(names(ctrl), "^\\d+_", "")

# list to dataset
idxr = rbindlist(idxr, use.names = TRUE, idcol = "project")
ctrl = rbindlist(ctrl, use.names = TRUE, idcol = "project")

# collapse entries to scan keeping N
idxr = idxr[, .N, , by = list(project, scan_id, gebruiker_id, aangemaakt_op)]
ctrl = ctrl[, .N, , by = list(project, scan_id, gebruiker_id, aangemaakt_op)]

# actual dates/times
idxr[, aangemaakt_op := anytime::anytime(aangemaakt_op)]
ctrl[, aangemaakt_op := anytime::anytime(aangemaakt_op)]

setorder(idxr, project, scan_id, aangemaakt_op)
setorder(ctrl, project, scan_id, aangemaakt_op)

# two groups of outreach-heavy projects
group1 = projecten[id %in% c(208, 37, 22), naam]
group2 = projecten[id %in% c(236, 45), naam]

idxr[, group := "other"]
idxr[project %in% group1, group := paste0("g1_", project)]
idxr[project %in% group2, group := paste0("g2_", project)]

# did projects do double entry?
# nb: some entered > 2
idxr[, .N, by = .(project, scan_id)][, mean(N == 1)]
ctrl[, .N, by = .(project, scan_id)][, mean(N == 1)]

# new user variable
idxr[order(aangemaakt_op), newuser := !duplicated(gebruiker_id)]
ctrl[order(aangemaakt_op), newuser := !duplicated(gebruiker_id)]

# active user defined as "has contributed in past 6 months"
half_year = (365.25 / 2) * 24 * 60 * 60 # in seconds
idxr[order(aangemaakt_op), activeuser := aangemaakt_op - shift(aangemaakt_op) < half_year, by = gebruiker_id]
ctrl[order(aangemaakt_op), activeuser := aangemaakt_op - shift(aangemaakt_op) < half_year, by = gebruiker_id]
idxr[is.na(activeuser), activeuser := FALSE] # is this correct?
ctrl[is.na(activeuser), activeuser := FALSE]

# calculate delays to controle
delays = merge(
    x = idxr[, list(date_entered = last(aangemaakt_op),
                    group = group[1]), 
            by = list(project, scan_id)], 
    y = ctrl[, list(date_checked = last(aangemaakt_op)), 
            by = list(project, scan_id)], 
    by = c("project", "scan_id"),
    suffixes = c("_idxr", "_ctrl"),
    all.x = TRUE, all.y = TRUE)
delays = delays[, 
    list(date_entered, 
        date_checked = fifelse(is.na(date_checked), max(date_checked, na.rm = TRUE), date_checked), 
        share_checked = mean(!is.na(date_checked))), 
    by = c("group", "project")]
delays[, delay := difftime(date_checked, date_entered, units = "days")]

# proposition 1 #
# ------------- #
# share new volunteers by project
idxr[, activity := .N, by = gebruiker_id]
new_volunteers = idxr[,
    list(
        start = min(aangemaakt_op),
        ratio_new_volunteers = ratio_new_volunteers(.SD),
        nvolunteers = uniqueN(gebruiker_id),
        nscans = .N),
    by = list(project)]

# distribution of shares new volunteers
pdf("~/repos/citsci/out/new_volunteers_byproject.pdf")
mypar()
hist(new_volunteers$ratio_new_volunteers, 
    xlab = "Share new volunteers in project", 
    main = "")
dev.off()

new_volunteers[year(start) > 2011, weighted.mean(ratio_new_volunteers, nvolunteers)]

# list high share projects that are not small
new_volunteers[ratio_new_volunteers > 0.8 & nvolunteers > 10 & year(start) > 2012, ]

# calculate future activity of the average new v. average pre-existing volunteer
# first a variable highlighting new users throughout their first project
idxr[, new4project := any(newuser), by = list(gebruiker_id, project)]

newoldprop = idxr[year(aangemaakt_op) > 2011, 
    list(
        new = sum(N[new4project == TRUE]),
        old = sum(N[new4project == FALSE]),
        nvolunteers = uniqueN(gebruiker_id)),
    by = list(project)]
newoldprop[, total := old + new]
newoldprop[, perc := old / total]

pdf("~/repos/citsci/out/perc_by_new.pdf")
mypar()
hist(newoldprop$perc)
dev.off()
newoldprop[, mean(perc)]
newoldprop[, weighted.mean(perc, nvolunteers)]

newoldprop[, prop := old / new]
newoldprop[!between(prop, 0.1, 10)]

pdf("~/repos/citsci/out/new_old_volunteers_contributions.pdf")
mypar()
plot(new ~ old, 
    data = newoldprop, 
    log = "xy", 
    xlab = "Input pre-existing volunteers", ylab = "Input new volunteers", 
    pch = 20,
    xlim = range(newoldprop[, list(old, new)] + 1),
    ylim = range(newoldprop[, list(old, new)] + 1))
# curve(2*x, add = TRUE)
curve(1*x, add = TRUE)
dev.off()

toplot = new_volunteers[order(year), mean(V1), by = list(year)][as.numeric(year) >= 2012]
pdf("~/repos/citsci/out/newvolunteers_overtime.pdf")
mypar()
plot(toplot, 
    type = "b", col = "red", lwd = 1.5, pch = 20,
    ylab = "Average share new volunteers")
dev.off()

# pool of active/new volunteers at any time
# since newuser has activeuser = false, is the -new really necessary? 

active_userpool = idxr[, 
    list(active = uniqueN(gebruiker_id[activeuser == TRUE]), 
         new = uniqueN(gebruiker_id[newuser == TRUE])), 
    by = list(quarter = zoo::as.yearqtr(aangemaakt_op))]
pdf("~/repos/citsci/out/activepool.pdf")
mypar()
plot(active_userpool[order(quarter), list(month = quarter, active - new)], 
    type = 'b', col = "red", lwd = 1.5, pch = 20,
    ylab = "N active, pre-existing volunteers")
dev.off()

# experience and activity
# user level
setorder(idxr, aangemaakt_op)
idxr[, newinproject := !duplicated(gebruiker_id), by = project]
idxr[, experience := 1:.N, by = gebruiker_id]
exp = idxr[, 
    list(
        activity = .N / as.numeric(diff(range(as.Date(aangemaakt_op))) + 1),
        experience = experience[newinproject == TRUE]), # -1 but then no log :( 
    by = list(project, gebruiker_id)]
plot(log(activity) ~ log(experience), data = exp)
abline(lm(log(activity) ~ log(experience), data = exp), col = 2)
summary(lm(log(activity) ~ log(experience), data = exp), col = 2)

ggplot(exp, aes(log(experience), log(activity))) + geom_point() + geom_smooth() + theme_classic()


# but of course, this does not necessarily mean that there is no project-wide effect, because we know contributions can be very lopsided
# so once more, but to the project level
exp = idxr[year(aangemaakt_op) > 2011, 
    list(
        start = min(aangemaakt_op),
        share_new = sum(newuser) / uniqueN(gebruiker_id),
        activity = .N / as.numeric(diff(range(as.Date(aangemaakt_op))) + 1),
        experience = mean(experience[newinproject == TRUE][1:floor(uniqueN(gebruiker_id) / 2)])), 
        # so we look at experience of first 50% of users
        # -1 but then no log :( 
    by = list(project)]
# 

pdf("~/repos/citsci/out/experience_activity_proj.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log(activity) ~ share_new, data = exp, 
    pch = 20,
    xlab = "share new volunteers",
    ylab = "log(entries per day)")
m_share = lm(log(activity) ~ share_new, data = exp)
abline(m_share, col = 2)
plot(log(activity) ~ log(experience), data = exp, 
    pch = 20,
    xlab = "log(average volunteer experience)",
    ylab = "log(entries per day)")
m_exp = lm(log(activity) ~ log(experience), data = exp)
abline(m_exp, col = 2)
dev.off()

m_exp2 = update(m_exp, . ~ . + as.numeric(start))
mlist = list(m_share, m_exp, m_exp2)
texreg::screenreg(mlist)
lapply(mlist, lmtest::coeftest, vcov. = sandwich::vcovHC)
lapply(mlist, function(x) sqrt(diag(sandwich::vcovHC(x))))
# lapply(mlist, coeftest, vcov. = sandwich::vcovHC)


# proposition 2 #
# ------------- #
# volunteers stay with the same organisation
# and
# proposition 3 #
# ------------- # 
# volunteers stay with the same type of project

# fixes to messy lijst (projecten does not have customer names)
# in idxr and projecten but not in lijst
# corrections first
lijst[Titel == "Zaanse briefhoofden", Naam := "zaa_index_bron"]
lijst[Titel == "30 dagen op zee", Naam := "moei_index_30dagen"]
lijst[Titel == "Nieuws! Lokale kronieken, 1500-1850", Naam := "vua_train_kronieken_transkribus"]
lijst[Titel == "Nieuws! Lokale kronieken, 1500-1850", Klantnaam := "Universiteit Leiden"]

# 4 projects but not in our data
lijst = lijst[Klantnaam != "Tresoar"]

# missing from lijst
add = fread(
   '"Naam"                            , "Klantnaam"
    "amc"                             , "AMC"
    "utr_index_bvr"                   , "Het Utrechts Archief"
    "gda_index_huwelijk"              , "Gelders Archief"
    "kem_index_dwaallichten"          , "Rijksarchief Antwerpen"
    "sahm_index_bevolkingsregisters"  , "Streekarchief Hollands Midden"
    "zaa_index_gaarder"               , "Gemeentearchief Zaanstad"')
lijst = rbindlist(list(lijst, add), fill = TRUE)

# list of largest organisations
fwrite(lijst[, .N, by = Klantnaam][order(-N)][1:10],
    "~/repos/citsci/out/organisation.csv")

setdiff(lijst$Naam, projecten$naam)
setdiff(lijst$Naam, idxr$project)

# drop because not in projecten and idxr
lijst = lijst[!Naam %in% c("frl_bso_index", "unesco_tagging_photos")]

# merge org names into idxr
idxr = lijst[, list(Naam, Klantnaam)][idxr, on = c(Naam = "project")]
setnames(idxr, "Naam", "project")
setnames(idxr, "Klantnaam", "org")

# merge project types into idxr
idxr = projecten[, list(naam, project_soort)][idxr, on = c(naam = "project")]
setnames(idxr, "naam", "project")

# calculate the expected number based on random project draw by permuting projects first
vpo = idxr[!is.na(org), 
    list(aangemaakt_op = min(aangemaakt_op)), 
    by = list(project, org, project_soort, gebruiker_id)]
vpo = vpo[order(gebruiker_id, aangemaakt_op)]
# should we do something about from == 1 not making sense? No, because this is tricky here and the placebo thing should take care of it

# example table
set.seed(321)
vpo[, uniqueN(org), by = gebruiker_id][order(V1)]
vpo[, prob_org := .N / nrow(vpo), by = org]
example = vpo[gebruiker_id %in% c(3280, 4260, 4804)][sample(.N, 20, prob = prob_org)][order(gebruiker_id, aangemaakt_op, org)]
example[, return := duplicated(org), by = gebruiker_id]

example = example[, list(
    volunteer = as.numeric(as.factor(gebruiker_id)), 
    date = as.Date(aangemaakt_op),
    organisation = org, 
    return = return,
    permuted = sample(org))]
example[, return_permuted := duplicated(permuted), by = volunteer]
knitr::kable(example)
writeLines(knitr::kable(example, "html"), 
    "~/repos/citsci/out/placebo_example.html")

# some sort of inverse weight to add up?
vpo[, invweight := 1 / uniqueN(project), by = org]
# vpo[, invweight := 1 / uniqueN(gebruiker_id), by = org]

set.seed(7/4)
vpo[, placeboorg := sample(org)] # this accounts for the number of volunteers, but not the number of scans
vpo[, placebotype := sample(project_soort)] # this accounts for the number of volunteers, but not the number of scans

vpo = vpo[, list(
        nproj = .N, 
        same_org = mean(duplicated(org)),
        same_org_wgt = sum(invweight[duplicated(org)]),
        same_placebo_org = mean(duplicated(placeboorg), na.rm = TRUE),
        same_org_direct = mean(org == shift(org, fill = FALSE), na.rm = TRUE),
        same_placebo_org_direct = mean(placeboorg == shift(placeboorg, fill = FALSE), na.rm = TRUE),
        org_mix = uniqueN(org) / .N, 
        placebo_org_mix = uniqueN(org) / .N,
        # same for project_type
        same_type = mean(duplicated(project_soort)),
        same_placebo_type = mean(duplicated(placebotype), na.rm = TRUE),
        same_type_direct = mean(project_soort == shift(project_soort, fill = FALSE), na.rm = TRUE),
        same_placebo_type_direct = mean(placebotype == shift(placebotype, fill = FALSE), na.rm = TRUE),
        type_mix = uniqueN(project_soort) / .N, 
        placebo_type_mix = uniqueN(project_soort) / .N), 
    by = gebruiker_id]

pdf("~/repos/citsci/out/returns.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))

plot(vpo[, .N, by = floor(same_org * 10) / 10][order(floor)], 
    main = "Returns to organisation",
    xlab = "Share returns",
    ylab = "N volunteers",
    lwd = 1.5, log = 'y',, type = 'o', pch = 19)
lines(vpo[, .N, by = floor(same_placebo_org * 10) / 10][order(floor)], 
    col = 2, lwd = 1.5, type = 'o', pch = 19)

plot(vpo[, .N, by = floor(same_org_direct * 10) / 10][order(floor)], 
    main = "Direct returns to organisation",
    xlab = "Share returns",
    ylab = "",
    lwd = 1.5, log = 'y', type = 'o', pch = 19)
lines(vpo[, .N, by = floor(same_placebo_org_direct * 10) / 10][order(floor)], 
    col = 2, lwd = 1.5, type = 'o', pch = 19)

legend("topright", fill = 1:2, legend = c("Actual", "Baseline"))

# plot(vpo[, .N, by = floor(same_type * 10) / 10][order(floor)], 
#     main = "Returns to project type",
#     xlab = "Share of volunteers' projects",
#     lwd = 1.5, log = 'y', , type = 'o', pch = 19)
# lines(vpo[, .N, by = floor(same_placebo_type * 10) / 10][order(floor)], 
#     col = 'red', lwd = 1.5, type = 'o', pch = 19)

# plot(vpo[, .N, by = floor(same_type_direct * 10) / 10][order(floor)], 
#     main = "Direct returns to project type",
#     xlab = "Share of volunteers' projects",
#     ylab = "",
#     lwd = 1.5, log = 'y', , type = 'o', pch = 19)
# lines(vpo[, .N, by = floor(same_placebo_type_direct * 10) / 10][order(floor)], 
#     col = 'red', lwd = 1.5, type = 'o', pch = 19)
dev.off()

mean(vpo[nproj > 1, same_org])
mean(vpo[nproj > 1, same_org_wgt])
mean(vpo[nproj > 1, same_placebo_org])
t.test(vpo[nproj > 1, same_org], vpo[nproj > 1, same_placebo_org])
mean(vpo[nproj > 1, same_org_direct])
mean(vpo[nproj > 1, same_placebo_org_direct])
t.test(vpo[nproj > 1, same_org_direct], vpo[nproj > 1, same_placebo_org_direct])
mean(vpo[nproj > 1, same_type])
mean(vpo[nproj > 1, same_placebo_type])
t.test(vpo[nproj > 1, same_type], vpo[nproj > 1, same_placebo_type])
mean(vpo[nproj > 1, same_type_direct])
mean(vpo[nproj > 1, same_placebo_type_direct])
t.test(vpo[nproj > 1, same_type_direct], vpo[nproj > 1, same_placebo_type_direct])

idxr[, nproj_byorg := uniqueN(project), by = org]
idxr[, largeorg := ifelse(org %in% c("Het Utrechts Archief",
        "Brabants Historisch Informatie Centrum",
        "Stadsarchief Amsterdam",
        "Westfries Archief",
        "Archief Eemland",
        "Regionaal Archief Nijmegen",
        "Tresoar"),
    org, "other")]
toplot = idxr[!is.na(org), list(aangemaakt_op = min(aangemaakt_op)), by = list(project, org, largeorg, project_soort, nproj_byorg, gebruiker_id)]
toplot = toplot[order(gebruiker_id, aangemaakt_op)]
toplot = toplot[, 
    list(
        from = largeorg, 
        nfrom = nproj_byorg, 
        to = shift(largeorg)), 
    by = gebruiker_id]
toplot = toplot[!is.na(to) & nfrom > 1, .N, by = list(from, to)]
toplot[, col := RColorBrewer::brewer.pal(uniqueN(from), "Set3")[as.factor(from)]]
toplot = toplot[order(-N)]
toplot[, from := factor(from, levels = unique(from))]
toplot[, to := factor(to, levels = unique(to))]

pdf("~/repos/citsci/out/flows_org.pdf", width = 9)
alluvial(toplot[, 1:2], freq = toplot$N, col = toplot$col, cw = 0.2, cex = 0.8)
dev.off()

# organisation size and retention
toplot = idxr[!is.na(org), list(aangemaakt_op = min(aangemaakt_op)), by = list(project, org, largeorg, project_soort, nproj_byorg, gebruiker_id)]
toplot = toplot[order(gebruiker_id, aangemaakt_op)]
toplot = toplot[, 
    list(
        from = org, 
        nfrom = nproj_byorg, 
        to = shift(org)), 
    by = gebruiker_id]
toplot = toplot[!is.na(to) & nfrom > 1, list(.N, nfrom = unique(nfrom)), by = list(from, to)]
toplot = toplot[, list(N[from == to] / sum(N), unique(nfrom)), by = list(from)]
toplot[order(V1)]

# proposition 4 #
# ------------- # 
# forum activity and project speed

proj_speed = idxr[, 
    list(scans_per_week = .N), 
    by = list(project, week = week(aangemaakt_op), year = year(aangemaakt_op))]
proj_speed = projecten[, .(project_id = id, project = naam)][proj_speed, on = "project"]

forum_activity = messages[, 
    list(messages_per_week = .N,
        volunteer_messages_per_week = sum(rol == "invoerder", na.rm = TRUE),
        non_volunteer_messages_per_week = sum(rol != "invoerder", na.rm = TRUE)), 
    by = list(project_id, week = week(created_at), year = year(created_at))]
# nb: NAs are omitted

toplot = merge(
    x = proj_speed, 
    y = forum_activity, 
    by = c("project_id", "year", "week"))
toplot[order(year, week), messages_last_week := shift(messages_per_week, type = "lag")]
toplot[order(year, week), non_volunteer_messages_last_week := shift(non_volunteer_messages_per_week, type = "lag")]
toplot = merge(toplot,
    idxr[, list(project_volunteers = uniqueN(gebruiker_id)), by = project],
    by = "project")
toplot = merge(toplot,
    messages[, list(forum_users = uniqueN(user_id)), by = project_id],
    by = "project_id")

toplot_proj = toplot[, 
    list(total_scans = sum(scans_per_week), 
        total_messages = sum(messages_per_week), 
        nonvol_total_messages = sum(non_volunteer_messages_per_week, na.rm = TRUE), 
        project_volunteers = project_volunteers[1], 
        forum_users = forum_users[1]), 
    by = project_id][, -"project_id"]

pdf("~/repos/citsci/out/posts_scans_project.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log(total_scans) ~ log(total_messages), data = toplot_proj,
    pch = 20, main = "All messages", 
    xlab = "log(messages)", ylab = "log(entries)")
m1 = lm(log(total_scans) ~ log(total_messages), data = toplot_proj)
abline(m1, col = 2, lwd = 1.5)
plot(log(total_scans) ~ log(nonvol_total_messages), data = toplot_proj[nonvol_total_messages > 0],
    pch = 20, main = "Only non-volunteer messages", 
    xlab = "log(messages)", ylab = "log(scans)")
m2 = lm(log(total_scans) ~ log(nonvol_total_messages), data = toplot_proj[nonvol_total_messages > 0])
abline(m2, col = 2, lwd = 1.5)
dev.off()

pdf("~/repos/citsci/out/posts_scans_weekly.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(messages_per_week), data = toplot,
    pch = 20, main = "All messages", 
    xlab = "log(messages)", ylab = "log(entries)")
m3 = lm(log(scans_per_week) ~ log(messages_per_week), data = toplot)
abline(m3, col = 2, lwd = 1.5)
plot(log(scans_per_week) ~ log(non_volunteer_messages_per_week), data = toplot,
    pch = 20, main = "Only non-volunteer messages", 
    xlab = "log(messages)", ylab = "log(scans)")
m4 = lm(log(scans_per_week) ~ log(non_volunteer_messages_per_week), data = toplot[non_volunteer_messages_per_week > 0])
abline(m4, col = 2, lwd = 1.5)
dev.off()

texreg(list(m1, m2, m3, m4), file = "~/repos/citsci/out/correlations.tex")

pdf("~/repos/citsci/out/posts_scans_weekly_lagged.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(messages_last_week), 
    data = toplot, 
    pch = 20)
m5 = lm(log(scans_per_week) ~ log(messages_last_week), data = toplot)
abline(m5, col = 2, lwd = 1.5)
plot(log(scans_per_week) ~ log(non_volunteer_messages_last_week), 
    data = toplot[non_volunteer_messages_last_week > 0], 
    pch = 20)
m6 = lm(log(scans_per_week) ~ log(non_volunteer_messages_last_week), 
    data = toplot[non_volunteer_messages_last_week > 0])
abline(m6, col = 2, lwd = 1.5)
dev.off()

# proposition 5 #
# ------------- #
# response times
messages[parent_id == "", parent_id := id]
messages[, thread_length := .N, by = parent_id]
messages[, mean(thread_length == 1)]
response_times = messages[thread_length > 1][
    order(parent_id, created_at), 
    list(
        topic_started = created_at[1],
        first_response = created_at[2]),
    by = list(parent_id, project_id)]
response_times[, response_time := first_response - topic_started]

response_times = projecten[, .(project_id, naam)][response_times, on = "project_id"]
response_times[, group := ifelse(naam %in% ..group1, "group1", "other")]
response_times[, group := ifelse(naam %in% ..group2, "group2", group)]

pdf("~/repos/citsci/out/hist_weekly_response_times.pdf")
mypar()
hist(log(as.numeric(response_times$response_time)), breaks = 30,
    xlab = "log(response time in seconds)", main = "")
dev.off()

quantile(na.omit(response_times$response_time) / 60, 1:10/10)

pdf("~/repos/citsci/out/dens_weekly_response_times_bygroup.pdf")
mypar()
plot(density(log(as.numeric(response_times[!is.na(response_time), response_time]))), 
    type = "n",
    main = "Density plot of response times", xlab = "log(response time)")
lines(density(log(as.numeric(response_times[!is.na(response_time) & group == "other", response_time]))), col = "black")
lines(density(log(as.numeric(response_times[!is.na(response_time) & group == "group1", response_time]))), col = "red")
lines(density(log(as.numeric(response_times[!is.na(response_time) & group == "group2", response_time]))), col = "blue")
legend("topright", fill = c("black", "red", "blue"), legend = c("other", "group1", "group2"))
dev.off()

response_times[, mean(response_time, na.rm = TRUE) / 60 / 60, by = group]
response_times[, median(response_time, na.rm = TRUE) / 60 / 60, by = group]

# median response time because massive outliers
pdf("~/repos/citsci/out/responsetimes_over_time.pdf")
mypar()
plot(response_times[!is.na(first_response), median(response_time / 3600, na.rm = TRUE), by = zoo::as.yearmon(first_response)][order(zoo)], 
    ylab = "response time (hours)", type = 'b', log = 'y', col = 2, pch = 20)
dev.off()

# before after break (hours)
response_times[!is.na(first_response), median(response_time / 3600, na.rm = TRUE), by = zoo::as.yearmon(first_response) > 2016.6][order(zoo)]

toplot_forum = response_times[, 
    list(mean_response_time = mean(as.numeric(response_time) / 60 / 60)), 
    by = list(project_id, week = week(first_response), year = year(first_response))]

toplot_forum = proj_speed[toplot_forum, on = c("project_id", "week", "year")]
toplot_forum_proj = toplot_forum[, list(scans = sum(scans_per_week), mean_response_time = mean(mean_response_time)),
    by = project_id]

# add delays
toplot_entry = delays[delay > (2 / 84600), # so quick is probably auto-accept
    list(mean_check_time = mean(as.numeric(delay)) * 24, .N),
    by = list(project, week(date_checked), year(date_checked))]
toplot_entry = toplot_entry[mean_check_time > 0.01] # these are real, but not realistic weekly estimates, typically N ~= 1
toplot_entry = proj_speed[toplot_entry, on = c("project", "week", "year")]
toplot_entry_proj = toplot_entry[, list(scans = sum(scans_per_week, na.rm = TRUE), mean_check_time = mean(mean_check_time)),
    by = project]

pdf("~/repos/citsci/out/forumdelays_activity.pdf", height = 5, width = 9)
mypar(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(mean_response_time), 
    data = toplot_forum, 
    pch = 20,
    xlab = "log(mean response time (hours))",
    ylab = "log(scans per week)",
    main = "Forum responses and activity (volunteer)")
m1 = lm(log(scans_per_week) ~ log(mean_response_time), data = toplot_forum)
abline(m1, col = 2)
plot(log(scans) ~ log(mean_response_time), 
    data = toplot_forum_proj[,-"project_id"], 
    pch = 20,
    xlab = "log(mean response time hours)",
    ylab = "log(scans per week)",
    main = "Forum responses and activity (project)")
m2 = lm(log(scans) ~ log(mean_response_time), data = toplot_forum_proj[,-"project_id"])
abline(m2, col = 2)
dev.off()

pdf("~/repos/citsci/out/checkdelays_activity.pdf", height = 5, width = 9)
mypar(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry, 
    pch = 20,
    xlab = "log(mean response time (hours))",
    ylab = "log(scans per week)",
    main = "Entry checks and activity (volunteer)")
m3 = lm(log(scans_per_week) ~ log(mean_check_time), data = toplot_entry)
abline(m3, col = 2)
# is there an issue here in the sense that if a user does 1000 scans and the project takes a year, than this places some limit on how slow it can be checked
# ie we're estimate N/week = time, so does this introduce an artificial relationship of the kind x 1/x?
plot(log(scans) ~ log(mean_check_time), 
    data = toplot_entry_proj, 
    pch = 20,
    xlab = "log(mean response time (hours))",
    ylab = "log(scans per week)",
    main = "Entry checks and activity (project)")
m4 = lm(log(scans) ~ log(mean_check_time), data = toplot_entry_proj)
abline(m4, col = 2)
dev.off()
# maybe this should not be volunteer weekly but rather volunteer project? But this misses changes within the project

lapply(list(m1, m3), lmtest::coeftest, vcov. = sandwich::vcovCL, cluster = ~ project_id)
lapply(list(m2, m4), lmtest::coeftest, vcov. = sandwich::vcovHC)
texreg::screenreg(list(m1, m2, m3, m4))
texreg::texreg(list(m1, m2, m3, m4), 
    file = "~/repos/citsci/out/correlations_delays.tex")

# do controleurs also have different roles?
users_projects[, list(nroles = uniqueN(rol)), by = gebruiker_id][, sum(nroles > 1)]
length(intersect(idxr$gebruiker_id, ctrl$gebruiker_id))

# proposition 6/7 #
# delays and point system
# ----------------#
projecten[, incentive := punten_bij_controle / punten_bij_invoeren]
out = projecten[, .N, by = list(punten_bij_invoeren, punten_bij_controle, incentive)][order(punten_bij_invoeren, punten_bij_controle)]
fwrite(out, "~/repos/citsci/out/punten.csv")

# simplified point system
idxr = projecten[, list(project = naam, project_id, punten_bij_invoeren, punten_bij_controle)][idxr, on = c("project")]

# how many points are spent?
# user 6694 is a sysadmin with 12m points, 0 spent, and no last activity, so we drop
users_projects[gebruiker_id != 6694, sum(spent_points)]
users_projects[gebruiker_id != 6694, sum(spent_points)] / 
    idxr[, sum(punten_bij_invoeren) + sum(punten_bij_controle)]
users_projects[gebruiker_id != 6694, sum(spent_points)] / users_projects[gebruiker_id != 6694, sum(saldo)]
users_projects[gebruiker_id != 6694, sum(spent_points)] / users_projects[gebruiker_id != 6694, sum(saldo + spent_points)]

idxr[gebruiker_id == 6694]

# how many points are at lease 1000 in one project
idxr[, sum(punten_bij_invoeren + punten_bij_controle), by = list(gebruiker_id, project)][, mean(V1 > 1000)]
# how many of the 40m points is this?
idxr[, sum(punten_bij_invoeren + punten_bij_controle), by = list(gebruiker_id, project)][, list(sum(V1[V1 > 1000]), sum(V1))][, V1 / V2]

pdf("~/repos/citsci/out/saldo_v_points.pdf")
mypar(mfrow = c(1, 2))
x = merge(
    idxr[, sum(punten_bij_invoeren + punten_bij_controle), by = project_id],
    users_projects[gebruiker_id != 6694, sum(saldo + spent_points), by = project_id],
    by = "project_id")
plot(x[, -"project_id"], log = 'xy', 
    main = "Project",
    pch = 20,
    xlab = "total points from entries",
    ylab = "total points from balance")
curve(1 * x, add = TRUE)
x = merge(
    idxr[, sum(punten_bij_invoeren + punten_bij_controle), by = gebruiker_id],
    users_projects[gebruiker_id != 6694, sum(saldo + spent_points), by = gebruiker_id],
    by = "gebruiker_id")
plot(x[, -"gebruiker_id"], log = 'xy',
    main = "user",
    pch = 20,
    xlab = "total points from entries",
    ylab = "total points from balance")
curve(1 * x, add = TRUE)
dev.off()
# is the delay issue driven by point spending possibility?
# ok so generally this is ok? just not in a couple of cases?
x[, list(V1.x / V1.y)]
x[, list(V1.x / V1.y)][, .N, by = round(V1, 1)][order(N)]
x[, list(V1.x / V1.y)][, .N, by = round(V1, 1)][order(round)]
hist(x[, list(V1.x / V1.y)][V1 < 10, V1], breaks = 100)
hist(x[, list(V1.x / V1.y)][V1 < 100, V1], breaks = 100)
x[, quantile(V1.x / V1.y, na.rm = TRUE, 0:10 / 10)]
x[, mean(between(V1.x / V1.y, 0.8, 1.2), na.rm = TRUE)]



# plot delay and split by:
    # yes/no coupons
    # autocontrole
    # incentives from table
toplot_entry = projecten[, list(project = naam, project_id, has_coupons, has_coupon_create, has_transactions, punten_bij_invoeren, punten_bij_controle, incentive)][toplot_entry, on = c("project")]
pdf("~/repos/citsci/out/delays_activity_bypoints.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry, type = "n", 
    xlab = "log(mean check time (hours))",
    ylab = "log(scans per week)",
    main = "No coupons")
points(log(scans_per_week) ~ log(mean_check_time), 
    pch = 20,
    data = toplot_entry[has_coupons == 0])
m1 = lm(log(scans_per_week) ~ log(mean_check_time), data = toplot_entry[has_coupons == 0])
abline(m1, col = 2, lwd = 1.5)

plot(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry, type = "n" , 
    xlab = "log(mean check time (hours))",
    ylab = "log(scans per week)",
    main = "Coupons")
points(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry[has_coupons == 1],
    pch = 20)
m2 = lm(log(scans_per_week) ~ log(mean_check_time), data = toplot_entry[has_coupons == 1])
abline(m2, col = 2, lwd = 1.5)
dev.off()

pdf("~/repos/citsci/out/delays_activity_bypointscreation.pdf", width = 9, height = 5)
plot(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry, type = "n", main = "No coupons creation")
points(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry[has_coupon_create == 0],
    pch = 2-)
m3 = lm(log(scans_per_week) ~ log(mean_check_time), data = toplot_entry[has_coupon_create == 0])
abline(m3, col = 2, lwd = 1.5)

plot(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry, type = "n", main = "Coupons creation")
points(log(scans_per_week) ~ log(mean_check_time), 
    data = toplot_entry[has_coupon_create == 1],
    pch = 20)
m4 = lm(log(scans_per_week) ~ log(mean_check_time), data = toplot_entry[has_coupon_create == 1])
abline(m4, col = 2, lwd = 1.5)
dev.off()

texreg::screenreg(list(m1, m2))
m1 = lm(log(scans_per_week) ~ log(mean_check_time)*has_coupons, data = toplot_entry)
m2 = lm(log(scans_per_week) ~ log(mean_check_time)*has_coupon_create, data = toplot_entry)
texreg::screenreg(list(m1, m2))
lapply(list(m1, m2), lmtest::coeftest, vcov. = sandwich::vcovHC)
lapply(list(m1, m2), lmtest::coeftest, vcov. = sandwich::vcovCL, cluster = ~ project)
texreg::texreg(list(m1, m2),
    file = "~/repos/citsci/out/correlations_delays_points.tex")

# preliminaries #
# ------------- #
# basic development of platform
pdf("~/repos/citsci/out/platformoverall.pdf", height = 9)

mypar(mfrow = c(3, 2))

toplot = idxr[, list(scans = sum(N)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, main = "Scans")

toplot = idxr[, list(projects = uniqueN(project)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, main = "Active projects")

# one of these two should not have "scans" as the y-axis
toplot = idxr[, list(new_volunteers = sum(newuser)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, main = "New volunteers")
toplot[order(new_volunteers)]

toplot = idxr[, list(active_volunteers = uniqueN(gebruiker_id)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, main = "Active volunteers")

toplot = messages[!is.na(created_at), list(posts = .N), by = list(month = zoo::as.yearmon(created_at))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, main = "Forum messages")
toplot[order(posts)]

dev.off()

# the peaks
toplot = messages[!is.na(created_at), list(posts = .N), by = list(month = zoo::as.yearmon(created_at), project_id)]
toplot[order(posts)]
projecten[id == 242]
# peak forums july 2017 is one project, surinam slavery registers

toplot = idxr[, list(new_volunteers = sum(newuser)), by = list(month = zoo::as.yearmon(aangemaakt_op), project)]
toplot[, monthtotal := sum(new_volunteers), by = month]
toplot[order(new_volunteers), list(month, project, new_volunteers, new_volunteers / monthtotal)]
# peak new volunteers in nov 2011 is one project, militieregisters, then the
# only project on platform. other peaks can typically also be linked to a
# single project June 2017 (Slavery registers; 46%), May 2014 (WieWasWie
# bevolkingsregisetrs; 77%), October 2018 (Holland Amerika Lijn
# Passagierslijsten; 67%), May 2018 (Captions for Cas [Oorthuys]; 68%) can
# typically also be linked to a single project 

# summary statistics #

projecten[naam != "demo", .N, by = status]
idxr[project != "demo", range(aangemaakt_op)]
idxr[project != "demo", uniqueN(org)]
lijst[Naam != "demo", uniqueN(Klantnaam)]
idxr[, uniqueN(gebruiker_id)]
idxr[, uniqueN(paste0(scan_id, project))]
# or
nrow(idxr) + nrow(ctrl)

sumstatlist = list(
    volunteers = idxr[, uniqueN(gebruiker_id), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    scans = idxr[, uniqueN(scan_id), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    duration = idxr[, as.numeric(diff(range(aangemaakt_op), units = "days")), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    project_appeal = idxr[, project_appeal(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    activity_ratio = idxr[, activity_ratio(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # variation_periodicity does not seem to work
    # omit distribution effort as well?
    distribution_effort = 
        # omit these two project because they only had one contributor (gini = 0)
        idxr[!project %in% c("bhic", "picvh_pvdm"), distribution_effort(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    public_contribution = idxr[, public_contribution(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # sustained_engagement requires dropping 1 contribution volunteers
    ratio_new_volunteers = idxr[, ratio_new_volunteers(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    ratio_platform_members = idxr[, ratio_platform_members(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    feedback_delay = delays[delay > (2 / 84600), feedback_delay(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    n_messages = messages[, .N, by = project_id][, 
            list(min = min(N), median = median(N), mean =  mean(N), max = max(N))],
    response_times = response_times[!is.na(response_time), mean(as.numeric(response_time)), by = project_id][, 
            list(min = min(V1), median = median(V1), mean =  mean(V1), max = max(V1))] / 60 / 60
    )
)
out = rbindlist(sumstatlist, idcol = "stat", use.names = TRUE, fill = TRUE)
writeLines(
    knitr::kable(out, digits = 2, format = "html"),
    "~/repos/citsci/out/sumstats.html")
