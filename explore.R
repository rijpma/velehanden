setwd("~/data/citsci")

library("data.table")
library("zoo")
library("stringi")
library("anytime")
library("readxl")
library("texreg")
library("alluvial")
library("RColorBrewer")
library("lmtest")
library("knitr")
library("sandwich")

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


# drop because not in projecten and idxr
lijst = lijst[!Naam %in% c("frl_bso_index", "unesco_tagging_photos")]

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
names(idxr) = stringi::stri_replace_all_regex(names(idxr), "_indexeer_data\\.csv", "")
names(idxr) = stringi::stri_replace_all_regex(names(idxr), "^\\d+_", "")

names(ctrl) = stringi::stri_replace_all_regex(names(ctrl), "_controle_data\\.csv", "")
names(ctrl) = stringi::stri_replace_all_regex(names(ctrl), "^\\d+_", "")

# list to dataset
idxr = rbindlist(idxr, use.names = TRUE, idcol = "project")
ctrl = rbindlist(ctrl, use.names = TRUE, idcol = "project")

# collapse entries to scan keeping N
idxr = idxr[, .N, , by = list(project, scan_id, gebruiker_id, aangemaakt_op)]
ctrl = ctrl[, .N, , by = list(project, scan_id, gebruiker_id, aangemaakt_op)]

# actual dates/times
idxr[, aangemaakt_op := anytime::anytime(aangemaakt_op)]
ctrl[, aangemaakt_op := anytime::anytime(aangemaakt_op)]


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

# merge org names into idxr
setdiff(lijst$Naam, projecten$naam)
setdiff(lijst$Naam, idxr$project)
idxr = lijst[, list(Naam, Klantnaam)][idxr, on = c(Naam = "project")]
setnames(idxr, "Naam", "project")
setnames(idxr, "Klantnaam", "org")

# merge project types into idxr
idxr = projecten[, list(naam, status, project_soort)][idxr, on = c(naam = "project")]
setnames(idxr, "naam", "project")

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

# new user variable
setorder(idxr, aangemaakt_op)
setorder(ctrl, aangemaakt_op)
idxr[, newuser := !duplicated(gebruiker_id)]
ctrl[, newuser := !duplicated(gebruiker_id)]

# new variables
# variable highlighting new users throughout their first project
idxr[, new4project := any(newuser), by = list(gebruiker_id, project)]

# new user shows up in project
idxr[, newinproject := !duplicated(gebruiker_id), by = project]

# experience measure (N projects )
idxr[, experience := 1:.N, by = gebruiker_id]

setorder(idxr, project, scan_id, aangemaakt_op)
setorder(ctrl, project, scan_id, aangemaakt_op)

# active user defined as "has contributed in past 6 months"
half_year = (365.25 / 2) * 24 * 60 * 60 # in seconds
idxr[order(aangemaakt_op), activeuser := aangemaakt_op - shift(aangemaakt_op) < half_year, by = gebruiker_id]
ctrl[order(aangemaakt_op), activeuser := aangemaakt_op - shift(aangemaakt_op) < half_year, by = gebruiker_id]
idxr[is.na(activeuser), activeuser := FALSE] # is this correct?
ctrl[is.na(activeuser), activeuser := FALSE]

# preliminaries #
# ------------- #
# Figure 1
# basic development of platform
pdf("~/repos/citsci/out/fig_1_platformoverall.pdf", height = 9)

mypar(mfrow = c(3, 2))

toplot = idxr[, list(scans = sum(N)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, 
    main = "Scans",
    ylab = "Number of scans", xlab = "Year")

toplot = idxr[, list(projects = uniqueN(project)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, 
    main = "Active projects",
    ylab = "Number of projects", xlab = "Year")

# one of these two should not have "scans" as the y-axis
toplot = idxr[, list(new_volunteers = sum(newuser)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, 
    main = "New volunteers",
    ylab = "Number of new volunteers", xlab = "Year")
toplot[order(new_volunteers)]
idxr[, list(new_volunteers = sum(newuser)), by = list(month = zoo::as.yearmon(aangemaakt_op), project)][order(new_volunteers)]

projecten[order(aangemaakt_op), list(aangemaakt_op, titel)][aangemaakt_op < "2011-11-31"]
projecten[order(aangemaakt_op), list(aangemaakt_op, titel)][aangemaakt_op < "2017-06-31"]

toplot = idxr[, list(active_volunteers = uniqueN(gebruiker_id)), by = list(month = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, 
    main = "Active volunteers",
    ylab = "Number of active volunteers", xlab = "Year")

toplot = messages[!is.na(created_at), list(posts = .N), by = list(month = zoo::as.yearmon(created_at))]
plot(toplot[order(month)], type = 'b',pch = 20, col = 2, 
    main = "Forum messages",
    ylab = "Number of posts", xlab = "Year")
toplot[order(posts)]

dev.off()


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
new_volunteers[, ratio_pre_existing_volunteers := 1 - ratio_new_volunteers]
new_volunteers[, percent_pre_existing_volunteers := ratio_pre_existing_volunteers * 100]

# figure 2
# distribution of shares new volunteers
pdf("~/repos/citsci/out/fig_2_old_volunteers_byproject.pdf")
mypar()
toplot = hist(new_volunteers$percent_pre_existing_volunteers, plot = FALSE)
plot(toplot,
    col = "lightgray",
    xlab = "Percent pre-existing volunteers in project", 
    ylab = "Number of projects",
    xaxt = "n",
    ylim = range(pretty(toplot$counts)),
    main = "")
axis(side = 1, at = axTicks(side = 1), labels = paste0(axTicks(side = 1), "%"))
dev.off()

# overall averages
new_volunteers[year(start) > 2011, weighted.mean(ratio_pre_existing_volunteers, nvolunteers)]

# list high share projects that are not small
new_volunteers[ratio_new_volunteers > 0.8 & nvolunteers > 10 & year(start) > 2012, ]


# figure 3
# share output by new volunteers
toplot = idxr[, 
    list(perc_by_new = mean(new4project) * 100), 
    by = list(ym = zoo::as.yearmon(aangemaakt_op))]
pdf("~/repos/citsci/out/fig_3_sharebynew.pdf")
mypar()
plot(toplot[order(ym)],
    type = 'b', col = 2, lwd = 1.5, pch = 20,
    xlab = "Year",
    ylab = "Percent entries",
    yaxt = "n")
axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
dev.off()

# figure 4: 
# experience and activity
# user level
expact = idxr[, 
    list(
        activity = .N / as.numeric(diff(range(as.Date(aangemaakt_op))) + 1),
        experience = experience[newinproject == TRUE]), # -1 but then no log :( 
    by = list(project, gebruiker_id)]

out = ggplot(expact, aes(log(experience), log(activity))) + geom_point() + geom_smooth() + theme_classic()

# but of course, this does not necessarily mean that there is no project-wide effect, because we know contributions can be very lopsided
# so once more, but to the project level
expact = idxr[year(aangemaakt_op) > 2011, 
    list(
        start = min(aangemaakt_op),
        perc_new = sum(newuser) / uniqueN(gebruiker_id) * 100,
        activity = .N / as.numeric(diff(range(as.Date(aangemaakt_op))) + 1),
        entries = .N,
        experience = mean(experience[newinproject == TRUE][1:floor(uniqueN(gebruiker_id) / 2)])), 
        # so we look at experience of first 50% of users
        # -1 but then no log :( 
    by = list(project)]
expact[, mean(1 - perc_new)]

pdf("~/repos/citsci/out/fig_4_experience_activity_proj.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log(activity) ~ perc_new, data = expact, 
    pch = 20,
    axes = FALSE,
    xlab = "share new volunteers",
    ylab = "entries per day")
m_share = lm(log(activity) ~ perc_new, data = expact)
abline(m_share, col = 2)
axis(1, at = axTicks(1), labels = paste0(axTicks(1), "%"))
axis(2, at = log(10^(0:5)), labels = 10^(0:5))

plot(log(activity) ~ log(experience), data = expact, 
    pch = 20,
    axes = FALSE,
    xlab = "average volunteer experience (entries)",
    ylab = "entries per day")
m_exp = lm(log(activity) ~ log(experience), data = expact)
abline(m_exp, col = 2)
axis(1, at = log(10^(0:5)), labels = 10^(0:5))
axis(2, at = log(10^(0:5)), labels = 10^(0:5))
dev.off()

m_exp2 = update(m_exp, . ~ . + as.numeric(start))
mlist = list(m_share, m_exp, m_exp2)
cfs = lapply(mlist, lmtest::coeftest, vcov. = sandwich::vcovHC)
ses = lapply(cfs, `[`, i=, j = 2)
pvs = lapply(cfs, `[`, i=, j = 4)
screenreg(mlist, override.se = ses, override.pval = pvs)
htmlreg(mlist, override.se = ses, override.pval = pvs,
    file = "~/repos/citsci/out/fig_4_regression.html")

# proposition 2 #
# ------------- #
# volunteers stay with the same organisation and type of project

# list of largest organisations
fwrite(lijst[, .N, by = Klantnaam][order(-N)][1:10],
    "~/repos/citsci/out/organisation.csv")

# calculate the expected number based on random project draw by permuting projects first
vpo = idxr[!is.na(org), 
    list(aangemaakt_op = min(aangemaakt_op)), 
    by = list(project, org, project_soort, gebruiker_id)]
vpo = vpo[order(gebruiker_id, aangemaakt_op)]
# should we do something about from == 1 not making sense? No, because this is tricky here and the placebo thing should take care of it

# table A1 - example table
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
writeLines(knitr::kable(example, "html"), 
    "~/repos/citsci/out/table_a1_placebo_example.html")

# some sort of inverse weight to add up?
vpo[, invweight := 1 / uniqueN(project), by = org]
# vpo[, invweight := 1 / uniqueN(gebruiker_id), by = org]

set.seed(7/4)
vpo[, placeboorg := sample(org)] # this accounts for the number of volunteers, but not the number of scans
vpo[, placebotype := sample(project_soort)] # this accounts for the number of volunteers, but not the number of scans

# figure 6
vpo = vpo[, list(
        nproj = .N, 
        same_org = mean(duplicated(org)) * 100,
        same_org_wgt = sum(invweight[duplicated(org)]) * 100,
        same_placebo_org = mean(duplicated(placeboorg), na.rm = TRUE) * 100,
        same_org_direct = mean(org == shift(org, fill = FALSE), na.rm = TRUE) * 100,
        same_placebo_org_direct = mean(placeboorg == shift(placeboorg, fill = FALSE), na.rm = TRUE) * 100,
        org_mix = uniqueN(org) / .N, 
        placebo_org_mix = uniqueN(org) / .N,
        # same for project_type
        same_type = mean(duplicated(project_soort)) * 100,
        same_placebo_type = mean(duplicated(placebotype), na.rm = TRUE) * 100,
        same_type_direct = mean(project_soort == shift(project_soort, fill = FALSE), na.rm = TRUE) * 100,
        same_placebo_type_direct = mean(placebotype == shift(placebotype, fill = FALSE), na.rm = TRUE) * 100,
        type_mix = uniqueN(project_soort) / .N, 
        placebo_type_mix = uniqueN(project_soort) / .N), 
    by = gebruiker_id]

vpo[, lapply(.SD, max), .SDcols = patterns("same")]

pdf("~/repos/citsci/out/fig_6_returns.pdf")
mypar()
plot(vpo[, .N, by = same_org - (same_org %% 10)][order(same_org)], 
    main = "Returns to organisation",
    xlab = "Share returns",
    ylab = "N volunteers",
    xaxt = "n",
    lwd = 1.5, log = 'y',, type = 'o', pch = 19)
lines(vpo[, .N, by = same_placebo_org - (same_placebo_org %% 10)][order(same_placebo_org)], 
    col = 2, lwd = 1.5, type = 'o', pch = 19)
axis(1, at = axTicks(1), labels = paste0(axTicks(1), "%"))
text(x = c(40, 60), y = c(60, 800), labels = c("Baseline", "Actual"), col = c(2, 1))
dev.off()

mean(vpo[nproj > 1, same_org])
mean(vpo[nproj > 1, same_placebo_org])
t.test(vpo[nproj > 1, same_org], vpo[nproj > 1, same_placebo_org])

mean(vpo[nproj > 1, same_org_direct])
mean(vpo[nproj > 1, same_placebo_org_direct])
t.test(vpo[nproj > 1, same_org_direct], vpo[nproj > 1, same_placebo_org_direct])

# figure 5 flows between organisations
idxr[, nproj_byorg := uniqueN(project), by = org]
idxr[, largeorg := ifelse(org %in% c("Het Utrechts Archief",
        "Brabants Historisch Informatie Centrum",
        "Stadsarchief Amsterdam",
        "Westfries Archief",
        "Regionaal Archief Nijmegen"),
    org, "other")]

# number of organisations in category "other"
idxr[largeorg == "other", uniqueN(org)]

toplot = idxr[!is.na(org), list(aangemaakt_op = min(aangemaakt_op)), by = list(project, org, largeorg, project_soort, nproj_byorg, gebruiker_id)]
toplot = toplot[order(gebruiker_id, aangemaakt_op)]

# percentage volunteers returning for each organisation
toplot[nproj_byorg > 1, list(org, retention = org == shift(org, type = "lead")), by = gebruiker_id][, mean(retention, na.rm = TRUE), by = org][order(V1)]
toplot[nproj_byorg > 1, list(largeorg, retention = org == shift(org, type = "lead")), by = gebruiker_id][, mean(retention, na.rm = TRUE), by = largeorg]

toplot = toplot[, 
    list(
        from = largeorg, 
        nfrom = nproj_byorg, 
        to = shift(largeorg)), 
    by = gebruiker_id]
toplot = toplot[!is.na(to) & nfrom > 1, .N, by = list(from, to)]
toplot = toplot[order(N)]
toplot[, col := RColorBrewer::brewer.pal(uniqueN(from), "Paired")[as.factor(from)]]
toplot = toplot[order(-N)]
toplot[, from := factor(from, levels = unique(from))]
toplot[, to := factor(to, levels = unique(to))]

pdf("~/repos/citsci/out/fig_5_flows_org.pdf", width = 9)
alluvial(toplot[, 1:2], freq = toplot$N, col = toplot$col, cw = 0.2, cex = 0.8)
dev.off()

nproj = lijst[, .N, by = Klantnaam][order(-N)][1:10]
outtab = dcast(toplot, from ~ to, value.var = "N")
outtab = merge(outtab, nproj, by.x = "from", by.y = "Klantnaam", all.x = TRUE)
setorder(outtab, -N, na.last = TRUE)
outtab = outtab[, c("from", outtab$from, "N"), with = FALSE]
# outtab[, from := paste0(from, " (", N, " projecten)")]
outtab[, N := NULL]
outtab[, (2:7) := round(.SD / rowSums(.SD) * 100), .SDcols = 2:7]
outtab[, (2:7) := lapply(.SD, paste, "%"), .SDcols = 2:7]
writeLines(
    knitr::kable(outtab, format = "html"),
    "~/repos/citsci/out/flowttable.html")

# proposition 3 #
# ------------- # 
# forum activity and project speed

proj_speed = idxr[, 
    list(scans_per_week = .N), 
    by = list(project, week = week(aangemaakt_op), year = year(aangemaakt_op))]
proj_speed = projecten[, .(project_id = id, project = naam)][proj_speed, on = "project"]

# no parent_id means it is the parent
messages[parent_id == "", parent_id := id]
messages[, thread_length := .N, by = parent_id]
# thread_length = 1 is no response and can't be calculated
messages[, nproject := uniqueN(project_id), by = parent_id]
# nproject > 1 happens when project_id = "", but these obviously cannot be linked to a project, so should be dropped
messages[, uniqueN(parent_id[nproject == 2]) / uniqueN(parent_id)]
messages[, quantile(created_at, na.rm = TRUE), by = nproject]

messages[, mean(thread_length == 1)]
response_times = messages[
    thread_length > 1      # should have reply
    & nproject == 1        # should not have multiple or missing projects
    & parent_id != 21674][ # drop projet with missing time
        order(parent_id, created_at), 
        list(
            topic_started = created_at[1],
            first_response = created_at[2]),
        by = list(parent_id, project_id)]
response_times[, response_time := first_response - topic_started]

response_times = projecten[, .(project_id, naam)][response_times, on = "project_id"]
response_times[, group := ifelse(naam %in% ..group1, "group1", "other")]
response_times[, group := ifelse(naam %in% ..group2, "group2", group)]

# figure 7
times = c(hr = 1, dy = 24, wk = 24 * 7)
pdf("~/repos/citsci/out/fig_7_hist_weekly_response_times.pdf")
mypar()
hist(log10(as.numeric(response_times$response_time) / 3600), 
    axes = FALSE,
    breaks = 30,
    xlab = "response time in hours", 
    main = "")
axis(2)
axis(1, at = -4:4, labels = 10^(-4:4))
abline(v = log10(times), lty = 2)
dev.off()

quantile(na.omit(response_times$response_time) / 60, 1:10/10)

# median response time because massive outliers
pdf("~/repos/citsci/out/responsetimes_over_time.pdf")
mypar()
plot(response_times[!is.na(first_response), median(response_time / 3600, na.rm = TRUE), by = zoo::as.yearmon(first_response)][order(zoo)], 
    ylab = "response time (hours)", type = 'b', log = 'y', col = 2, pch = 20)
dev.off()

# before after break (hours)
response_times[!is.na(first_response), median(response_time / 3600, na.rm = TRUE), by = zoo::as.yearmon(first_response) > 2016.6][order(zoo)]
response_times[, mean(response_time / 3600 > times["hr"])]
response_times[, mean(response_time / 3600 > times["dy"])]
response_times[, mean(response_time / 3600 > times["wk"])]

# figure 8, response times and activity
toplot_forum = response_times[, 
    list(mean_response_time = mean(as.numeric(response_time) / 60 / 60, na.rm = TRUE)), 
    by = list(project_id, week = week(first_response), year = year(first_response))]

toplot_forum = proj_speed[toplot_forum, on = c("project_id", "week", "year")]
toplot_forum_proj = toplot_forum[, 
    list(scans = sum(scans_per_week, na.rm = TRUE), 
        mean_response_time = mean(mean_response_time, na.rm = TRUE)),
    by = project_id]
toplot_forum_proj = toplot_forum_proj[scans > 0]

# add delays
toplot_entry = delays[delay > (2 / 84600), # so quick is probably auto-accept
    list(mean_check_time = mean(as.numeric(delay)) * 24, .N),
    by = list(project, week(date_checked), year(date_checked))]
toplot_entry = toplot_entry[mean_check_time > 0.01] # these are real, but not realistic weekly estimates, typically N ~= 1
toplot_entry = proj_speed[toplot_entry, on = c("project", "week", "year")]
toplot_entry_proj = toplot_entry[, list(scans = sum(scans_per_week, na.rm = TRUE), mean_check_time = mean(mean_check_time)),
    by = project]

# figure 8
pdf("~/repos/citsci/out/fig_8_forumdelays_activity.pdf", height = 5, width = 9)
mypar(mfrow = c(1, 2))
plot(log10(scans_per_week) ~ log10(mean_response_time), 
    data = toplot_forum, 
    pch = 20,
    axes = FALSE,
    xlab = "mean response time (hours)",
    ylab = "scans per week",
    main = "Forum responses and activity (volunteer)")
# use log10 to mimic "log=xy" in plot
m1 = lm(log10(scans_per_week) ~ log10(mean_response_time), data = toplot_forum)
abline(m1, col = 2)
axis(1, at = -4:4, labels = 10^(-4:4))
axis(2, at = -4:4, labels = 10^(-4:4))

plot(log10(scans) ~ log10(mean_response_time), 
    data = toplot_forum_proj[,-"project_id"], 
    pch = 20,
    axes = FALSE,
    xlab = "mean response time hours",
    ylab = "scans per week",
    main = "Forum responses and activity (project)")
m2 = lm(log10(scans) ~ log10(mean_response_time), data = toplot_forum_proj[,-"project_id"])
abline(m2, col = 2)
axis(1, at = 0:6, labels = 10^(0:6))
axis(2, at = 0:6, labels = 10^(0:6))
dev.off()


# proposition 4 #
# --------------#
# delay checks are bad
# figure 9
pdf("~/repos/citsci/out/fig_9_checkdelays_activity.pdf", height = 5, width = 9)
mypar(mfrow = c(1, 2))
plot(log10(scans_per_week) ~ log10(mean_check_time), 
    data = toplot_entry, 
    pch = 20,
    axes = FALSE,
    xlab = "mean response time (hours)",
    ylab = "scans per week",
    main = "Entry checks and activity (volunteer)")
m3 = lm(log10(scans_per_week) ~ log10(mean_check_time), data = toplot_entry)
abline(m3, col = 2)
axis(1, at = -2:5, labels = 10^(-2:5))
axis(2, at = -2:5, labels = 10^(-2:5))

# is there an issue here in the sense that if a user does 1000 scans and the project takes a year, than this places some limit on how slow it can be checked
# ie we're estimate N/week = time, so does this introduce an artificial relationship of the kind x 1/x?
plot(log10(scans) ~ log10(mean_check_time), 
    data = toplot_entry_proj, 
    pch = 20,
    axes = FALSE,
    xlab = "mean response time (hours)",
    ylab = "scans per week",
    main = "Entry checks and activity (project)")
m4 = lm(log10(scans) ~ log10(mean_check_time), data = toplot_entry_proj)
abline(m4, col = 2)
axis(1, at = 1:6, labels = 10^(1:6))
axis(2, at = 1:6, labels = 10^(1:6))
dev.off()
# maybe this should not be volunteer weekly but rather volunteer project? But this misses changes within the project

mlist = list(m1, m2, m3, m4)
cfs = lapply(mlist, lmtest::coeftest, vcov. = sandwich::vcovHC)
ses = lapply(cfs, `[`, i=, j = 2)
pvs = lapply(cfs, `[`, i=, j = 4)
screenreg(mlist, override.se = ses, override.pval = pvs)
htmlreg(mlist, override.se = ses, override.pval = pvs,
    file = "~/repos/citsci/out/fig_8_9_regression.html")

# proposition 5 #
# delays and point system
# ----------------#
projecten[, incentive := punten_bij_controle / punten_bij_invoeren]
out = projecten[, .N, by = list(punten_bij_invoeren, punten_bij_controle, incentive)][order(punten_bij_invoeren, punten_bij_controle)]
fwrite(out, "~/repos/citsci/out/punten.csv")

# simplified point system
idxr = projecten[, list(project = naam, project_id, punten_bij_invoeren, punten_bij_controle)][idxr, on = c("project")]

# some checks on the point system
# how many points are spent?
# user 6694 is a sysadmin with 12m points, 0 spent, and no last activity, so we drop
users_projects[gebruiker_id != 6694, sum(spent_points)]
users_projects[gebruiker_id != 6694, sum(spent_points)] / 
    idxr[, sum(punten_bij_invoeren) + sum(punten_bij_controle)]
users_projects[gebruiker_id != 6694, sum(spent_points)] / users_projects[gebruiker_id != 6694, sum(saldo)]
users_projects[gebruiker_id != 6694, sum(spent_points)] / users_projects[gebruiker_id != 6694, sum(saldo + spent_points)]
# not in idxr
# idxr[gebruiker_id == 6694]

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

# figure 10
toplot_entry = projecten[, list(project = naam, project_id, has_coupons, has_coupon_create, has_transactions, punten_bij_invoeren, punten_bij_controle, incentive)][toplot_entry, on = c("project")]
pdf("~/repos/citsci/out/fig_10_delays_activity_bypoints.pdf", width = 9, height = 5)
mypar(mfrow = c(1, 2))
plot(log10(scans_per_week) ~ log10(mean_check_time), 
    data = toplot_entry, type = "n", 
    axes = FALSE,
    xlab = "mean check time (hours)",
    ylab = "scans per week",
    main = "No coupons")
points(log10(scans_per_week) ~ log10(mean_check_time), 
    pch = 20,
    data = toplot_entry[has_coupons == 0])
m1 = lm(log10(scans_per_week) ~ log10(mean_check_time), data = toplot_entry[has_coupons == 0])
abline(m1, col = 2, lwd = 1.5, untf = TRUE)
axis(1, at = -2:5, labels = 10^(-2:5))
axis(2, at = -2:5, labels = 10^(-2:5))

plot(log10(scans_per_week) ~ log10(mean_check_time), 
    data = toplot_entry, type = "n" , 
    axes = FALSE,
    xlab = "mean check time (hours)",
    ylab = "scans per week",
    main = "Coupons")
points(log10(scans_per_week) ~ log10(mean_check_time), 
    data = toplot_entry[has_coupons == 1],
    pch = 20)
m2 = lm(log10(scans_per_week) ~ log10(mean_check_time), data = toplot_entry[has_coupons == 1])
abline(m2, col = 2, lwd = 1.5)
axis(1, at = -2:5, labels = 10^(-2:5))
axis(2, at = -2:5, labels = 10^(-2:5))
dev.off()

mlist = list(m1, m2)
cfs = lapply(mlist, lmtest::coeftest, vcov. = sandwich::vcovHC)
ses = lapply(cfs, `[`, i=, j = 2)
pvs = lapply(cfs, `[`, i=, j = 4)
screenreg(mlist, override.se = ses, override.pval = pvs)
htmlreg(mlist, override.se = ses, override.pval = pvs,
    file = "~/repos/citsci/out/fig_10_regression.html")


# the peaks
# toplot = messages[!is.na(created_at), list(posts = .N), by = list(month = zoo::as.yearmon(created_at), project_id)]
# projecten[id == 242]
# peak forums july 2017 is one project, surinam slavery registers

# toplot = idxr[, list(new_volunteers = sum(newuser)), by = list(month = zoo::as.yearmon(aangemaakt_op), project)]
toplot[, monthtotal := sum(new_volunteers), by = month]
# toplot[order(new_volunteers), list(month, project, new_volunteers, new_volunteers / monthtotal)]
# peak new volunteers in nov 2011 is one project, militieregisters, then the
# only project on platform. other peaks can typically also be linked to a
# single project June 2017 (Slavery registers; 46%), May 2014 (WieWasWie
# bevolkingsregisetrs; 77%), October 2018 (Holland Amerika Lijn
# Passagierslijsten; 67%), May 2018 (Captions for Cas [Oorthuys]; 68%) can
# typically also be linked to a single project 

# summary statistics #
# -------------------#

# date range
range(idxr$aangemaakt_op)

out = list(
    `Number of public projects on VH since 2011, June 20 (start platform):` = 
        projecten[naam != "demo", uniqueN(project_id)],
    `Number of completed projects by 2020, February 11:` = 
        projecten[naam != "demo" & status == "afgerond", uniqueN(project_id)],
    `Number of unique organizations using VH:` = 
        lijst[, uniqueN(Klantnaam)],
    `Number of active volunteers:` = 
        idxr[, uniqueN(gebruiker_id)],
    `Number of items entered:` =
        nrow(idxr) + nrow(ctrl),
    `Unique items entered:` = 
        idxr[, uniqueN(paste0(scan_id, project))]
)
write.csv(do.call(rbind, out),
    "~/repos/citsci/out/size.csv")

sumstatlist = list(
    `N. volunteers` = idxr[, uniqueN(gebruiker_id), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    `N. scans` = idxr[, uniqueN(scan_id), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    `Duration (days)` = idxr[, as.numeric(diff(range(aangemaakt_op), units = "days")), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # `Project appeal` = idxr[, project_appeal(.SD), by = project][,
    #         list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # `Activity ratio` = idxr[, activity_ratio(.SD), by = project][,
    #         list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # # variation_periodicity does not seem to work
    # # omit distribution effort as well?
    # `Distribution of effort` = 
    #     # omit these two project because they only had one contributor (gini = 0)
    #     idxr[!project %in% c("bhic", "picvh_pvdm"), distribution_effort(.SD), by = project][,
    #         list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # `Public contribution` = idxr[, public_contribution(.SD), by = project][,
    #         list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    # sustained_engagement requires dropping 1 contribution volunteers
    `Ratio of new volunteers` = idxr[, ratio_new_volunteers(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    `Ratio of platform members` = idxr[, ratio_platform_members(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    `Feedback delay (days)` = delays[delay > (2 / 84600), feedback_delay(.SD), by = project][,
            list(min = min(V1), median = median(V1), mean = mean(V1), max = max(V1))],
    `N. messages` = messages[, .N, by = project_id][, 
            list(min = min(N), median = median(N), mean =  mean(N), max = max(N))],
    `Forum response time (hours)` = response_times[!is.na(response_time), mean(as.numeric(response_time)), by = project_id][, 
            list(min = min(V1), median = median(V1), mean =  mean(V1), max = max(V1))] / 60 / 60
)

out = rbindlist(sumstatlist, idcol = "stat", use.names = TRUE, fill = TRUE)
writeLines(
    knitr::kable(out, digits = 2, format = "html"),
    "~/repos/citsci/out/sumstats.html")
