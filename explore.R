setwd("~/data/citsci")

library("data.table")
library("stringi")
library("anytime")
library("ggplot2")
library("readxl")
library("texreg")
library("alluvial")
library("RColorBrewer")

source("~/repos/citsci/functions.R")

# project-level info
lijst = readxl::read_xlsx("~/repos/citsci/Data1_projecten_9.xlsx",
    sheet = "Complete lijst")
setDT(lijst)

# forum data
users_projects <- fread("20200210_portal_gebruikers_projecten.csv")
users_projects <- users_projects[project_id != "project_id"]

messages <- fread("20200210_portal_messages.csv")
messages <- messages[project_id != "project_id"]

# merge project roles into the messages
# seems to work, but could do with a final check whether the user ids of projects and messages are the same (maybe check using jiw where we can know this 100%)
messages = merge(messages, 
    users_projects[, .(project_id, user_id = gebruiker_id, rol)],
    by = c("project_id", "user_id"), all.x = TRUE, all.y = FALSE)
messages[, created_at := as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%OS")]

# entry data
ctrl = readRDS("allctrl.rds.gz")
idxr = readRDS("allidxr.rds.gz")

# remove headers inserted as data in 1st 5k rows
idxr = lapply(idxr, function(x) x[gebruiker_id != "gebruiker_id"])
ctrl = lapply(ctrl, function(x) x[gebruiker_id != "gebruiker_id"])

# entered separately and not yet processed, so select here
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
lijst[`Project ID` %in% c(208, 37, 22), Naam]
lijst[`Project ID` %in% c(236, 45), Naam]
group1 = c(
    `22` = "wfg_index",
    # `22` = "wfg_transcription_notarieel",
    `37` = "bhic_strafgevangenissen",
    `208` = "bhic_strafgevangenissen_2", 
    "ja_ik_wil")
group2 = c(
    `236` = "utr_index",
    `236` = "utr_index_second",
    `45` = "utr_index_bvr")

idxr[, group := "other"]
idxr[project %in% group1, group := paste0("g1_", project)]
idxr[project %in% group2, group := paste0("g2_", project)]

# did projects do double entry?
# nb: some entered > 2
idxr[, .N, by = .(project, scan_id)][, .(entered = .N), by = N][order(entered)]
ctrl[, .N, by = .(project, scan_id)][, .(entered = .N), by = N][order(entered)]

# create new user variable
idxr[order(aangemaakt_op), newuser := !duplicated(gebruiker_id)]
ctrl[order(aangemaakt_op), newuser := !duplicated(gebruiker_id)]

# active user defined as "has contributed in past 6 months"
half_year = (365.25 / 2) * 24 * 60 * 60 # in seconds
idxr[order(aangemaakt_op), activeuser := aangemaakt_op - shift(aangemaakt_op) < half_year, by = gebruiker_id]
ctrl[order(aangemaakt_op), activeuser := aangemaakt_op - shift(aangemaakt_op) < half_year, by = gebruiker_id]
idxr[is.na(activeuser), activeuser := FALSE] # is this right?
ctrl[is.na(activeuser), activeuser := FALSE] # is this right?

# calculate delays to controle
delays = merge(
    idxr[, list(
            date_entered = last(aangemaakt_op),
            group = group[1]), 
        by = list(project, scan_id)], 
    ctrl[, list(date_checked = last(aangemaakt_op)), 
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

# proposition 4 #
# ------------- # 
# forum activity and project speed

proj_speed = idxr[, 
    list(scans_per_week = .N), 
    by = list(project, week = week(aangemaakt_op), year = year(aangemaakt_op))]
proj_speed = lijst[, .(project_id = `Project ID`, project = Naam)][proj_speed, on = "project"]
proj_speed[is.na(project_id), unique(project)]
proj_speed[!is.na(project_id), unique(project)]
proj_speed = proj_speed[!is.na(project_id)]

messages[, project_id := as.numeric(project_id)]

messages[, uniqueN(user_id), by = rol]
messages[, .N, by = rol]

forum_activity = messages[, 
    list(messages_per_week = .N,
        volunteer_messages_per_week = sum(rol == "invoerder", na.rm = TRUE),
        non_volunteer_messages_per_week = sum(rol != "invoerder", na.rm = TRUE)), 
    by = list(project_id, week = week(created_at), year = year(created_at))]
# nb: NAs are omitted

pdf("~/repos/citsci/out/forumposts.pdf")
plot(toplot[order(year, week), sum(messages_per_week), by = list(year + week / 52)], type = 'l', col = 2, lwd = 1.5,
        ylab = "posts per week")
dev.off()

toplot = merge(proj_speed, forum_activity, by = c("project_id", "year", "week"))
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
par(mfrow = c(1, 2))
plot(log(total_scans) ~ log(total_messages), data = toplot_proj,
    main = "All messages", xlab = "log(messages)", ylab = "log(scans)")
m1 = lm(log(total_scans) ~ log(total_messages), data = toplot_proj)
abline(m1, col = 2, lwd = 1.5)
plot(log(total_scans) ~ log(nonvol_total_messages), data = toplot_proj[nonvol_total_messages > 0],
    main = "Only non-volunteer messages", xlab = "log(messages)", ylab = "log(scans)")
m2 = lm(log(total_scans) ~ log(nonvol_total_messages), data = toplot_proj[nonvol_total_messages > 0])
abline(m2, col = 2, lwd = 1.5)
dev.off()

pdf("~/repos/citsci/out/posts_scans_weekly.pdf", width = 9, height = 5)
par(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(messages_per_week), data = toplot,
        main = "All messages", xlab = "log(messages)", ylab = "log(scans)")
m3 = lm(log(scans_per_week) ~ log(messages_per_week), data = toplot)
abline(m3, col = 2, lwd = 1.5)
plot(log(scans_per_week) ~ log(non_volunteer_messages_per_week), data = toplot,
    main = "Only non-volunteer messages", xlab = "log(messages)", ylab = "log(scans)")
m4 = lm(log(scans_per_week) ~ log(non_volunteer_messages_per_week), data = toplot[non_volunteer_messages_per_week > 0])
abline(m4, col = 2, lwd = 1.5)
dev.off()

texreg(list(m1, m2, m3, m4), file = "~/repos/citsci/out/correlations.tex")

pdf("~/repos/citsci/out/posts_scans_weekly_lagged.pdf", width = 9, height = 5)
par(mfrow = c(1, 2))
plot(log(scans_per_week) ~ log(messages_last_week), data = toplot)
m5 = lm(log(scans_per_week) ~ log(messages_last_week), data = toplot)
abline(m5, col = 2, lwd = 1.5)
plot(log(scans_per_week) ~ log(non_volunteer_messages_last_week), data = toplot[non_volunteer_messages_last_week > 0])
m6 = lm(log(scans_per_week) ~ log(non_volunteer_messages_last_week), data = toplot[non_volunteer_messages_last_week > 0])
abline(m6, col = 2, lwd = 1.5)
dev.off()

# proposition 1 #
# ------------- #
# share new volunteers by project
new_volunteers = idxr[,  
    list(
        start = min(aangemaakt_op),
        ratio_new_volunteers = ratio_new_volunteers(.SD),
        nvolunteers = uniqueN(gebruiker_id),
        nscans = .N),
    by = list(project)]

# distribution of shares new volunteers
pdf("~/repos/citsci/out/new_volunteers_byproject.pdf")
hist(new_volunteers$ratio_new_volunteers, xlab = "Share new volunteers", main = "")
dev.off()
cat(mean(new_volunteers$ratio_new_volunteers), file = "~/repos/citsci/out/mean_new_volunteers_byproject.txt")
new_volunteers[, weighted.mean(ratio_new_volunteers, nvolunteers)]
new_volunteers[year(start) > 2011, weighted.mean(ratio_new_volunteers, nvolunteers)]
new_volunteers[, weighted.mean(ratio_new_volunteers, nscans)]

# high share projects that are not small
new_volunteers[ratio_new_volunteers > 0.8 & nvolunteers > 10 & year(start) > 2012, ]

# share new volunteers by project, over time
new_volunteers = idxr[,
    list(ratio_new_volunteers(.SD)),
    by = list(project, quarter = zoo::as.yearqtr(aangemaakt_op))]

# alternatively, directly avereage by quarter and skip the project step, but
# this is closer to approach above

toplot = new_volunteers[order(quarter), mean(V1), by = quarter][as.numeric(quarter) >= 2012]
pdf("~/repos/citsci/out/newvolunteers_overtime.pdf")
plot(toplot, type = "b", col = "red")
dev.off()

# pool of active/new volunteers at any time
# since newuser has activeuser = false, is the -new really necessary? 

active_userpool = idxr[, 
    list(active = uniqueN(gebruiker_id[activeuser == TRUE]), 
         new = uniqueN(gebruiker_id[newuser == TRUE])), 
    by = list(mth = zoo::as.yearmon(aangemaakt_op))]
pdf("~/repos/citsci/out/activepool.pdf")
plot(active_userpool[order(mth), list(mth, active - new)], 
    type = 'b', col = "red",
    ylab = "N")
dev.off()
plot(active_userpool[order(mth), list(mth, diff(active - new))], 
    type = 'b', col = "red",
    ylab = "N", main = "Growth of pool of pre-existing volunteers")
abline(h = 0, col = "gray")
active_userpool[order(mth), mean(diff(active - new))]

# proposition 2 #
# ------------- #
names(lijst)
fwrite(lijst[, .N, by = Klantnaam][order(-N)][1:10],
    "~/repos/citsci/out/organisation.csv")
lijst[Klantnaam == "Tresoar"]
lijst[Klantnaam == "AlleFriezen"]
lijst[Klantnaam == "Fryske Akademy"]

# in idxr but not in lijst
setdiff(unique(idxr$project), lijst$Naam)
lijst[grep("gaarder", Naam)]

# in lijst but not in idxr
setdiff(lijst$Naam, unique(idxr$project))

# ignore for now and merge
idxr = lijst[, list(Naam, Klantnaam)][idxr, on = c(Naam = "project")]
setnames(idxr, "Naam", "project")
setnames(idxr, "Klantnaam", "org")

# calculate the expected number based on random project draw by permuting projects first
x = idxr[!is.na(org), list(aangemaakt_op = min(aangemaakt_op)), by = list(project, org, gebruiker_id)]
x = x[order(gebruiker_id, aangemaakt_op)]
set.seed(7/4)
x[, placeboorg := sample(org)] # this accounts for the number of volunteers, but not the number of scans
x = x[, list(
        nproj = .N, 
        same_org = mean(duplicated(org)),
        same_placebo_org = mean(duplicated(placeboorg), na.rm = TRUE),
        same_org_direct = mean(org == shift(org, fill = FALSE), na.rm = TRUE),
        same_placebo_org_direct = mean(placeboorg == shift(placeboorg, fill = FALSE), na.rm = TRUE),
        org_mix = uniqueN(org) / .N, 
        placebo_org_mix = uniqueN(org) / .N), 
    by = gebruiker_id]

mean(x[, same_org])
mean(x[, same_placebo_org])
mean(x[nproj > 1, same_org])
mean(x[nproj > 1, same_placebo_org])

pdf("~/repos/citsci/out/returnvolunteers.pdf")
plot(density(x[nproj > 1, same_placebo_org]))
lines(density(x[nproj > 1, same_org]), col = "red")
legend("topright", fill = 1:2, legend = c("placebo", "actual returns"))
dev.off()

red = "#DF536BAA"
green = "#61D04FAA"
blue = "#2297E6AA"
hist(x[nproj > 1, same_placebo_org], col = blue, xlim = c(0, 1), breaks = 20)
hist(x[nproj > 1, same_org], col = red, add = TRUE, breaks = 20)

toplot = x[, list(
        sameorg_sd = sd(same_org), 
        sameorg_mean = mean(same_org), 
        same_placebo_org_sd = sd(same_placebo_org), 
        same_placebo_org_mean = mean(same_placebo_org)), 
    by = nproj]
plot(sameorg_mean ~ nproj, data = toplot[nproj < 20][order(nproj)], type = 'l', lwd = 1.5, col = "red")
lines(same_placebo_org_mean ~ nproj, data = toplot[nproj < 20][order(nproj)], type = 'l', lwd = 1.5, col = "red")

# alternative: is exactly next one same?
mean(x[nproj > 1, same_org_direct])
mean(x[nproj > 1, same_placebo_org_direct])

pdf("~/repos/citsci/out/directreturnvolunteers.pdf")
plot(density(x[nproj > 1, same_placebo_org_direct]))
lines(density(x[nproj > 1, same_org_direct]), col = "red")
legend("topright", fill = 1:2, legend = c("placebo", "direct actual returns"))
dev.off()

idxr[, largeorg := ifelse(org %in% c("Het Utrechts Archief",
        "Brabants Historisch Informatie Centrum",
        "Stadsarchief Amsterdam",
        "Westfries Archief",
        "Archief Eemland",
        "Regionaal Archief Nijmegen",
        "Tresoar"),
    org, "other")]
x = idxr[!is.na(org), list(aangemaakt_op = min(aangemaakt_op)), by = list(project, largeorg, gebruiker_id)]
x[order(gebruiker_id, aangemaakt_op)]
x = x[, list(from = largeorg, to = shift(largeorg)), by = gebruiker_id]
x = x[!is.na(to), .N, by = list(from, to)]
# x = x[!is.na(V2)]
x[, col := RColorBrewer::brewer.pal(8, "Set3")[as.factor(from)]]
x = x[order(-N)]
x[, from := factor(from, levels = unique(from))]
x[, to := factor(to, levels = unique(to))]

pdf("~/repos/citsci/out/flows.pdf", width = 9)
alluvial(x[, 1:2], freq = x$N, col = x$col, cw = 0.2, cex = 0.8)
dev.off()

# more alternative: what's the mix of orgs per user
hist(x$org_mix)
toplot = x[, mean(org_mix, na.rm = TRUE), by = nproj]
plot(toplot[order(nproj)], type = 'b')
# ok can you do this the other way round? what do we join next?

# woonplaats vrijwilligers
# invoerduur
# hoeveel projecten zijn voltooid?

# and now can we expand this to the type of material/task?
names(lijst)
lijst[, .N, by = `Soort materiaal`][order(N)]
lijst[, .N, by = `Soort materiaal gecategoriseerd (tekstueel, foto, kaarten)`][order(N)]
lijst[, .N, by = `Soort taak`][order(N)]
lijst[, .N, by = `Omschrijving taak`][order(N)]
lijst[, .N, by = `Locatie specifiek ?`][order(N)]
lijst[, .N, by = `Locatie`][order(N)]
lijst[, .N, by = `Periode specifiek?`][order(N)]
lijst[, .N, by = `Periode`][order(N)]

# old stuff, to be deleted
# -----------------------

# that "move" chart, going either "same org" "same kind" "something else"

par(mfrow = c(1, 2))
# by project then month
# omit first month because of volatility
# by month directly
toplot = idxr[, ratio_new_volunteers(.SD), by = list(mnth = zoo::as.yearmon(aangemaakt_op))]
plot(toplot[mnth >= 2012][order(mnth)], type = 'b')

toplot = new_volunteers[, list(V1, mth = order(mnth)), by = list(project)][order(mth), list(avg = mean(V1), sd = sd(V1)), by = list(mth = mth)]
matplot(toplot$mth, toplot[, .(avg, avg + sd, avg - sd)], type = 'b')

x = idxr[, list(
    min(aangemaakt_op), 
    sum(newuser) / uniqueN(gebruiker_id)), 
    by = list(project, year(aangemaakt_op),month(aangemaakt_op))][order(V1)]
plot(V2 ~ V1, data = x)
abline(lm(V2 ~ V1, data = x))
plot(V2 ~ V1, data = x)
p = loess(V2 ~ as.numeric(V1), data = x)
lines(p$x, p$fitted, col = 2)
toplot = x[, list(min(V1), mean(V2)), by = list(year(V1), quarter(V1))]
plot(V2 ~ V1, data = toplot[V2 < 1], type = 'b', col = 'red')

# scans for entire platform
pdf("~/repos/citsci/scans_platform.pdf")
toplot = idxr[, 
    list(N = .N, monthdate = min(aangemaakt_op)), 
    by = list(year(aangemaakt_op), month(aangemaakt_op))]
plot(toplot[order(monthdate), .(monthdate, N)], type = 'l', col = "red")
dev.off()

pdf("~/repos/citsci/scans_project.pdf")
toplot = idxr[,
    list(scans = .N, projects = uniqueN(project), monthdate = as.Date(min(aangemaakt_op))),
    by = list(year(aangemaakt_op), month(aangemaakt_op))
    ][, list(scans / projects, monthdate)]
plot(toplot[order(monthdate), .(monthdate, V1)], log = 'y', type = 'l', col = "red")
dev.off()

# what is that outlier in 2015-05-31
toplot = idxr[year(aangemaakt_op) == 2015 & between(month(aangemaakt_op), 4, 8)][, .N, by = list(project, month(aangemaakt_op))][order(N)]
ggplot(toplot, aes(month, N, col = project)) + 
    geom_line() + geom_point() + theme_classic()

# active users per month
pdf("~/repos/citsci/users_platform.pdf")
toplot = idxr[, 
    list(N = uniqueN(gebruiker_id), monthdate = min(aangemaakt_op)), 
    by = list(year(aangemaakt_op), month(aangemaakt_op))]
plot(toplot[order(monthdate), .(monthdate, N)], type = 'l', col = "red")
dev.off()

# users per project
pdf("~/repos/citsci/users_project.pdf")
toplot = idxr[,
    list(volunteers = uniqueN(gebruiker_id), projects = uniqueN(project), monthdate = as.Date(min(aangemaakt_op))),
    by = list(year(aangemaakt_op), month(aangemaakt_op))
        ][, list(volunteers / projects, monthdate)]
plot(toplot[order(monthdate), .(monthdate, V1)], log = 'y', type = 'l', col = "red")
dev.off()

## user turnover, mean projects per user
pdf("~/repos/citsci/projects_per_user.pdf")
toplot = idxr[,
    list(uniqueN(project), monthdate = as.Date(min(aangemaakt_op))), by = list(gebruiker_id, year(aangemaakt_op), month(aangemaakt_op))
    ][, list(mean(V1), monthdate = as.Date(min(monthdate))),
        by = list(year, month)]
plot(toplot[order(monthdate), .(monthdate, V1)], log = 'y', type = 'l', col = "red")
dev.off()


pdf("~/repos/citsci/turnover.pdf")
idxr[order(aangemaakt_op), new_user_in_project := !duplicated(gebruiker_id), by = project]
toplot = idxr[, list(sum(new_user_in_project) / uniqueN(gebruiker_id), monthdate = min(aangemaakt_op)), by = list(year(aangemaakt_op), month(aangemaakt_op))]
plot(toplot[order(monthdate), .(monthdate, V1)], log = 'y', type = 'l', col = "red",
    ylab = "New volunteers to project as % active volunteers")
dev.off()

# number of active projects per month
pdf("~/repos/citsci/projects_platform.pdf")
toplot = idxr[, 
    list(N = uniqueN(project), monthdate = min(aangemaakt_op)), 
    by = list(year(aangemaakt_op), month(aangemaakt_op))]
plot(toplot[order(monthdate), .(monthdate, N)], type = 'l', col = "red")
dev.off()


# overall dataset
sumstats = idxr[, list(.N, sum(N), uniqueN(project), uniqueN(gebruiker_id))]

fwrite(idxr[project == "bhic"][1:5],
    "~/repos/citsci/examplerows.csv")

fwrite(data.table(group = 1, no = names(group1), name = group1), 
    "~/repos/citsci/group1.csv")
fwrite(data.table(group = 2, no = names(group2), name = group2), 
    "~/repos/citsci/group2.csv")

# basic sumstats
sumstats = idxr[, list(
        start = as.Date(min(aangemaakt_op)),
        end = as.Date(max(aangemaakt_op)),
        scans = .N,
        entries = sum(N)),
    by = list(group, project)]
out = sumstats[, lapply(.SD, mean), by = group, .SDcols = -"project"]
fwrite(out, "~/repos/citsci/sumstats.csv")

# jiw sumstats
sumstats = list(
    project_appeal = idxr[, project_appeal(.SD), by = group],
    activity_ratio = idxr[, activity_ratio(.SD), by = group],
    variation_periodicity = idxr[, variation_periodicity(.SD), by = group],
    distribution_effort = idxr[, distribution_effort(.SD), by = group],
    public_contribution = idxr[, public_contribution(.SD), by = group],
    sustained_engagement = idxr[, sustained_engagement(.SD), by = group],
    ratio_new_volunteers = idxr[, ratio_new_volunteers(.SD), by = group],
    ratio_platform_members = idxr[, ratio_platform_members(.SD), by = group],
    feedback_delay = delays[, feedback_delay(.SD), by = group]
)

for (nm in names(sumstats)){
    pdf(paste0("~/repos/citsci/", nm, ".pdf"), height = 6)
    dotchart(
        sumstats[[nm]][[2]], 
        sumstats[[nm]][[1]],
        pch = 19)
    dev.off()
}

out = rbindlist(sumstats, use.names = FALSE, id = "measure")
out[, V1 := signif(V1, 3)]
fwrite(
    dcast(out[!is.na(group)], group ~ measure),
    "~/repos/citsci/jiwmeasures.csv")

Reduce(function(...) merge(..., by = "group"), sumstats)

sumstats = idxr[, list(
            # scans = .N,
            # entries = sum(N),
            activity = uniqueN(aangemaakt_op) / as.numeric(diff(range(aangemaakt_op))), # not exactly as defined in prev paper
            volunt = uniqueN(gebruiker_id),
            newvolunt = sum(newuser),
            appeal = uniqueN(gebruiker_id) / as.numeric(diff(range(aangemaakt_op)))
    ),
    by = list(group, project)]
out = sumstats[, lapply(.SD, mean), by = group, .SDcols = -"project"]
fwrite(out, "~/repos/citsci/jiwmeasures.csv")

# monthly activity
toplot = idxr[
    !project %in% c("amsterdam_pensioen", "militieregisters"),
    .(N = .N, monthdate = min(as.Date(aangemaakt_op))), 
    by = list(group, project, month(aangemaakt_op), year(aangemaakt_op))]
toadd = toplot[group == "other", .(group = "rest", N = mean(N), monthdate = min(monthdate)), by = list(year(monthdate), quarter(monthdate))]
toplot = rbindlist(list(toplot, toadd), fill = TRUE)

out = ggplot(toplot[group != "other"], aes(monthdate, N)) + 
    geom_line(data = toplot[group == "other", -"group"], aes(monthdate, N, group = project), col = "lightgray") + 
    geom_line(col = "red") + 
    facet_wrap(~ group, nrow = 2) + 
    theme_classic()
ggsave("~/repos/citsci/activity.pdf", plot = out, width = 10, height = 5)

# new users
toplot = idxr[
    !project %in% c("amsterdam_pensioen", "militieregisters"),
    .(newuser = sum(newuser), monthdate = min(as.Date(aangemaakt_op))), 
    by = list(group, project, month(aangemaakt_op), year(aangemaakt_op))]
toadd = toplot[group == "other", .(group = "rest", newuser = mean(newuser), monthdate = min(monthdate)), by = list(year(monthdate), quarter(monthdate))]
toplot = rbindlist(list(toplot, toadd), fill = TRUE)
out = ggplot(toplot[group != "other"], aes(monthdate, newuser)) + 
    geom_line(data = toplot[group == "other", -"group"], aes(monthdate, newuser, group = project), col = "lightgray") + 
    geom_line(col = "red") + 
    facet_wrap(~ group, nrow = 2) + 
    theme_classic()
ggsave("~/repos/citsci/newusers.pdf", plot = out, width = 10, height = 5)



sumstats = merge(sumstats,
    delays[, list(delay = mean(delay, na.rm = TRUE),
            share_checked = first(share_checked)), by = project], 
    by = "project")
distr = idxr[, uniqueN(scan_id), by = .(project, gebruiker_id)][, .(distr = ineq::ineq(V1)), by = project]

sumstats = merge(sumstats, distr, by = "project")



slopes = idxr[, .N, by = .(project, month = zoo::as.yearmon(aangemaakt_op)) ][, 
    lm(log(N) ~ month, data = .SD)$coef["month"], by = project]

sumstats = merge(sumstats, slopes, by = "project")

sumstats = merge(sumstats, lijst,
    by.x = "project", by.y = "Naam",
    all.x = TRUE)

delays_week = delays[order(V1_ctrl, V1_idxr), 
    list(delay = mean(delay, na.rm = TRUE), 
        share_checked = mean(!is.na(V1_ctrl)),
        .N,
         weekdate = min(V1_ctrl)), 
    by = list(project, year(V1_ctrl), week(V1_ctrl))]
delays_week[is.nan(delay), delay := NA]

pdf("~/repos/citsci/graphs.pdf")
plot(log(volunteers) ~ log(scans), data = sumstats, 
    main = "Project size and volunteers")
abline(lm(log(volunteers) ~ log(scans), data = sumstats), col = 2)

plot(log(activity) ~ log(scans), data = sumstats,
    main = "Project size and daily activity", ylab = "log(daily scans)")
abline(lm(log(activity) ~ log(scans), data = sumstats), col = 2)

plot(log(newvolunteers) ~ log(scans), data = sumstats,
    main = "Project size and new volunteers")
abline(lm(log(newvolunteers) ~ log(scans), data = sumstats[newvolunteers > 0]), col = 2)

plot(distr ~ log(scans), data = sumstats,
    main = "Project size and contribution inequality")
abline(lm(distr ~ log(scans), data = sumstats), col = 2)

toplot = idxr[, .(.N, projecten = uniqueN(project)), by = gebruiker_id][order(N)]
plot(toplot[, .(order(-.I), N)], 
    log = 'xy', xlab = "rank", ylab = 'N scans',
    main = "Rank-Input")

plot(log(activity) ~ log(as.numeric(delay)), data = sumstats[share_checked > 0.8 & delay > 1],
    main = "Activity and delay")
abline(lm(log(activity) ~ log(as.numeric(delay)), data = sumstats[share_checked > 0.8 & delay > 1]), col = 2)

plot(log(N) ~ log(as.numeric(delay)), data = delays_week,
    main = "Activity and delay (weekly)")
abline(lm(log(N) ~ log(as.numeric(delay)), data = delays_week[delay > 1e-4]), col = 2)
abline(v = log(1))

ggplot(sumstats[, mean(activity, na.rm = TRUE), by = `Soort taak`][order(V1)], aes(V1, `Soort taak`)) + 
    geom_point() +
    xlab("Activity") + 
    theme_classic()

ggplot(sumstats[, mean(activity, na.rm = TRUE), by = `Coupons`][order(V1)], aes(V1, `Coupons`)) + 
    geom_point() +
    xlab("Activity") + 
    theme_classic()

dev.off()

idxr[, .(sum(newuser), sum(newuser) / uniqueN(gebruiker_id)), by = project][order(-V1)][1:10]
toplot
screenreg(lm(log(N) ~ log(as.numeric(delay)), data = delays_week[delay > 1e-4]))



plot(log(scans) ~ log(as.numeric(delay)), data = sumstats[share_checked > 0.8 & delay > 1])
abline(lm(log(scans) ~ log(as.numeric(delay)), data = sumstats[share_checked > 0.8 & delay > 1]), col = 2)
summary(lm(log(scans) ~ log(as.numeric(delay)), data = sumstats[share_checked > 0.8 & delay > 1]))


slopes[order(V1)]
plot(idxr[project == "raa", .N, by = as.yearmon(aangemaakt_op)])
hist(slopes$V1)

idxr[project]

# user uptake, of which new/old
# some view of user movements?

plot(rbindlist(list(idxr, ctrl), use.names = TRUE)[, .N, by = zoo::as.yearmon(aangemaakt_op)])
plot(rbindlist(list(idxr), use.names = TRUE)[, .N, by = list(ym = zoo::as.yearmon(aangemaakt_op))][order(ym)], type = 'b')

ggplot(idxr[, .N, by = list(project, month = zoo::as.yearmon(aangemaakt_op))], aes(month, log(N))) + 
    geom_smooth(aes(group = project), method = "lm", se = FALSE) + 
    theme_classic() + 
    theme(legend.position = "none")

merge(idxr, ctrl, 
    by = c("project", "scan_id"),
    all.x = TRUE, all.y = TRUE)