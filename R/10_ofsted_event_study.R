# 10_ofsted_event_study.R - Do prices move when the nearest school's Ofsted grade changes?
# ---------------------------------------------------------------------------
# Event study. An "event" is the publication of a graded Ofsted inspection that changes the
# overall effectiveness grade of an Output Area's nearest state primary or secondary (by walking
# time, R/04): an upgrade (e.g. Good to Outstanding, Requires improvement to Good) or a downgrade.
# Each OA takes the first such event published March 2019 to December 2024 (graded inspections
# ended in September 2024), so every event has at least six months of sales before it.
#
# Inspection history: every graded inspection in Ofsted's official statistics as at 31 August
# 2018 to 2023 (latest and previous inspection: publication date and overall effectiveness)
# and the latest OEIF graded inspection in the management information as at 30 September 2026,
# on current URNs via GIAS successor links (as R/05). The grade change is against the school's
# previous graded inspection in that history (or the file's own "previous" grade).
#
# Model (fixest::feols, Sun and Abraham's interaction-weighted estimator via sunab()): log price
# on hedonic controls (as R/06) | OA + district x year-month, with cohort = half-year of the event
# and period = half-year of sale, never-treated OAs (no grade change of their nearest school in
# the window) as controls; standard errors clustered by OA. Run separately for upgrades and
# downgrades, primary and secondary. Reference period: the half-year before publication.
# Inputs: data/sales_panel.parquet (R/05), data/oa_nearest.parquet (R/04), Ofsted and GIAS files in
#   ../school_value_added/data-raw
# Output: data/ofsted_event_study.rds, figures/ofsted_event_study.png    Rscript R/10_ofsted_event_study.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
sva <- normalizePath(file.path(dirname(here::here()), "school_value_added"), winslash = "/")
num <- function(x) suppressWarnings(as.numeric(x))
pdate <- function(x) {   # Ofsted files use dd/mm/yyyy, yyyy-mm-dd and the odd "NULL"
  x <- trimws(as.character(x)); x[x %in% c("", "NULL", "NA")] <- NA
  d <- as.Date(x, format = "%d/%m/%Y"); i <- is.na(d) & !is.na(x); d[i] <- as.Date(x[i], format = "%Y-%m-%d"); as.IDate(d)
}

# ---- inspection history ------------------------------------------------------------------------
L <- fread(file.path(sva, "data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
succ <- unique(L[LinkType == "Successor", .(from = URN, to = LinkURN)]); succ <- succ[!from %in% succ[duplicated(from), from]]
final_of <- function(u) { f <- as.integer(u); for (i in 1:10) { nx <- succ$to[match(f, succ$from)]; f <- ifelse(is.na(nx), f, nx) }; f }
pick <- function(nm, pat) { h <- grep(pat, nm, value = TRUE, ignore.case = TRUE); if (length(h)) h[1] else NA_character_ }
H <- rbindlist(lapply(2018:2023, function(y) {
  x <- fread(file.path(sva, "data-raw", "ofsted", sprintf("ofsted_as_at_31_aug_%d.csv", y)), colClasses = "character", showProgress = FALSE)
  nm <- names(x)
  cur <- x[, .(urn = as.integer(URN), pub = pdate(get("Publication date")), grade = num(get("Overall effectiveness")))]
  pg <- pick(nm, "^Previous.*overall effectiveness$")
  prev <- x[, .(urn = as.integer(URN), pub = pdate(get("Previous publication date")), grade = num(get(pg)))]
  rbind(cur, prev)
}))
mi <- fread(file.path(sva, "data-raw", "ofsted", "Management_information_-_state-funded_schools_-_latest_inspections_as_at_30_Sep_2026.csv"),
            colClasses = "character", showProgress = FALSE)
H <- rbind(H, mi[, .(urn = as.integer(URN), pub = pdate(get("Publication date of latest OEIF graded inspection")),
                     grade = num(get("Latest OEIF overall effectiveness")))])
H <- unique(H[!is.na(pub) & grade %in% 1:4])
H[, urn := final_of(urn)]
H <- unique(H)[order(urn, pub)][, .SD[1], by = .(urn, pub)]
H[, prev := shift(grade), by = urn]
E <- H[!is.na(prev) & grade != prev & pub >= as.IDate("2019-03-01") & pub <= as.IDate("2024-12-31")]
E[, type := fifelse(grade < prev, "Upgrade", "Downgrade")]

# ---- sales, nearest schools, first event per OA ------------------------------------------------
S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lad26cd)]
hy <- function(d) (as.integer(format(d, "%Y")) - 2018L) * 2L + (as.integer(format(d, "%m")) >= 7L)   # half-year index; 2018H2 = 1
S[, period := hy(date)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"

run <- function(phase, kind) {
  u <- sprintf("near_%s_urn", phase)
  first <- E[, .SD[1], by = urn][, .(urn, pub, type, prev, grade)]   # the school's first change in the window
  D <- S[, c("lp", ctrl_vars, "oa21cd", "lad26cd", "ym", "period", u), with = FALSE]
  setnames(D, u, "urn")
  D <- merge(D, first, by = "urn", all.x = TRUE)
  D <- D[is.na(type) | type == kind]
  D[, cohort := fifelse(is.na(pub), 10000L, hy(pub))]
  m <- feols(as.formula(sprintf("lp ~ sunab(cohort, period, ref.p = -1) + %s | oa21cd + lad26cd^ym", ctrl)),
             data = D, cluster = ~oa21cd, lean = TRUE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl("^period::", term)]
  setnames(ct, 2:3, c("b", "se"))
  ct[, rel := as.integer(sub("period::", "", term))]
  att <- summary(m, agg = "att")$coeftable["ATT", 1:2]
  list(es = ct[, .(phase, kind, rel, b, se)], att = data.table(phase, kind, b = att[1], se = att[2]),
       n = nobs(m), oas_treated = uniqueN(D[cohort < 10000, oa21cd]), schools = uniqueN(D[cohort < 10000, urn]))
}
ctrl_vars <- c("lfa", "rooms", "propertytype", "built_form", "construction_age_band", "eff", "duration", "oldnew")
out <- list()
for (ph in c("secondary", "primary")) for (k in c("Upgrade", "Downgrade")) { message(ph, " ", k); out[[paste(ph, k)]] <- run(ph, k) }
es <- rbindlist(lapply(out, `[[`, "es")); att <- rbindlist(lapply(out, `[[`, "att"))
att[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
info <- rbindlist(lapply(names(out), function(k) data.table(model = k, n = out[[k]]$n, oas_treated = out[[k]]$oas_treated, schools = out[[k]]$schools)))
saveRDS(list(es = es, att = att, info = info, events = E[, .N, by = .(type, prev, grade)], built = Sys.Date()), here::here("data", "ofsted_event_study.rds"))
options(width = 160); print(info); print(att[, .(phase, kind, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(E[, .N, by = .(type, prev, grade)][order(type, prev, grade)])

es <- rbind(es, unique(es[, .(phase, kind)])[, .(phase, kind, rel = -1L, b = 0, se = 0)])
es[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1), years = (rel + 0.5) / 2)]
es[, panel := paste0(tools::toTitleCase(phase), ": ", tolower(kind))]
g <- ggplot(es[abs(rel) < 8], aes(years, pct)) + geom_hline(yintercept = 0, colour = "#6b6680") + geom_vline(xintercept = 0, linetype = 2, colour = "#6b6680") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#c9a2f5", alpha = 0.5) + geom_line(colour = "#361a54", linewidth = 0.6) + geom_point(colour = "#361a54", size = 1.6) +
  facet_wrap(~panel) +
  labs(x = "Years from the published Ofsted report (half-years; the half-year before is the reference)", y = "Price change (%), 95% interval",
       title = "House prices before and after the nearest school's Ofsted grade changes",
       subtitle = "Output Areas whose nearest state school (by walking time) was regraded, March 2019 to December 2024, against areas whose nearest school was not; like-for-like homes",
       caption = "Sun and Abraham event study; OA and district x month fixed effects. Price Paid linked to EPCs (Chi; Beech); Ofsted official statistics and management information") +
  theme_minimal(base_size = 10) + theme(plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave(here::here("figures", "ofsted_event_study.png"), g, width = 10, height = 6.5, dpi = 150)
