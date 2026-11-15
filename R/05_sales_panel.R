# 05_sales_panel.R - Sales from September 2018, with their nearest schools and the schools' quality
# ---------------------------------------------------------------------------
# One row per matched sale (R/01, with EPC attributes from R/03), September 2018 to September
# 2026, the years for which Ofsted grades at the date of sale are in hand. Each sale gets:
#   its 2021 OA, LSOA, MSOA, district and 2025 IMD rank (R/02, ONS Postcode Directory)
#   its OA's nearest state primary and secondary by walking time, in minutes (R/04)
#   each nearest school's Ofsted overall effectiveness at the date of sale
#   the walking time from its OA to the nearest Outstanding (and Good or better) state primary
#     and secondary within 20 minutes, at the date of sale
#   the nearest secondary's Value Added v2.1 (four-year, shrunk, all pupils) and mean Attainment 8
#     (2021/22 to 2024/25), which are not dated: VA for a school's recent years
#
# Ofsted grade at the date of sale: the grade in Ofsted's official statistics as at 31 August
# before the sale (2018 to 2023 files); for sales after August 2023, the latest graded (OEIF)
# overall effectiveness in the management information as at 30 September 2026 if that
# inspection started before the sale, else the 2023 grade. Graded inspections ended in
# September 2024, so grades are frozen after that. URNs in the Ofsted files are followed to the
# school's current URN through GIAS's one-to-one successor links (as R/43 in school_value_added),
# so an academy inherits its predecessor's grade.
#
# Inputs: data/sales_epc.parquet (R/03; falls back to data/ppd_epc_matched.parquet), data/postcode_lookup.parquet,
#   data/oa_nearest.parquet, data/oa_school_pairs20.parquet (R/04);
#   ../school_value_added/data-raw/ofsted/*.csv, ../school_value_added/data-raw/gias/links_edubasealldata20260610.csv,
#   ../school_value_added/data/va_v2_schools.csv, ../school_value_added/app/data/app_data.rds (four-year Attainment 8)
# Output: data/sales_panel.parquet     Rscript R/05_sales_panel.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow) })
sva <- normalizePath(file.path(dirname(here::here()), "school_value_added"), winslash = "/")
num <- function(x) suppressWarnings(as.numeric(x))

# ---- sales ----------------------------------------------------------------------------------
f <- here::here("data", "sales_epc.parquet")
if (!file.exists(f)) f <- here::here("data", "ppd_epc_matched.parquet")
S <- as.data.table(read_parquet(f))
S[, date := as.IDate(dateoftransfer)]
S <- S[date >= as.IDate("2018-09-01")]
S[, pc := gsub(" ", "", toupper(postcode))]
P <- as.data.table(read_parquet(here::here("data", "postcode_lookup.parquet")))[, .(pc, oa21cd, lsoa21cd, msoa21cd, lad26cd, imd25ind, east1m, north1m)]
P <- unique(P, by = "pc")
S <- merge(S, P, by = "pc")
S <- merge(S, as.data.table(read_parquet(here::here("data", "oa_nearest.parquet"))), by = "oa21cd", all.x = TRUE)

# ---- Ofsted grades as dated events, on current URNs ---------------------------------------------
L <- fread(file.path(sva, "data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
succ <- unique(L[LinkType == "Successor", .(from = URN, to = LinkURN)]); succ <- succ[!from %in% succ[duplicated(from), from]]
final_of <- function(u) { f <- as.integer(u); for (i in 1:10) { nx <- succ$to[match(f, succ$from)]; f <- ifelse(is.na(nx), f, nx) }; f }
ev <- rbindlist(lapply(2018:2023, function(y) {
  x <- fread(file.path(sva, "data-raw", "ofsted", sprintf("ofsted_as_at_31_aug_%d.csv", y)), select = c("URN", "Overall effectiveness"), showProgress = FALSE)
  x[, .(urn = as.integer(URN), date = as.IDate(sprintf("%d-08-31", y)), grade = num(`Overall effectiveness`))]
}))
lat <- fread(file.path(sva, "data-raw", "ofsted", "Management_information_-_state-funded_schools_-_latest_inspections_as_at_30_Sep_2026.csv"),
             select = c("URN", "Inspection start date of latest OEIF graded inspection", "Latest OEIF overall effectiveness"), showProgress = FALSE)
setnames(lat, c("urn", "d", "grade"))
lat[, `:=`(urn = as.integer(urn), date = as.IDate(as.Date(ifelse(d == "NULL", NA, d), format = "%d/%m/%Y")), grade = num(grade))]
ev <- rbind(ev, lat[date > as.IDate("2023-08-31") & !is.na(grade), .(urn, date, grade)])
ev <- ev[!is.na(grade) & grade %in% 1:4]
ev[, urn := final_of(urn)]
ev <- ev[order(urn, date, grade)][, .SD[1], by = .(urn, date)]
setkey(ev, urn, date)
grade_at <- function(u_, d_) { k <- data.table(urn = as.integer(u_), date = as.IDate(d_)); ev[k, on = .(urn, date), roll = Inf, x.grade] }
S[, `:=`(near_primary_grade = grade_at(near_primary_urn, date), near_secondary_grade = grade_at(near_secondary_urn, date))]

# ---- nearest Outstanding / Good-or-better school within 20 minutes, at the date of sale ----------
# the grade a school holds is constant between its events, so evaluate per OA and Ofsted "epoch"
pairs <- as.data.table(read_parquet(here::here("data", "oa_school_pairs20.parquet")))[, walk_min := as.numeric(walk_min)]
epochs <- as.IDate(c(sprintf("%d-08-31", 2018:2023), "2023-11-30", "2024-02-29", "2024-05-31", "2024-09-30"))   # year ends, then quarters until grades stopped (Sept 2024)
S[, epoch := epochs[findInterval(date, epochs)]]
best_within <- function(ep) {
  g <- ev[data.table(urn = pairs$urn, date = ep), on = .(urn, date), roll = Inf, x.grade]
  x <- pairs[, .(oa21cd, walk_min, primary, secondary, grade = g)]
  rbind(x[primary == TRUE, .(phase = "primary", outst = suppressWarnings(min(walk_min[grade == 1], na.rm = TRUE)),
                              good = suppressWarnings(min(walk_min[grade <= 2], na.rm = TRUE))), by = oa21cd],
        x[secondary == TRUE, .(phase = "secondary", outst = suppressWarnings(min(walk_min[grade == 1], na.rm = TRUE)),
                                good = suppressWarnings(min(walk_min[grade <= 2], na.rm = TRUE))), by = oa21cd])[, epoch := ep]
}
used <- sort(unique(S$epoch))
B <- rbindlist(lapply(used, best_within))
for (v in c("outst", "good")) B[!is.finite(get(v)), (v) := NA_real_]
Bw <- dcast(B, oa21cd + epoch ~ phase, value.var = c("outst", "good"))
S <- merge(S, Bw, by = c("oa21cd", "epoch"), all.x = TRUE)

# ---- nearest secondary's value added and Attainment 8 ---------------------------------------------
va <- fread(file.path(sva, "data", "va_v2_schools.csv"), select = c("URN", "all_va_shrunk", "disadvantaged_va_shrunk", "non_disadvantaged_va_shrunk"))
ad <- readRDS(file.path(sva, "app", "data", "app_data.rds"))
a8 <- as.data.table(ad$four)[group == "all", .(URN, att8_4yr = att8, exp_4yr = exp)]
qual <- merge(va, a8, by = "URN", all = TRUE)
setnames(qual, "URN", "near_secondary_urn")
S <- merge(S, qual, by = "near_secondary_urn", all.x = TRUE)

S[, pc := NULL]
write_parquet(S, here::here("data", "sales_panel.parquet"))
message(sprintf("%s sales, %s to %s; nearest secondary graded for %.1f%%, with value added for %.1f%%",
                format(nrow(S), big.mark = ","), min(S$date), max(S$date),
                100 * mean(!is.na(S$near_secondary_grade)), 100 * mean(!is.na(S$all_va_shrunk))))
