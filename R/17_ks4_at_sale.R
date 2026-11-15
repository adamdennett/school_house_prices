# 17_ks4_at_sale.R - The nearest secondary's published results at the date of sale
# ---------------------------------------------------------------------------
# R/06 to R/12 use a four-year mean Attainment 8 (2021/22 to 2024/25) for every sale. Here each sale
# gets the results a buyer could have seen: the latest school performance tables published before
# the sale. Provisional KS4 tables are published each October (dates used: 20 Oct 2022 for 2021/22,
# 19 Oct 2023 for 2022/23, 17 Oct 2024 for 2023/24, 16 Oct 2025 for 2024/25). No school tables were
# published for 2019/20 or 2020/21, so until October 2022 buyers saw 2018/19 results. The 2016/17
# to 2018/19 tables are not in hand (the performance tables download service refuses scripted
# downloads), so this covers sales from 20 October 2022 to September 2026.
#
# Attainment 8 is standardised within each results year across schools (z), because grading
# changed between years (2021/22 was graded more generously than 2022/23 onwards).
# Source: school_value_added app data (DfE KS4 institution-level performance, all pupils).
#
# Models (log price, R/06 hedonic controls, clustered by MSOA unless stated):
#   M1 same LSOA            att8_sale_z | ym + LSOA
#   M2 same LSOA, both      att8_sale_z + att8_4yr_z | ym + LSOA   (does the latest year matter beyond the average?)
#   M3 within school        att8_sale_z | nearest secondary + district x ym + LSOA:
#                           only year-to-year changes in a school's published results identify it
#   M4 same property        att8_sale_z + lfa + rooms + eff | property (UPRN) + district x ym, clustered by property
#   M5 within school, news  change in the school's z from the previous published year, as a "news" term,
#                           with the previous year's z held fixed | nearest secondary + district x ym + LSOA
# Inputs: data/sales_panel.parquet (R/05), ../school_value_added/app/data/app_data.rds
# Output: data/ks4_at_sale.rds      Rscript R/17_ks4_at_sale.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest) })
setFixest_nthreads(8)
sva <- normalizePath(file.path(dirname(here::here()), "school_value_added"), winslash = "/")
Y <- as.data.table(readRDS(file.path(sva, "app", "data", "app_data.rds"))$years)[group == "all" & !is.na(att8), .(URN, year, att8)]
Y[, z := (att8 - mean(att8)) / sd(att8), by = year]
pub <- data.table(year = c("2021-22", "2022-23", "2023-24", "2024-25"),
                  pub = as.IDate(c("2022-10-20", "2023-10-19", "2024-10-17", "2025-10-16")))
Y <- merge(Y, pub, by = "year")
setorder(Y, URN, pub); Y[, z_prev := shift(z), by = URN]

S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0 & date >= as.IDate("2022-10-20")]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd) & !is.na(near_secondary_urn)]
# the latest published results at the date of sale (rolling join on publication date)
K <- Y[, .(near_secondary_urn = URN, date = pub, results_year = year, att8_sale = att8, att8_sale_z = z, z_prev)]
setkey(K, near_secondary_urn, date)
S[, c("results_year", "att8_sale", "att8_sale_z", "z_prev") := K[S[, .(near_secondary_urn, date)], on = .(near_secondary_urn, date), roll = Inf,
                                                                   .(x.results_year, x.att8_sale, x.att8_sale_z, x.z_prev)]]
sch <- unique(S[!is.na(att8_4yr), .(near_secondary_urn, att8_4yr)])
S[, att8_4yr_z := (att8_4yr - mean(sch$att8_4yr)) / sd(sch$att8_4yr)]
S <- S[!is.na(att8_sale_z)]
S[, news := att8_sale_z - z_prev]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"
fit <- function(f, d, keep, cl = ~msoa21cd) {
  m <- feols(as.formula(f), data = d, cluster = cl, lean = TRUE); ct <- coeftable(m)[keep, , drop = FALSE]
  data.table(term = keep, b = ct[, 1], se = ct[, 2], n = nobs(m))
}
S[, uprn := fifelse(is.na(uprn) | uprn == "", NA_character_, as.character(uprn))]
Rp <- S[!is.na(uprn)][, k := .N, by = uprn][k >= 2]
res <- list(
  M1 = fit(sprintf("lp ~ att8_sale_z + %s | ym + lsoa21cd", ctrl), S, "att8_sale_z")[, model := "Same LSOA: results at the date of sale"],
  M2 = fit(sprintf("lp ~ att8_sale_z + att8_4yr_z + %s | ym + lsoa21cd", ctrl), S[!is.na(att8_4yr_z)], c("att8_sale_z", "att8_4yr_z"))[, model := "Same LSOA: date of sale and four-year mean together"],
  M3 = fit(sprintf("lp ~ att8_sale_z + %s | near_secondary_urn + lad26cd^ym + lsoa21cd", ctrl), S, "att8_sale_z")[, model := "Within school: year-to-year changes in published results"],
  M4 = fit("lp ~ att8_sale_z + lfa + i(rooms) + eff | uprn + lad26cd^ym", Rp, "att8_sale_z", ~uprn)[, model := "Same property: repeat sales"],
  M5 = fit(sprintf("lp ~ news + z_prev + %s | near_secondary_urn + lad26cd^ym + lsoa21cd", ctrl), S[!is.na(news)], c("news", "z_prev"))[, model := "Within school: the change from the previous year"])
R <- rbindlist(res)
R[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
info <- list(n = nrow(S), n_repeat = nrow(Rp), props_repeat = uniqueN(Rp$uprn), by_results_year = S[, .N, by = results_year][order(results_year)],
             sd_change = S[!is.na(news), sd(news)], sd_z = sd(Y$z), pub = pub)
saveRDS(list(R = R, info = info, built = Sys.Date()), here::here("data", "ks4_at_sale.rds"))
options(width = 160); print(R[, .(model, term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), n)]); print(info[c("n", "n_repeat", "props_repeat", "sd_change")]); print(info$by_results_year)
