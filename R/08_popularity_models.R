# 08_popularity_models.R - Do buyers pay for popular schools, results, or value added?
# ---------------------------------------------------------------------------
# Same sample, controls and three designs as R/06 (raw | ym; same kind of home | ym + district;
# same home, same neighbourhood | ym + LSOA; log price, clustered by MSOA), on the panel with
# popularity (R/07). Standardised measures, entered together:
#   secondary: Attainment 8 (four-year), value added (v2.1, shrunk), log first preferences per place
#   primary:   log first preferences per place of the nearest primary
# Plus the premium for an over-subscribed nearest secondary (2+ first preferences per place) and
# primary, one at a time. Grammar-school areas are left in; the nearest secondary's selective
# flag is a control in the secondary models.
# Output: data/popularity_models.rds      Rscript R/08_popularity_models.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest) })
setFixest_nthreads(8)
S <- as.data.table(read_parquet(here::here("data", "sales_panel_pop.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"
z <- function(x) as.numeric(scale(x))
designs <- function(rhs) c(raw = sprintf("lp ~ %s | ym", rhs), hedonic = sprintf("lp ~ %s + %s | ym + lad26cd", rhs, ctrl),
                           neighbourhood = sprintf("lp ~ %s + %s | ym + lsoa21cd", rhs, ctrl))
fit <- function(rhs, d, keep) rbindlist(lapply(designs(rhs), function(f) {
  m <- feols(as.formula(f), data = d, cluster = ~msoa21cd, lean = TRUE); ct <- coeftable(m)[keep, , drop = FALSE]
  data.table(term = keep, b = ct[, 1], se = ct[, 2], n = nobs(m))
}), idcol = "model")

# secondary: results, value added and popularity together
Sec <- S[!is.na(att8_4yr) & !is.na(all_va_shrunk) & !is.na(secondary_first_per_place) & secondary_first_per_place > 0]
Sec[, `:=`(att8_z = z(att8_4yr), va_z = z(all_va_shrunk), pop_z = z(log(secondary_first_per_place)), sel = as.integer(secondary_selective %in% TRUE))]
sec3 <- fit("att8_z + va_z + pop_z + sel", Sec, c("att8_z", "va_z", "pop_z"))
sec_pop <- fit("pop_z + sel", Sec, "pop_z")
Sec[, over2 := as.integer(secondary_first_per_place >= 2)]
sec_over <- fit("over2 + sel", Sec, "over2")

# primary popularity
Pri <- S[!is.na(primary_first_per_place) & primary_first_per_place > 0]
Pri[, `:=`(pop_z = z(log(primary_first_per_place)), over2 = as.integer(primary_first_per_place >= 2))]
pri_pop <- fit("pop_z", Pri, "pop_z"); pri_over <- fit("over2", Pri, "over2")

res <- list(sec3 = sec3, sec_pop = sec_pop, sec_over = sec_over, pri_pop = pri_pop, pri_over = pri_over,
            n_sec = nrow(Sec), n_pri = nrow(Pri), share_over_sec = mean(Sec$over2), share_over_pri = mean(Pri$over2),
            sd = c(att8 = sd(Sec$att8_4yr), va = sd(Sec$all_va_shrunk), log_pop_sec = sd(log(Sec$secondary_first_per_place)),
                   log_pop_pri = sd(log(Pri$primary_first_per_place))), built = Sys.Date())
saveRDS(res, here::here("data", "popularity_models.rds"))
pct <- function(d) d[, .(model, term, pct = round(100 * (exp(b) - 1), 2), lo = round(100 * (exp(b - 1.96 * se) - 1), 2), hi = round(100 * (exp(b + 1.96 * se) - 1), 2), n)]
options(width = 160)
for (k in c("sec3", "sec_pop", "sec_over", "pri_pop", "pri_over")) { cat("\n==", k, "\n"); print(pct(res[[k]])) }
print(res[c("n_sec", "n_pri", "share_over_sec", "share_over_pri", "sd")])
