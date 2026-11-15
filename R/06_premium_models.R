# 06_premium_models.R - How much more do homes near "good" schools cost? Raw, then like for like
# ---------------------------------------------------------------------------
# Follows Tan and Dennett's Brighton design nationally. Sample: standard residential sales
# (Price Paid category A) in England, September 2018 to September 2026, matched to an EPC, with
# complete dwelling attributes (data/sales_panel.parquet, R/05).
#
# "Near a good school", five ways (each a 0/1 indicator):
#   outst_sec10   an Outstanding state secondary within 10 minutes' walk, at the date of sale
#   outst_pri10   an Outstanding state primary within 10 minutes' walk
#   near_sec_out  the nearest state secondary (by walking time) is Outstanding
#   top_att8      the nearest secondary is in England's top 10% for Attainment 8 (four years)
#   top_va        the nearest secondary is in England's top 10% for Value Added v2.1 (shrunk)
#
# Each is estimated three ways (fixest::feols, log price, standard errors clustered by MSOA):
#   raw           log(price) ~ indicator | year-month                       the premium people see
#   hedonic       + log floor area, rooms, property type, built form, age band, energy efficiency,
#                   tenure (freehold/leasehold), new build | year-month + district
#   neighbourhood + the same with LSOA fixed effects in place of district: homes compared only
#                   with others in the same small neighbourhood
# Premium (%) = 100 (exp(b) - 1). Then, within LSOAs, value added against Attainment 8 together
# (standardised), to see which buyers pay for.
#
# Output: data/premium_models.rds, figures/premium_raw_vs_adjusted.png
#   Rscript R/06_premium_models.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"))]
S[, rooms := pmin(numberrooms, 10)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S[, eff := current_energy_efficiency]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd)]

# school indicators
q90 <- function(x) quantile(unique(x), 0.9, na.rm = TRUE)
sch <- unique(S[, .(near_secondary_urn, att8_4yr, all_va_shrunk)])
cut_att8 <- q90(sch$att8_4yr); cut_va <- q90(sch$all_va_shrunk)
S[, `:=`(outst_sec10 = as.integer(!is.na(outst_secondary) & outst_secondary <= 10),
         outst_pri10 = as.integer(!is.na(outst_primary) & outst_primary <= 10),
         near_sec_out = as.integer(!is.na(near_secondary_grade) & near_secondary_grade == 1),
         top_att8 = as.integer(!is.na(att8_4yr) & att8_4yr >= cut_att8),
         top_va = as.integer(!is.na(all_va_shrunk) & all_va_shrunk >= cut_va))]

ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"
ind <- c(outst_sec10 = "Outstanding secondary within 10 min walk", outst_pri10 = "Outstanding primary within 10 min walk",
         near_sec_out = "Nearest secondary is Outstanding", top_att8 = "Nearest secondary in top 10% for Attainment 8",
         top_va = "Nearest secondary in top 10% for value added")
est <- rbindlist(lapply(names(ind), function(v) {
  f <- list(raw = as.formula(sprintf("lp ~ %s | ym", v)),
            hedonic = as.formula(sprintf("lp ~ %s + %s | ym + lad26cd", v, ctrl)),
            neighbourhood = as.formula(sprintf("lp ~ %s + %s | ym + lsoa21cd", v, ctrl)))
  rbindlist(lapply(names(f), function(k) {
    m <- feols(f[[k]], data = S, cluster = ~msoa21cd, lean = TRUE)
    b <- coef(m)[[v]]; s <- se(m)[[v]]
    data.table(indicator = v, label = ind[[v]], model = k, b = b, se = s, n = nobs(m),
               premium = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * s) - 1), hi = 100 * (exp(b + 1.96 * s) - 1),
               share_treated = mean(S[[v]]))
  }))
}))
# value added against Attainment 8, within neighbourhoods (secondaries with both)
Z <- S[!is.na(att8_4yr) & !is.na(all_va_shrunk)]
Z[, `:=`(att8_z = as.numeric(scale(att8_4yr)), va_z = as.numeric(scale(all_va_shrunk)))]
horse <- lapply(c(raw = "lp ~ att8_z + va_z | ym", hedonic = sprintf("lp ~ att8_z + va_z + %s | ym + lad26cd", ctrl),
                  neighbourhood = sprintf("lp ~ att8_z + va_z + %s | ym + lsoa21cd", ctrl)),
                function(f) { m <- feols(as.formula(f), data = Z, cluster = ~msoa21cd, lean = TRUE); coeftable(m)[c("att8_z", "va_z"), ] })

res <- list(est = est, horse = horse, n_sales = nrow(S), cuts = c(att8 = cut_att8, va = cut_va),
            years = range(S$date), built = Sys.Date())
saveRDS(res, here::here("data", "premium_models.rds"))

est[, model := factor(model, c("raw", "hedonic", "neighbourhood"), c("Raw", "Same kind of home", "Same home, same neighbourhood"))]
est[, label := factor(label, rev(ind))]
g <- ggplot(est, aes(premium, label, colour = model)) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), size = 0.4) +
  scale_colour_manual(values = c("Raw" = "#c9a2f5", "Same kind of home" = "#993bff", "Same home, same neighbourhood" = "#361a54"), name = NULL) +
  labs(x = "Price premium (%), with 95% interval", y = NULL,
       title = "House prices near 'good' schools in England, raw and like for like",
       subtitle = sprintf("%s sales, Sept 2018 to Sept 2026", format(nrow(S), big.mark = ",")),
       caption = "Land Registry Price Paid linked to EPCs (Chi; Beech); walking times from the national routing; Ofsted; Value Added v2.1") +
  theme_minimal(base_size = 11) + theme(legend.position = "top", plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave(here::here("figures", "premium_raw_vs_adjusted.png"), g, width = 9, height = 5, dpi = 150)

options(width = 160)
print(est[, .(label, model, premium = round(premium, 1), lo = round(lo, 1), hi = round(hi, 1), n, treated = round(100 * share_treated, 1))])
print(lapply(horse, round, 4))
