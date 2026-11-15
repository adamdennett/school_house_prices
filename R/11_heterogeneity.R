# 11_heterogeneity.R - Where do buyers pay for school results? Admissions type, region, deprivation
# ---------------------------------------------------------------------------
# The within-neighbourhood premium per standard deviation of the nearest state secondary's
# four-year Attainment 8 (school-level SD), by group. Same sample, controls and design as R/06's
# neighbourhood model: log price, hedonic controls | year-month + LSOA, standard errors clustered
# by MSOA, with value added (school-level SD) alongside. The slope is allowed to differ by group:
#   admissions  the nearest secondary's admissions basis: Faith (religious character, where
#               priority is usually by faith, not distance), Selective (grammar), or Other (most
#               use distance or catchment). From DfE applications and offers, school level (latest year).
#   la_bh       Brighton and Hove (catchments with random allocation within them since 2008) against
#               the rest of England
#   region      region of the nearest secondary (DfE applications and offers)
#   imd         quintile of the sale's LSOA on the 2025 Index of Multiple Deprivation (ONSPD rank)
#   urban       the nearest secondary's urban-rural class (DfE): urban or rural
# Plus Step 3's "admissions rules" check on the R/09 split-LSOA design: the better-side premium
# when the better school is Faith, Selective or Other.
# Inputs: data/sales_panel_pop.parquet (R/07), data-raw/admissions (R/07), data/gap_models.rds (R/09 pairs)
# Output: data/heterogeneity.rds, figures/heterogeneity.png      Rscript R/11_heterogeneity.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
S <- as.data.table(read_parquet(here::here("data", "sales_panel_pop.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd) & !is.na(att8_4yr) & !is.na(all_va_shrunk)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"

sch <- unique(S[, .(urn = near_secondary_urn, att8_4yr, all_va_shrunk)])
S[, `:=`(att8_z = (att8_4yr - mean(sch$att8_4yr)) / sd(sch$att8_4yr), va_z = (all_va_shrunk - mean(sch$all_va_shrunk)) / sd(sch$all_va_shrunk))]

A <- fread(here::here("data-raw", "admissions", "supporting-files", "AppsandOffers_2026_SchoolLevel29062026.csv"),
           select = c("time_period", "school_phase", "school_urn", "denomination", "admissions_policy", "urban_rural", "region_name"), colClasses = "character")
A <- A[school_phase == "Secondary"][order(-as.integer(time_period))][, .SD[1], by = school_urn]
A[, `:=`(urn = as.integer(school_urn),
         admissions = fifelse(admissions_policy == "Selective", "Selective (grammar)", fifelse(denomination == "Faith", "Faith", "Other (mostly distance or catchment)")),
         urban = fifelse(grepl("^Urban", urban_rural), "Urban school", fifelse(grepl("rural", urban_rural), "Rural school", NA_character_)))]
S <- merge(S, A[, .(near_secondary_urn = urn, admissions, urban, region = region_name)], by = "near_secondary_urn", all.x = TRUE)
S[, la_bh := fifelse(lad26cd == "E06000043", "Brighton and Hove", "Rest of England")]
S[, imd := paste0("IMD quintile ", pmin(5L, 1L + floor(5 * (as.numeric(imd25ind) - 1) / 33755)), fifelse(as.numeric(imd25ind) <= 6751, " (most deprived)", fifelse(as.numeric(imd25ind) > 27004, " (least deprived)", "")))]

slope <- function(g) {
  d <- S[!is.na(get(g)) & get(g) != ""]
  m <- feols(as.formula(sprintf("lp ~ i(%s, att8_z) + i(%s, va_z) + %s | ym + lsoa21cd", g, g, ctrl)), data = d, cluster = ~msoa21cd, lean = TRUE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(":(att8|va)_z$", term)]
  setnames(ct, 2:3, c("b", "se"))
  n_g <- d[, .N, by = g]; setnames(n_g, c("group", "sales"))
  ct[, `:=`(dim = g, group = sub(".*::", "", sub(":(att8|va)_z$", "", term)), measure = fifelse(grepl("att8", term), "Attainment 8", "Value added"))]
  merge(ct[, .(dim, group, measure, b, se, n = nobs(m))], n_g, by = "group")
}
dims <- c("admissions", "la_bh", "region", "imd", "urban")
H <- rbindlist(lapply(dims, function(g) { message(g); slope(g) }))
H[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
overall <- feols(as.formula(sprintf("lp ~ att8_z + va_z + %s | ym + lsoa21cd", ctrl)), data = S, cluster = ~msoa21cd, lean = TRUE)

# R/09 split-LSOA design by the better school's admissions basis
gm <- readRDS(here::here("data", "gap_models.rds")); pr <- gm$pairs
G <- merge(S, pr[, .(lsoa21cd, hi_urn, lo_urn)], by = "lsoa21cd")
G[, better := as.integer(near_secondary_urn == hi_urn)]
G <- merge(G, A[, .(hi_urn = urn, hi_adm = admissions)], by = "hi_urn", all.x = TRUE)
mb <- feols(as.formula(sprintf("lp ~ i(hi_adm, better) + %s | ym + lsoa21cd", ctrl)), data = G[!is.na(hi_adm)], cluster = ~msoa21cd, lean = TRUE)
B <- as.data.table(coeftable(mb), keep.rownames = "term")[grepl("better", term)]; setnames(B, 2:3, c("b", "se"))
B[, `:=`(group = sub(".*::", "", sub(":better$", "", term)), pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
B <- merge(B, G[!is.na(hi_adm), .(lsoas = uniqueN(lsoa21cd), sales = .N), by = .(group = hi_adm)], by = "group")

saveRDS(list(H = H, overall = coeftable(overall)[c("att8_z", "va_z"), ], better_by_adm = B, n = nrow(S),
             sd_school = c(att8 = sd(sch$att8_4yr), va = sd(sch$all_va_shrunk)), built = Sys.Date()), here::here("data", "heterogeneity.rds"))
options(width = 160)
print(round(coeftable(overall)[c("att8_z", "va_z"), ], 4))
print(H[order(dim, measure, group), .(dim, group, measure, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), sales)])
print(B[, .(group, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), lsoas, sales)])

lab_dim <- c(admissions = "Nearest secondary's admissions", la_bh = "Brighton and Hove", region = "Region", imd = "Neighbourhood deprivation", urban = "Urban or rural school")
P <- H[measure == "Attainment 8"][, dim := factor(lab_dim[dim], lab_dim)]
g <- ggplot(P, aes(pct, reorder(group, pct))) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = "#993bff", size = 0.35) +
  facet_grid(dim ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Price premium (%) per standard deviation of the nearest secondary's Attainment 8, same home, same LSOA (95% interval)", y = NULL,
       title = "Where buyers pay for school results",
       subtitle = sprintf("Within-neighbourhood premium by group, with value added held constant; %s sales, Sept 2018 to Sept 2026", format(nrow(S), big.mark = ",")),
       caption = "Price Paid linked to EPCs (Chi; Beech); walking routes (school_active_travel); DfE Attainment 8 and applications and offers; Value Added v2.1; IMD 2025") +
  theme_minimal(base_size = 10) + theme(plot.title.position = "plot", strip.placement = "outside",
                                        strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1), panel.grid.minor = element_blank())
ggsave(here::here("figures", "heterogeneity.png"), g, width = 10, height = 7.5, dpi = 150)
