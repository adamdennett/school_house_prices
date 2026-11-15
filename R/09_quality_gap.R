# 09_quality_gap.R - Is the premium concentrated where neighbouring schools differ most?
# ---------------------------------------------------------------------------
# A walking-time analogue of Greaves and Venturin's (2026, IZA DP 18750) catchment-boundary
# design. Where an LSOA is split between two nearest state secondaries (by walking time from each
# Output Area; 7,643 LSOAs, about 1.3m sales), homes in the same LSOA are compared across the line
# where the nearest school changes. "Better side" = the OA whose nearest secondary has the higher
# four-year mean Attainment 8. Models (fixest::feols, log price, hedonic controls as R/06,
# year-month and LSOA fixed effects, standard errors clustered by MSOA):
#   m1  better side
#   m2  better side x quartile of the Attainment 8 gap between the two schools (gap quartiles by LSOA)
#   m3  better side x pair position: both schools below the national median, straddling, both above
#       (national median of school-level Attainment 8); and G&V's "escaping the bottom" (worse school in
#       Q1, better in Q2) against "reaching the top" (worse in Q3, better in Q4)
#   m4  better side x whether the higher-attainment school also has the lower free school meals share
#       (FSM eligible %, DfE applications and offers school file, mean of entry 2022 to 2026)
#   m5  better side on Attainment 8 and, separately, the side with the higher value added (v2.1)
#   m6  m1 and m2 for sales near the switch line only: OAs whose walk to the other school is
#       within 3 minutes of the walk to their own nearest (from R/04's pairs within 20 minutes)
# Differences from G&V: nearest school by walking time, not catchment; no exclusion of lines on
# roads, rivers or railways (m6 is the partial check); Attainment 8 2021/22 to 2024/25, not KS4 at sale.
# Inputs: data/sales_panel_pop.parquet (R/07), data/oa_school_pairs20.parquet (R/04),
#   data-raw/admissions/supporting-files/AppsandOffers_2026_SchoolLevel29062026.csv
# Output: data/gap_models.rds, figures/gap_premiums.png       Rscript R/09_quality_gap.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
num <- function(x) suppressWarnings(as.numeric(x))
S <- as.data.table(read_parquet(here::here("data", "sales_panel_pop.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd) & !is.na(att8_4yr)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"

# school-level measures and national positions
sch <- unique(S[, .(urn = near_secondary_urn, att8 = att8_4yr, va = all_va_shrunk)])
nat <- unique(as.data.table(read_parquet(here::here("data", "sales_panel_pop.parquet"), col_select = c("near_secondary_urn", "att8_4yr")))[!is.na(att8_4yr)])
qs <- quantile(nat$att8_4yr, c(0.25, 0.5, 0.75))
A <- fread(here::here("data-raw", "admissions", "supporting-files", "AppsandOffers_2026_SchoolLevel29062026.csv"),
           select = c("time_period", "school_phase", "school_urn", "FSM_eligible_percent"), colClasses = "character")
fsm <- A[school_phase == "Secondary" & as.integer(substr(time_period, 1, 4)) >= 2022,
         .(fsm = mean(num(FSM_eligible_percent), na.rm = TRUE)), by = .(urn = as.integer(school_urn))]
sch <- merge(sch, fsm, by = "urn", all.x = TRUE)

# LSOAs split between exactly two nearest secondaries
k <- unique(S[, .(lsoa21cd, urn = near_secondary_urn)])[, n := .N, by = lsoa21cd][n == 2]
k <- merge(k, sch, by = "urn")[order(lsoa21cd, -att8)]
pr <- k[, .(hi_urn = urn[1], lo_urn = urn[2], hi_att8 = att8[1], lo_att8 = att8[2], hi_va = va[1], lo_va = va[2],
            hi_fsm = fsm[1], lo_fsm = fsm[2]), by = lsoa21cd][hi_att8 > lo_att8]
pr[, `:=`(gap = hi_att8 - lo_att8,
          position = fifelse(hi_att8 < qs[2], "Both below median", fifelse(lo_att8 >= qs[2], "Both above median", "Straddling median")),
          qpair = paste0("Q", findInterval(lo_att8, qs) + 1, "-Q", findInterval(hi_att8, qs) + 1),
          aligned = fifelse(is.na(hi_fsm) | is.na(lo_fsm), NA_character_, fifelse(hi_fsm < lo_fsm, "Higher results, fewer FSM", "Higher results, more FSM")))]
pr[, gap_q := paste0("Gap Q", cut(gap, quantile(gap, 0:4 / 4), labels = FALSE, include.lowest = TRUE))]
G <- merge(S, pr, by = "lsoa21cd")
G[, `:=`(better = as.integer(near_secondary_urn == hi_urn),
         better_va = as.integer(!is.na(hi_va) & !is.na(lo_va) & ((near_secondary_urn == hi_urn) == (hi_va > lo_va))))]

# near the switch line: the other school is within 3 minutes' walk of the nearest
pairs <- as.data.table(read_parquet(here::here("data", "oa_school_pairs20.parquet")))[, .(oa21cd, urn, walk_min = as.numeric(walk_min))]
G[, other_urn := fifelse(better == 1, lo_urn, hi_urn)]
G <- merge(G, pairs[, .(oa21cd, other_urn = urn, other_min = walk_min)], by = c("oa21cd", "other_urn"), all.x = TRUE)
G[, near_line := !is.na(other_min) & (other_min - near_secondary_min) <= 3]

fit <- function(rhs, d, keep_re) {
  m <- feols(as.formula(sprintf("lp ~ %s + %s | ym + lsoa21cd", rhs, ctrl)), data = d, cluster = ~msoa21cd, lean = TRUE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(keep_re, term)]
  setnames(ct, 2:3, c("b", "se"))
  ct[, .(term, b, se, n = nobs(m), pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
}
res <- list(
  m1 = fit("better", G, "^better$"),
  m2 = fit("i(gap_q, better)", G, "better"),
  m3 = fit("i(position, better)", G, "better"),
  m3q = fit("i(qpair, better)", G[qpair %in% c("Q1-Q2", "Q2-Q3", "Q3-Q4", "Q1-Q1", "Q4-Q4")], "better"),
  m4 = fit("i(aligned, better)", G[!is.na(aligned)], "better"),
  m5 = fit("better + better_va", G[!is.na(hi_va) & !is.na(lo_va)], "better"),
  m6 = fit("better", G[near_line == TRUE], "^better$"),
  m6g = fit("i(gap_q, better)", G[near_line == TRUE], "better"),
  pairs = pr, n_lsoa = nrow(pr), n_sales = nrow(G), n_near_line = sum(G$near_line),
  gap_cuts = quantile(pr$gap, 0:4 / 4), nat_quartiles = qs, built = Sys.Date())
saveRDS(res, here::here("data", "gap_models.rds"))

options(width = 160)
for (x in c("m1", "m2", "m3", "m3q", "m4", "m5", "m6", "m6g")) { cat("\n==", x, "\n"); print(res[[x]][, .(term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), n)]) }
cat("\nLSOAs", res$n_lsoa, " sales", res$n_sales, " near line", res$n_near_line, "\n"); print(round(res$gap_cuts, 1)); print(table(pr$position)); print(table(pr$aligned, useNA = "ifany"))

# figure: premium for the better side, by gap quartile and by pair position (all, and near the line)
lab <- function(d, panel, set) d[, .(panel, set, group = sub(".*::", "", sub(":better$", "", term)), pct, lo, hi)]
F <- rbind(lab(res$m1[, term := "All split LSOAs"], "Overall", "All sales"),
           lab(res$m6[, term := "All split LSOAs"], "Overall", "Within 3 min of the line"),
           lab(res$m2, "By size of the Attainment 8 gap", "All sales"), lab(res$m6g, "By size of the Attainment 8 gap", "Within 3 min of the line"),
           lab(res$m3, "By where the pair sits nationally", "All sales"), lab(res$m4, "By free school meals", "All sales"))
F[, panel := factor(panel, unique(panel))]
g <- ggplot(F, aes(pct, group, colour = set)) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.35) +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c("All sales" = "#993bff", "Within 3 min of the line" = "#361a54"), name = NULL) +
  labs(x = "Price premium (%) on the side whose nearest secondary has higher Attainment 8, 95% interval", y = NULL,
       title = "Same neighbourhood, different nearest secondary: the premium for the 'better' side",
       subtitle = sprintf("%s LSOAs split between two nearest state secondaries by walking time; %s sales, Sept 2018 to Sept 2026; like-for-like homes",
                          format(res$n_lsoa, big.mark = ","), format(res$n_sales, big.mark = ",")),
       caption = "Land Registry Price Paid linked to EPCs (Chi; Beech); walking routes (school_active_travel); DfE Attainment 8 and applications and offers") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", plot.title.position = "plot", strip.placement = "outside",
                                        strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1), panel.grid.minor = element_blank())
ggsave(here::here("figures", "gap_premiums.png"), g, width = 10, height = 6.5, dpi = 150)
