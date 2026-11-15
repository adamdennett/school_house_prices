# 16_premium_pounds.R - The premium in pounds, by property type and year
# ---------------------------------------------------------------------------
# Puts R/06's percentages in terms of what people pay. The premium is estimated separately for
# each property type (detached, semi-detached, terraced, flat or maisonette) in each year of sale
# (September 2018 to September 2026; years are calendar years of sale), in R/06's three designs:
#   raw            log(price) ~ indicator x type-year | year-month + type-year (homes of the same type sold that year)
#   same district  + dwelling attributes (R/06) | year-month + local authority district
#   same LSOA      + dwelling attributes | year-month + LSOA
# for two indicators: the nearest state secondary is Outstanding (Ofsted at the date of sale;
# 15.5% of sales), and the nearest secondary is in England's top 10% for four-year Attainment 8.
# Premium in pounds = 100 (exp(b) - 1)% of the median price of that property type in that year
# (all category A matched sales in England), with the 95% interval scaled the same way. The
# median price is the typical home of that type, not the typical home near a good school.
# Input: data/sales_panel.parquet (R/05). Output: data/premium_pounds.rds, figures/premium_pounds.png
#   Rscript R/16_premium_pounds.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0 & propertytype %in% c("D", "S", "T", "F")]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), yr = as.integer(format(date, "%Y")), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd)]
types <- c(D = "Detached", S = "Semi-detached", T = "Terraced", F = "Flat or maisonette")
S[, ty := paste(propertytype, yr, sep = "_")]
cut_att8 <- quantile(unique(S[, .(near_secondary_urn, att8_4yr)])$att8_4yr, 0.9, na.rm = TRUE)
S[, `:=`(near_sec_out = as.integer(!is.na(near_secondary_grade) & near_secondary_grade == 1),
         top_att8 = as.integer(!is.na(att8_4yr) & att8_4yr >= cut_att8))]
med <- S[, .(median_price = median(price), sales = .N), by = .(propertytype, yr)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"
des <- c(raw = "| ym + ty", district = sprintf("+ %s | ym + ty + lad26cd", ctrl), lsoa = sprintf("+ %s | ym + ty + lsoa21cd", ctrl))   # ty: each type-year has its own baseline, so like is compared with like
P <- rbindlist(lapply(c("near_sec_out", "top_att8"), function(v) rbindlist(lapply(names(des), function(k) {
  message(v, " ", k)
  m <- feols(as.formula(sprintf("lp ~ i(ty, %s) %s", v, des[[k]])), data = S, cluster = ~msoa21cd, lean = TRUE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl("^ty::", term)]; setnames(ct, 2:3, c("b", "se"))
  ct[, `:=`(indicator = v, design = k, propertytype = sub("ty::([A-Z])_.*", "\\1", term), yr = as.integer(sub(".*_([0-9]{4}):.*", "\\1", term)))]
}))))
P <- merge(P, med, by = c("propertytype", "yr"))
P[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
P[, `:=`(gbp = median_price * pct / 100, gbp_lo = median_price * lo / 100, gbp_hi = median_price * hi / 100, type = types[propertytype])]
treated <- S[, .(near_sec_out = mean(near_sec_out), top_att8 = mean(top_att8)), by = .(propertytype)]
saveRDS(list(P = P, med = med, treated = treated, n = nrow(S), built = Sys.Date()), here::here("data", "premium_pounds.rds"))
options(width = 170)
print(dcast(P[indicator == "near_sec_out", .(type, yr, design, v = sprintf("%+.1f%% £%s", pct, format(round(gbp, -2), big.mark = ",")))], type + yr ~ design, value.var = "v"))
print(dcast(med, yr ~ propertytype, value.var = "median_price"))

P[, `:=`(design = factor(design, c("raw", "district", "lsoa"), c("Raw: homes of the same type sold that year", "Same kind of home, same local authority", "Same kind of home, same neighbourhood (LSOA)")),
         type = factor(type, types))]
g <- ggplot(P[indicator == "near_sec_out"], aes(yr, gbp / 1000, colour = design, fill = design)) +
  geom_hline(yintercept = 0, colour = "#6b6680") +
  geom_ribbon(aes(ymin = gbp_lo / 1000, ymax = gbp_hi / 1000), alpha = 0.18, colour = NA) + geom_line(linewidth = 0.7) + geom_point(size = 1.4) +
  facet_wrap(~type, nrow = 1) +
  scale_colour_manual(values = c("#c9a2f5", "#993bff", "#361a54"), name = NULL, aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(2018, 2026, 2)) + scale_y_continuous(labels = function(x) paste0("£", x, "k")) +
  labs(x = NULL, y = "Extra paid for a home whose nearest\nsecondary is Outstanding (£, 95% interval)",
       title = "What a home near an Outstanding secondary costs extra, in pounds",
       subtitle = "Premium for each property type and year, applied to that type's median price that year (England)",
       caption = "Price Paid linked to EPCs (Chi; Beech); nearest school by walking time (school_active_travel); Ofsted at the date of sale") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave(here::here("figures", "premium_pounds.png"), g, width = 11, height = 4.8, dpi = 150)
