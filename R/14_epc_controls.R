# 14_epc_controls.R - Does it hold with full property characteristics, and for the same property?
# ---------------------------------------------------------------------------
# Two checks on R/06 and R/12.
#
# A. Control ladder. The nearest state secondary's Attainment 8 and value added (school-level SDs,
#    together), and "nearest secondary is Outstanding" (Ofsted at the date of sale), estimated with
#    more and more of the property's characteristics, first with year-month effects only, then
#    with year-month and LSOA fixed effects:
#      C0  none
#      C1  what Price Paid alone gives: property type, freehold/leasehold, new build
#      C2  + EPC size: log floor area, habitable rooms
#      C3  + EPC construction age band, built form, energy efficiency score   (the main specification)
#      C4  + every other usable EPC field: floor area decile x property type, energy rating band,
#          main fuel, mains gas, floor level (flats and maisonettes), extensions, heated rooms, EPC
#          property type (house, bungalow, flat, maisonette), years from the EPC inspection to the sale
#    Standard errors clustered by MSOA.
#
# B. Repeat sales. Properties (EPC UPRN) sold more than once between September 2018 and September
#    2026 (about 700,000 properties, 1.4m sales). Property fixed effects absorb everything fixed
#    about the home and its plot, measured or not; district x year-month effects absorb local
#    price trends. Only school measures that change over time are identified:
#      near_sec_out / near_pri_out   the nearest secondary / primary is Outstanding at the date of sale
#      outst_sec10 / outst_pri10     an Outstanding secondary / primary within 10 minutes' walk
#      pop_sec / pop_pri             log first preferences per place in the admissions round of that year (per SD)
#    Each alone, with the time-varying EPC measures (log floor area, rooms, energy efficiency) as
#    controls, clustered by property. For comparison, the same indicators on the same sales with
#    LSOA fixed effects instead of property effects.
# Input: data/sales_panel_pop.parquet (R/07). Output: data/epc_controls.rds, figures/epc_ladder.png
#   Rscript R/14_epc_controls.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
S <- as.data.table(read_parquet(here::here("data", "sales_panel_pop.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd)]

# extended EPC fields, cleaned
S[, fuel := fifelse(grepl("gas", main_fuel, ignore.case = TRUE) & !grepl("LPG|bottled", main_fuel, ignore.case = TRUE), "gas",
            fifelse(grepl("electric", main_fuel, ignore.case = TRUE), "electricity", fifelse(grepl("oil", main_fuel, ignore.case = TRUE), "oil",
            fifelse(grepl("LPG|bottled", main_fuel, ignore.case = TRUE), "LPG", fifelse(is.na(main_fuel), "unknown", "other")))))]
S[, gasflag := fifelse(is.na(mains_gas_flag), "unknown", mains_gas_flag)]
fl <- tolower(trimws(S$floor_level))
S[, floor := fifelse(!property_type_epc %in% c("Flat", "Maisonette"), "not a flat",
             fifelse(is.na(fl), "flat, unknown", fifelse(grepl("basement|^-", fl), "basement", fifelse(grepl("ground|^0", fl), "ground",
             fifelse(grepl("top", fl), "top", fifelse(grepl("mid", fl), "mid", fifelse(grepl("^1", fl), "1st",
             fifelse(grepl("^(2|3)", fl), "2nd-3rd", "4th+"))))))))]
S[, `:=`(ext = fifelse(is.na(extension_count), "unknown", as.character(pmin(extension_count, 3))),
         heated = fifelse(is.na(number_heated_rooms), "unknown", as.character(pmin(number_heated_rooms, 10))),
         rating = fifelse(is.na(current_energy_rating), "unknown", current_energy_rating),
         epctype = fifelse(is.na(property_type_epc), "unknown", property_type_epc),
         epc_age = cut(as.numeric(date - as.IDate(inspectiondate)) / 365.25, c(-Inf, 0, 1, 3, 6, 10, Inf)),
         fa_dec = paste(propertytype, cut(tfarea, unique(quantile(tfarea, 0:10 / 10)), include.lowest = TRUE)))]

rung <- c(C0 = "1", C1 = "i(propertytype) + i(duration) + i(oldnew)",
          C2 = "i(propertytype) + i(duration) + i(oldnew) + lfa + i(rooms)",
          C3 = "i(propertytype) + i(duration) + i(oldnew) + lfa + i(rooms) + i(construction_age_band) + i(built_form) + eff")
rung["C4"] <- paste(rung["C3"], "+ i(fa_dec) + i(rating) + i(fuel) + i(gasflag) + i(floor) + i(ext) + i(heated) + i(epctype) + i(epc_age)")
lab_rung <- c(C0 = "No property controls", C1 = "Price Paid: type, tenure, new build", C2 = "+ EPC floor area and rooms",
              C3 = "+ EPC age, built form, energy efficiency", C4 = "+ every other EPC field")

sch <- unique(S[!is.na(att8_4yr) & !is.na(all_va_shrunk), .(near_secondary_urn, att8_4yr, all_va_shrunk)])
S[, `:=`(att8_z = (att8_4yr - mean(sch$att8_4yr)) / sd(sch$att8_4yr), va_z = (all_va_shrunk - mean(sch$all_va_shrunk)) / sd(sch$all_va_shrunk),
         near_sec_out = as.integer(!is.na(near_secondary_grade) & near_secondary_grade == 1))]
A <- S[!is.na(att8_z) & !is.na(va_z)]
lad <- rbindlist(lapply(names(rung), function(r) rbindlist(lapply(c("ym", "ym + lsoa21cd"), function(fe) {
  message(r, " | ", fe)
  m1 <- feols(as.formula(sprintf("lp ~ att8_z + va_z + %s | %s", rung[r], fe)), data = A, cluster = ~msoa21cd, lean = TRUE)
  m2 <- feols(as.formula(sprintf("lp ~ near_sec_out + %s | %s", rung[r], fe)), data = S, cluster = ~msoa21cd, lean = TRUE)
  ct <- rbind(coeftable(m1)[c("att8_z", "va_z"), 1:2, drop = FALSE], coeftable(m2)["near_sec_out", 1:2, drop = FALSE])
  data.table(rung = r, fe = fe, term = rownames(ct), b = ct[, 1], se = ct[, 2], r2 = c(r2(m1, "wr2"), r2(m1, "wr2"), r2(m2, "wr2")),
             n = c(nobs(m1), nobs(m1), nobs(m2)))
}))))
lad[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]

# ---- B. repeat sales --------------------------------------------------------------------------------
S[, uprn := fifelse(is.na(uprn) | uprn == "", NA_character_, as.character(uprn))]
R <- S[!is.na(uprn)][, nsale := .N, by = uprn][nsale >= 2]
R[, `:=`(near_pri_out = as.integer(!is.na(near_primary_grade) & near_primary_grade == 1),
         outst_sec10 = as.integer(!is.na(outst_secondary) & outst_secondary <= 10), outst_pri10 = as.integer(!is.na(outst_primary) & outst_primary <= 10),
         pop_sec = as.numeric(scale(log(fifelse(secondary_first_per_place > 0, secondary_first_per_place, NA_real_)))),
         pop_pri = as.numeric(scale(log(fifelse(primary_first_per_place > 0, primary_first_per_place, NA_real_)))))]
inds <- c(near_sec_out = "Nearest secondary Outstanding", near_pri_out = "Nearest primary Outstanding",
          outst_sec10 = "Outstanding secondary within 10 min", outst_pri10 = "Outstanding primary within 10 min",
          pop_sec = "Secondary popularity (per SD)", pop_pri = "Primary popularity (per SD)")
rep <- rbindlist(lapply(names(inds), function(v) {
  d <- R[!is.na(get(v))]
  mp <- feols(as.formula(sprintf("lp ~ %s + lfa + i(rooms) + eff | uprn + lad26cd^ym", v)), data = d, cluster = ~uprn, lean = TRUE)
  ml <- feols(as.formula(sprintf("lp ~ %s + %s | ym + lsoa21cd", v, rung["C3"])), data = d, cluster = ~msoa21cd, lean = TRUE)
  # how many repeat-sold properties see the measure change between their sales
  ch <- d[, .(chg = uniqueN(get(v)) > 1), by = uprn][, sum(chg)]
  rbind(data.table(ind = v, design = "Same property (repeat sales)", b = coef(mp)[[v]], se = se(mp)[[v]], n = nobs(mp), changers = ch),
        data.table(ind = v, design = "Same LSOA (same sales)", b = coef(ml)[[v]], se = se(ml)[[v]], n = nobs(ml), changers = ch))
}))
rep[, `:=`(label = inds[ind], pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]

saveRDS(list(ladder = lad, repeat_sales = rep, lab_rung = lab_rung, n = nrow(S), n_att8 = nrow(A), n_repeat = nrow(R),
             props_repeat = uniqueN(R$uprn), built = Sys.Date()), here::here("data", "epc_controls.rds"))
options(width = 170)
print(lad[, .(rung, fe, term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), r2 = round(r2, 3), n)])
print(rep[, .(label, design, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), n, changers)])
cat("repeat-sold properties", uniqueN(R$uprn), "sales", nrow(R), "\n")

P <- copy(lad)[, `:=`(rung = factor(lab_rung[rung], lab_rung), fe = fifelse(fe == "ym", "Month effects only", "Month and LSOA effects"),
                      term = factor(term, c("att8_z", "va_z", "near_sec_out"), c("Attainment 8 (per SD)", "Value added (per SD)", "Nearest secondary Outstanding")))]
g <- ggplot(P, aes(pct, rung, colour = fe)) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.3) +
  facet_wrap(~term, scales = "free_x") + scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c("Month effects only" = "#c9a2f5", "Month and LSOA effects" = "#361a54"), name = NULL) +
  labs(x = "Price premium (%), 95% interval", y = NULL, title = "Adding the home's characteristics from its EPC, step by step",
       subtitle = sprintf("Nearest state secondary; %s sales, Sept 2018 to Sept 2026", format(nrow(S), big.mark = ",")),
       caption = "Price Paid linked to EPCs (Chi; Beech); walking routes (school_active_travel); DfE Attainment 8; Value Added v2.1; Ofsted") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave(here::here("figures", "epc_ladder.png"), g, width = 11, height = 5, dpi = 150)
