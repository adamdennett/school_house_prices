# 12_three_groups.R - Whose results do buyers pay for? All, disadvantaged and non-disadvantaged pupils
# ---------------------------------------------------------------------------
# The nearest state secondary's four-year mean Attainment 8 and Value Added v2.1 (shrunk) for
# all pupils, disadvantaged pupils and non-disadvantaged pupils (school_value_added app data and
# va_v2_schools.csv). Each standardised by its school-level SD. Three designs as R/06: raw | ym;
# same kind of home | ym + district; same home, same LSOA | ym + LSOA; log price, hedonic
# controls, standard errors clustered by MSOA. Sample: sales whose nearest secondary has all six
# measures, so the three groups are compared on the same sales.
#   g_all, g_dis, g_nd  each group's Attainment 8 and value added together, one group at a time
#   joint               disadvantaged and non-disadvantaged Attainment 8 together, plus the
#                       school's free school meals share (DfE applications and offers, mean of entry
#                       2022 to 2026; per 10 percentage points): results for "a child like mine"
#                       against the intake
# Output: data/three_groups.rds, figures/three_groups.png      Rscript R/12_three_groups.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
num <- function(x) suppressWarnings(as.numeric(x))
sva <- normalizePath(file.path(dirname(here::here()), "school_value_added"), winslash = "/")
S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"

ad <- as.data.table(readRDS(file.path(sva, "app", "data", "app_data.rds"))$four)
a8 <- dcast(ad[group %in% c("all", "disadvantaged", "non_disadvantaged")], URN ~ group, value.var = "att8")
setnames(a8, c("near_secondary_urn", "a8_all", "a8_dis", "a8_nd"))
A <- fread(here::here("data-raw", "admissions", "supporting-files", "AppsandOffers_2026_SchoolLevel29062026.csv"),
           select = c("time_period", "school_phase", "school_urn", "FSM_eligible_percent"), colClasses = "character")
fsm <- A[school_phase == "Secondary" & as.integer(substr(time_period, 1, 4)) >= 2022,
         .(fsm = mean(num(FSM_eligible_percent), na.rm = TRUE)), by = .(near_secondary_urn = as.integer(school_urn))]
S <- merge(S, a8, by = "near_secondary_urn"); S <- merge(S, fsm, by = "near_secondary_urn", all.x = TRUE)
S <- S[!is.na(a8_all) & !is.na(a8_dis) & !is.na(a8_nd) & !is.na(all_va_shrunk) & !is.na(disadvantaged_va_shrunk) & !is.na(non_disadvantaged_va_shrunk) & !is.na(fsm)]
sch <- unique(S[, .(near_secondary_urn, a8_all, a8_dis, a8_nd, va_all = all_va_shrunk, va_dis = disadvantaged_va_shrunk, va_nd = non_disadvantaged_va_shrunk)])
zs <- function(x, ref) (x - mean(ref)) / sd(ref)
S[, `:=`(a8_all_z = zs(a8_all, sch$a8_all), a8_dis_z = zs(a8_dis, sch$a8_dis), a8_nd_z = zs(a8_nd, sch$a8_nd),
         va_all_z = zs(all_va_shrunk, sch$va_all), va_dis_z = zs(disadvantaged_va_shrunk, sch$va_dis), va_nd_z = zs(non_disadvantaged_va_shrunk, sch$va_nd),
         fsm10 = fsm / 10)]
designs <- function(rhs) c(raw = sprintf("lp ~ %s | ym", rhs), hedonic = sprintf("lp ~ %s + %s | ym + lad26cd", rhs, ctrl),
                           neighbourhood = sprintf("lp ~ %s + %s | ym + lsoa21cd", rhs, ctrl))
fit <- function(rhs, keep) rbindlist(lapply(designs(rhs), function(f) {
  m <- feols(as.formula(f), data = S, cluster = ~msoa21cd, lean = TRUE); ct <- coeftable(m)[keep, , drop = FALSE]
  data.table(term = keep, b = ct[, 1], se = ct[, 2], n = nobs(m))
}), idcol = "model")
res <- list(
  g_all = fit("a8_all_z + va_all_z", c("a8_all_z", "va_all_z"))[, group := "All pupils"],
  g_dis = fit("a8_dis_z + va_dis_z", c("a8_dis_z", "va_dis_z"))[, group := "Disadvantaged pupils"],
  g_nd = fit("a8_nd_z + va_nd_z", c("a8_nd_z", "va_nd_z"))[, group := "Non-disadvantaged pupils"],
  joint = fit("a8_dis_z + a8_nd_z + fsm10", c("a8_dis_z", "a8_nd_z", "fsm10")),
  n = nrow(S), schools = nrow(sch), sd = sapply(sch[, -1], sd), built = Sys.Date())
pc <- function(d) d[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
for (k in c("g_all", "g_dis", "g_nd", "joint")) pc(res[[k]])
saveRDS(res, here::here("data", "three_groups.rds"))
options(width = 160)
G3 <- rbind(res$g_all, res$g_dis, res$g_nd)
print(G3[, .(group, model, term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), n)])
print(res$joint[, .(model, term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2))]); print(round(res$sd, 2)); cat("schools", res$schools, "\n")

G3[, `:=`(measure = fifelse(grepl("^a8", term), "Attainment 8", "Value added"),
          model = factor(model, c("raw", "hedonic", "neighbourhood"), c("Raw", "Same kind of home", "Same home, same neighbourhood")),
          group = factor(group, rev(c("All pupils", "Disadvantaged pupils", "Non-disadvantaged pupils"))))]
g <- ggplot(G3, aes(pct, group, colour = model)) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge2(width = 0.6, reverse = TRUE), size = 0.35) +
  facet_wrap(~measure, scales = "free_x") +
  scale_colour_manual(values = c("Raw" = "#c9a2f5", "Same kind of home" = "#993bff", "Same home, same neighbourhood" = "#361a54"), name = NULL) +
  labs(x = "Price premium (%) per school-level standard deviation, 95% interval", y = NULL,
       title = "Whose results do buyers pay for?",
       subtitle = sprintf("Nearest state secondary's Attainment 8 and value added for each pupil group, entered together; %s sales", format(res$n, big.mark = ",")),
       caption = "Price Paid linked to EPCs (Chi; Beech); walking routes (school_active_travel); DfE Attainment 8 (2021/22 to 2024/25); Value Added v2.1") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave(here::here("figures", "three_groups.png"), g, width = 10, height = 5, dpi = 150)
