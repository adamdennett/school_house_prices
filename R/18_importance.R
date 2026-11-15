# 18_importance.R - What matters most in the price of a home? Shapley (LMG) decomposition of R²
# ---------------------------------------------------------------------------
# The same method as the Attainment 8 variance decomposition (school_attainment_tool,
# output/variance_decomposition.qmd): each block of variables gets its average contribution to R²
# over every order in which the blocks could enter the model (Shapley value; Lindeman, Merenda and
# Gold 1980; Gromping 2007), computed over the full subset lattice. Shared variance is split, not
# double counted, and the shares add up to the full model's R². Reported beside two simpler
# measures: R² alone (the block on its own) and R² added last (what it adds to all the others).
#
# Outcome: log price. Sales: as R/06 (category A, September 2018 to September 2026, EPC-matched,
# complete attributes), whose nearest state secondary has Attainment 8 and value added for all,
# disadvantaged and non-disadvantaged pupils and a free school meals share. A random sample is
# used, as 2^k models are fitted for each decomposition (seeded; a second sample checks stability).
#
# Blocks (fixed effects in brackets):
#   size        log floor area, rooms
#   type        property type, built form
#   age         construction age band
#   energy      EPC energy efficiency score
#   tenure      freehold or leasehold, new build
#   time        [year-month of sale]
#   district    [local authority district]                                   (design A)
#   deprivation neighbourhood deprivation: IMD 2025 decile of the LSOA         (design A)
#   lsoa        [LSOA: the small neighbourhood itself]                         (design B)
#   ofsted      Ofsted grade of the nearest state primary and secondary (at the date of sale)
#   results     nearest secondary's four-year Attainment 8 and Value Added v2.1 (standardised),
#               for all pupils; repeated with disadvantaged and non-disadvantaged pupils' figures
#   intake      nearest secondary's free school meals share (DfE applications and offers)
# Design A: location as district + deprivation (11 blocks, 2,048 models). Design B: location as
# LSOA (10 blocks, 1,024 models), which absorbs everything fixed about the neighbourhood.
# Output: data/importance.rds, figures/importance_*.png      Rscript R/18_importance.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest); library(ggplot2); library(parallel) })
num <- function(x) suppressWarnings(as.numeric(x))
sva <- normalizePath(file.path(here::here(), "..", "school_value_added"), winslash = "/")
S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd) & !is.na(imd25ind)]
S[, `:=`(gs = fifelse(is.na(near_secondary_grade), "none", as.character(near_secondary_grade)),
         gp = fifelse(is.na(near_primary_grade), "none", as.character(near_primary_grade)))]
ad <- as.data.table(readRDS(file.path(sva, "app", "data", "app_data.rds"))$four)
a8 <- dcast(ad[group %in% c("all", "disadvantaged", "non_disadvantaged")], URN ~ group, value.var = "att8")
setnames(a8, c("near_secondary_urn", "a8_all", "a8_dis", "a8_nd"))
A <- fread(here::here("data-raw", "admissions", "supporting-files", "AppsandOffers_2026_SchoolLevel29062026.csv"),
           select = c("time_period", "school_phase", "school_urn", "FSM_eligible_percent"), colClasses = "character")
fsm <- A[school_phase == "Secondary" & as.integer(substr(time_period, 1, 4)) >= 2022,
         .(fsm = mean(num(FSM_eligible_percent), na.rm = TRUE)), by = .(near_secondary_urn = as.integer(school_urn))]
S <- merge(S, a8, by = "near_secondary_urn"); S <- merge(S, fsm, by = "near_secondary_urn")
S <- S[!is.na(a8_all) & !is.na(a8_dis) & !is.na(a8_nd) & !is.na(all_va_shrunk) & !is.na(disadvantaged_va_shrunk) & !is.na(non_disadvantaged_va_shrunk) & is.finite(fsm)]
sch <- unique(S[, .(near_secondary_urn, a8_all, a8_dis, a8_nd, va_all = all_va_shrunk, va_dis = disadvantaged_va_shrunk, va_nd = non_disadvantaged_va_shrunk)])
zs <- function(x, ref) (x - mean(ref)) / sd(ref)
S[, `:=`(a8_all_z = zs(a8_all, sch$a8_all), a8_dis_z = zs(a8_dis, sch$a8_dis), a8_nd_z = zs(a8_nd, sch$a8_nd),
         va_all_z = zs(all_va_shrunk, sch$va_all), va_dis_z = zs(disadvantaged_va_shrunk, sch$va_dis), va_nd_z = zs(non_disadvantaged_va_shrunk, sch$va_nd), fsm10 = fsm / 10)]
S[, imd_dec := cut(imd25ind, quantile(imd25ind, 0:10 / 10), include.lowest = TRUE, labels = FALSE)]   # decile of LSOAs' index, as sold
N_ALL <- nrow(S)
keep <- c("lp", "lfa", "rooms", "propertytype", "built_form", "construction_age_band", "eff", "duration", "oldnew", "ym", "lad26cd", "lsoa21cd", "imd_dec",
          "gs", "gp", "a8_all_z", "a8_dis_z", "a8_nd_z", "va_all_z", "va_dis_z", "va_nd_z", "fsm10")
set.seed(18); s1 <- S[sample(.N, 250000), ..keep]; s2 <- S[sample(.N, 250000), ..keep]
set.seed(19); sB <- S[sample(.N, 1000000), ..keep]
rm(S, A, ad); gc()

blocks <- function(design, grp) {
  b <- list(size = list(x = "lfa + i(rooms)"), type = list(x = "i(propertytype) + i(built_form)"), age = list(x = "i(construction_age_band)"),
            energy = list(x = "eff"), tenure = list(x = "i(duration) + i(oldnew)"), time = list(fe = "ym"))
  if (design == "A") b <- c(b, list(district = list(fe = "lad26cd"), deprivation = list(x = "i(imd_dec)")))
  if (design == "B") b <- c(b, list(lsoa = list(fe = "lsoa21cd")))
  c(b, list(ofsted = list(x = "i(gs) + i(gp)"), results = list(x = sprintf("a8_%s_z + va_%s_z", grp, grp)), intake = list(x = "fsm10")))
}
form <- function(b, mask) {
  sel <- b[bitwAnd(mask, 2^(seq_along(b) - 1)) > 0]
  x <- unlist(lapply(sel, `[[`, "x")); fe <- unlist(lapply(sel, `[[`, "fe"))
  sprintf("lp ~ %s%s", if (length(x)) paste(x, collapse = " + ") else "1", if (length(fe)) paste(" |", paste(fe, collapse = " + ")) else "")
}
jobs <- list(A_all = list(d = "A", g = "all", s = "s1"), A_dis = list(d = "A", g = "dis", s = "s1"), A_nd = list(d = "A", g = "nd", s = "s1"),
             A_all_s2 = list(d = "A", g = "all", s = "s2"), B_all = list(d = "B", g = "all", s = "sB"), B_dis = list(d = "B", g = "dis", s = "sB"), B_nd = list(d = "B", g = "nd", s = "sB"))
tasks <- rbindlist(lapply(names(jobs), function(j) { b <- blocks(jobs[[j]]$d, jobs[[j]]$g); data.table(job = j, sample = jobs[[j]]$s, mask = 1:(2^length(b) - 1),
  f = vapply(1:(2^length(b) - 1), function(m) form(b, m), "")) }))
tasks <- unique(tasks[, .(sample, f)])[tasks, on = .(sample, f)]                   # same formula on the same sample is fitted once
uf <- unique(tasks[, .(sample, f)])
message(nrow(tasks), " subset models, ", nrow(uf), " distinct fits")
cl <- makeCluster(8)
clusterExport(cl, c("s1", "s2", "sB"))
clusterEvalQ(cl, { suppressPackageStartupMessages(library(fixest)); setFixest_nthreads(1); NULL })
t0 <- Sys.time()
uf[, r2 := unlist(parLapplyLB(cl, seq_len(.N), function(i, smp, ff) { d <- get(smp[i]); m <- feols(as.formula(ff[i]), data = d, warn = FALSE, notes = FALSE); unname(r2(m, "r2")) }, smp = sample, ff = f))]
stopCluster(cl); message("fits done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
tasks <- uf[tasks, on = .(sample, f)]

shapley <- function(jn) {
  b <- blocks(jobs[[jn]]$d, jobs[[jn]]$g); k <- length(b)
  r2m <- c(0, tasks[job == jn][order(mask), r2])                                # index = mask + 1
  phi <- vapply(seq_len(k), function(i) { bit <- 2^(i - 1); tot <- 0
    for (m in 0:(2^k - 1)) if (bitwAnd(m, bit) == 0) { s <- sum(bitwAnd(m, 2^(seq_len(k) - 1)) > 0)
      tot <- tot + exp(lfactorial(s) + lfactorial(k - s - 1) - lfactorial(k)) * (r2m[bitwOr(m, bit) + 1] - r2m[m + 1]) }
    tot }, 0)
  full <- r2m[2^k]
  data.table(job = jn, design = jobs[[jn]]$d, group = jobs[[jn]]$g, sample = jobs[[jn]]$s, block = names(b),
             alone = r2m[2^(seq_len(k) - 1) + 1], shapley = phi, last = full - r2m[(2^k - 1 - 2^(seq_len(k) - 1)) + 1], full_r2 = full)
}
SH <- rbindlist(lapply(names(jobs), shapley))
SH[, share := shapley / full_r2]
labs_b <- c(size = "Size (floor area, rooms)", type = "Type and built form", age = "Age of building", energy = "Energy efficiency", tenure = "Tenure and new build",
            time = "Month of sale", district = "Local authority district", deprivation = "Neighbourhood deprivation (IMD)", lsoa = "Neighbourhood (LSOA)",
            ofsted = "Ofsted grades of nearest schools", results = "Nearest secondary's results (Attainment 8, VA)", intake = "Nearest secondary's intake (FSM share)")
SH[, label := labs_b[block]]
saveRDS(list(SH = SH, n_sales = N_ALL, n_sample = c(A = nrow(s1), B = nrow(sB)), labels = labs_b, built = Sys.Date()), here::here("data", "importance.rds"))
options(width = 170)
print(SH[job %in% c("A_all", "A_all_s2", "B_all"), .(job, block, alone = round(alone, 3), shapley = round(shapley, 3), share = round(100 * share, 1), last = round(last, 4), full_r2 = round(full_r2, 3))])
print(dcast(SH[block %in% c("ofsted", "results", "intake")], design + block ~ group, value.var = "shapley")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])

# ---- figures --------------------------------------------------------------------------------------
pal <- c(`R² alone` = "#c9a2f5", `Shapley (LMG) share` = "#361a54", `Added last` = "#993bff")
plot_design <- function(jn, file, ttl) {
  d <- SH[job == jn]; ord <- d[order(shapley), label]
  L <- melt(d[, .(label, `R² alone` = alone, `Shapley (LMG) share` = shapley, `Added last` = last)], id.vars = "label", variable.name = "measure")
  L[, `:=`(label = factor(label, ord), measure = factor(measure, names(pal)))]
  g <- ggplot(L, aes(value, label, fill = measure)) + geom_col(position = position_dodge(width = 0.78), width = 0.72) +
    geom_text(data = L[measure == "Shapley (LMG) share"], aes(label = sprintf("%.1f%%", 100 * value / d$full_r2[1])), position = position_dodge(width = 0.78), hjust = -0.15, size = 2.8, colour = "#361a54") +
    scale_fill_manual(values = pal, name = NULL) + scale_x_continuous(labels = function(x) sprintf("%.2f", x), expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Share of the variance in log price (R²)", y = NULL, title = ttl,
         subtitle = sprintf("Full model R² %.3f; labels: Shapley share of the explained variance. %s sales sampled", d$full_r2[1], format(nrow(if (d$design[1] == "A") s1 else sB), big.mark = ",")),
         caption = "Price Paid linked to EPCs (Chi; Beech); school measures: DfE, Ofsted, Value Added v2.1; IMD 2025") +
    theme_minimal(base_size = 10) + theme(legend.position = "top", plot.title.position = "plot", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
  ggsave(here::here("figures", file), g, width = 9.5, height = 5.4, dpi = 150)
}
plot_design("A_all", "importance_district.png", "What matters most in the price of a home: location as district and deprivation")
plot_design("B_all", "importance_lsoa.png", "What matters most in the price of a home: location as the neighbourhood (LSOA)")
