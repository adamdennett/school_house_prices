# 15_bh_catchments.R - Brighton and Hove: real secondary catchments, boundary design and the 2025 redraw
# ---------------------------------------------------------------------------
# Brighton and Hove gives catchment-area priority for secondary places and, since 2008, allocates
# by random tie-break within a catchment rather than by distance. Its catchments are published
# (Brighton & Hove City Council, OGL v3); copies are in the sibling bh-school-system/data
# (that folder is named bh_school_system on some machines):
#   catchments_pre2024.geojson  the map in force for entry up to September 2025
#   catchments_current.geojson  "Option Z", agreed by the council in early 2025, for entry from September 2026
# Sales before 1 March 2025 are placed on the old map, later sales on the new one.
#
# Each catchment's schools (community and academy secondaries; the two faith schools, Cardinal
# Newman and King's, have no catchment):
#   Patcham: Patcham High | Stringer/Varndean: Dorothy Stringer, Varndean | BACA: Brighton Aldridge
#   Community Academy | Longhill: Longhill High | Blatchington/Hove Park: Blatchington Mill, Hove Park |
#   Portslade: Portslade Aldridge Community Academy
# Catchment quality: pupil-weighted mean of its schools' four-year Attainment 8 (2021/22 to 2024/25),
# free school meals share and value added (all pupils), from ../school_value_added/app/data/app_data.rds.
#
# A. Boundary design (Black 1999). Shared boundaries between adjacent catchments on the old map,
#    sampled every 50 m and cut into 500 m segments. Each sale (postcode centroid, ONSPD) is matched
#    to its nearest boundary point: segment, distance, and the catchment on the other side.
#    "Better side" = the side whose catchment has the higher Attainment 8. Sales within 300, 500 and
#    800 m of a boundary; log price on the better side (and on the Attainment 8 gap), with and
#    without the hedonic controls of R/06, | segment + year-month, clustered by segment.
# B. Catchment premiums. Each catchment against Stringer/Varndean: raw, like-for-like with
#    year-month effects (only 25 LSOAs straddle a catchment boundary, too few for LSOA effects).
# C. The 2025 redraw. Postcodes whose catchment changed between the maps, to a catchment with
#    higher or lower Attainment 8, against postcodes that kept theirs; sales before and after
#    1 March 2025, with OA and year-month fixed effects (difference in differences). Few sales after
#    the change, so a first look only.
# Inputs: data/sales_panel.parquet (R/05). Output: data/bh_catchments.rds, figures/bh_catchments_map.png,
#   figures/bh_boundary.png       Rscript R/15_bh_catchments.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(sf); library(fixest); library(ggplot2) })
sf_use_s2(FALSE); setFixest_nthreads(8)
# The sibling repo is checked out as bh-school-system on some machines and
# bh_school_system on others, so take whichever is actually there.
bhs_root <- Filter(dir.exists, file.path(dirname(here::here()),
                                         c("bh-school-system", "bh_school_system")))[1]
if (is.na(bhs_root)) stop("Neither bh-school-system nor bh_school_system is a sibling of ", here::here())
bhs <- normalizePath(file.path(bhs_root, "data"), winslash = "/")
sva <- normalizePath(file.path(dirname(here::here()), "school_value_added"), winslash = "/")

# ---- catchments and their schools ---------------------------------------------------------------
std <- c("Patcham HighSchool" = "Patcham", "StringerVarndean" = "Stringer/Varndean", "BrightonAldridge" = "BACA", "Longhill" = "Longhill",
         "BlatchingtonHove" = "Blatchington/Hove Park", "Portslade" = "Portslade",
         "VarndeanStringer" = "Stringer/Varndean", "Patcham" = "Patcham", "BACA" = "BACA", "HoveBlatchington" = "Blatchington/Hove Park", "PACA" = "Portslade")
old <- st_read(file.path(bhs, "catchments_pre2024.geojson"), quiet = TRUE); old$catch <- std[old$AreaName]
new <- st_read(file.path(bhs, "catchments_current.geojson"), quiet = TRUE); new$catch <- std[new$catchment]
old <- st_make_valid(st_transform(old["catch"], 27700)); new <- st_make_valid(st_transform(new["catch"], 27700))
stopifnot(!anyNA(old$catch), !anyNA(new$catch))
urns <- data.table(catch = c("Patcham", "Stringer/Varndean", "Stringer/Varndean", "BACA", "Longhill", "Blatchington/Hove Park", "Blatchington/Hove Park", "Portslade"),
                   URN = c(114608L, 114580L, 114579L, 136164L, 114581L, 114606L, 114607L, 137063L))
ad <- readRDS(file.path(sva, "app", "data", "app_data.rds"))
sch <- merge(as.data.table(ad$schools)[, .(URN, name, pupils, fsm)], as.data.table(ad$four)[group == "all", .(URN, att8, va)], by = "URN")
CQ <- merge(urns, sch, by = "URN")[, .(schools = paste(name, collapse = "; "), att8 = weighted.mean(att8, pupils), fsm = weighted.mean(fsm, pupils),
                                       va = weighted.mean(va, pupils), pupils = sum(pupils)), by = catch][order(-att8)]
print(CQ)

# ---- sales in the catchments ---------------------------------------------------------------------
S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
bb <- st_bbox(st_union(old, new))
S <- S[east1m >= bb["xmin"] & east1m <= bb["xmax"] & north1m >= bb["ymin"] & north1m <= bb["ymax"]]
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff)]
pts <- st_as_sf(S[, .(id = .I, east1m, north1m)], coords = c("east1m", "north1m"), crs = 27700)
S[, catch_old := old$catch[as.integer(st_intersects(pts, old, sparse = TRUE) |> sapply(function(z) if (length(z)) z[1] else NA))]]
S[, catch_new := new$catch[as.integer(st_intersects(pts, new, sparse = TRUE) |> sapply(function(z) if (length(z)) z[1] else NA))]]
S <- S[!is.na(catch_old) | !is.na(catch_new)]
S[, catch := fifelse(date < as.IDate("2025-03-01"), catch_old, catch_new)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"
message(nrow(S), " sales in the catchment area; ", S[!is.na(catch), .N], " placed in a catchment")

# ---- A. boundary design on the old map --------------------------------------------------------------
pairs <- CJ(a = seq_len(nrow(old)), b = seq_len(nrow(old)))[a < b]
B <- rbindlist(lapply(seq_len(nrow(pairs)), function(k) {
  a <- old[pairs$a[k], ]; b <- old[pairs$b[k], ]
  ln <- tryCatch(st_intersection(st_boundary(st_geometry(a)), st_buffer(st_geometry(b), 10)), error = function(e) NULL)   # 10 m: the published polygons were digitised with small gaps between neighbours (30 km shared at 10 m, 15 km at 1 m)
  if (is.null(ln) || !length(ln)) return(NULL)
  ln <- st_collection_extract(ln, "LINESTRING"); if (!length(ln)) return(NULL)
  u <- st_union(ln); if (inherits(u, "sfc_MULTILINESTRING")) u <- st_line_merge(u)
  ln <- st_cast(u, "LINESTRING"); len <- sum(as.numeric(st_length(ln))); if (len < 100) return(NULL)
  p <- st_cast(st_line_sample(ln, density = 1 / 50), "POINT")
  xy <- st_coordinates(p)
  data.table(ca = a$catch, cb = b$catch, x = xy[, 1], y = xy[, 2], seq = seq_len(nrow(xy)))
}))
B <- B[!is.na(x) & !is.na(y)][, seq := seq_len(.N), by = .(ca, cb)]   # pieces shorter than 50 m give empty samples
B[, `:=`(pair = paste(ca, cb, sep = " | "), seg = paste(ca, cb, ceiling(seq / 10), sep = " | "))]   # 10 points x 50 m = 500 m segments
bp <- st_as_sf(B, coords = c("x", "y"), crs = 27700)
O <- S[date < as.IDate("2025-03-01") & !is.na(catch_old)]
op <- st_as_sf(O[, .(east1m, north1m)], coords = c("east1m", "north1m"), crs = 27700)
nn <- st_nearest_feature(op, bp)
O[, `:=`(dist = as.numeric(st_distance(op, bp[nn, ], by_element = TRUE)), seg = B$seg[nn], ca = B$ca[nn], cb = B$cb[nn])]
O[, other := fifelse(catch_old == ca, cb, ca)]
O <- merge(O, CQ[, .(catch_old = catch, att8_own = att8, fsm_own = fsm, va_own = va)], by = "catch_old")
O <- merge(O, CQ[, .(other = catch, att8_oth = att8, fsm_oth = fsm, va_oth = va)], by = "other")
O[, `:=`(better = as.integer(att8_own > att8_oth), gap = att8_own - att8_oth, gap10 = (att8_own - att8_oth) / 10)]
bd <- function(d, rhs, ctl = TRUE) {
  D <- O[dist <= d]
  f <- sprintf("lp ~ %s%s | seg + ym", rhs, if (ctl) paste(" +", ctrl) else "")
  m <- feols(as.formula(f), data = D, cluster = ~seg, lean = TRUE)
  ct <- coeftable(m)[strsplit(rhs, " \\+ ")[[1]], , drop = FALSE]
  data.table(dist = d, term = rownames(ct), controls = ctl, b = ct[, 1], se = ct[, 2], n = nobs(m), segs = uniqueN(D$seg))
}
A <- rbindlist(c(lapply(c(300, 500, 800), function(d) bd(d, "better", FALSE)), lapply(c(300, 500, 800), function(d) bd(d, "better")),
                 lapply(c(300, 500, 800), function(d) bd(d, "gap10"))))
A[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
# by boundary pair (500 m, with controls), where enough sales
Pb <- rbindlist(lapply(unique(O$pairs <- paste(pmin(O$ca, O$cb), pmax(O$ca, O$cb), sep = " | ")), function(p) {
  D <- O[dist <= 500 & pairs == p]; if (D[, uniqueN(better)] < 2 || nrow(D) < 200) return(NULL)
  m <- tryCatch(feols(as.formula(sprintf("lp ~ better + %s | seg + ym", ctrl)), data = D, cluster = ~seg, lean = TRUE), error = function(e) NULL)
  if (is.null(m) || !"better" %in% names(coef(m)) || !is.finite(se(m)[["better"]])) return(NULL)
  data.table(pair = p, b = coef(m)[["better"]], se = se(m)[["better"]], n = nobs(m), gap = D[better == 1, gap[1]])
}))
Pb[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]

# ---- B. catchment premiums ------------------------------------------------------------------------
C <- S[!is.na(catch)][, catch := relevel(factor(catch), ref = "Stringer/Varndean")]
cm <- list(raw = feols(lp ~ i(catch, ref = "Stringer/Varndean") | ym, data = C, cluster = ~lsoa21cd),
           hedonic = feols(as.formula(sprintf("lp ~ i(catch, ref = 'Stringer/Varndean') + %s | ym", ctrl)), data = C, cluster = ~lsoa21cd),
           district = feols(as.formula(sprintf("lp ~ i(catch, ref = 'Stringer/Varndean') + %s | ym + lad26cd", ctrl)), data = C, cluster = ~lsoa21cd))
CP <- rbindlist(lapply(names(cm), function(k) { ct <- as.data.table(coeftable(cm[[k]]), keep.rownames = "term")[grepl("^catch::", term)]
  setnames(ct, 2:3, c("b", "se")); ct[, .(model = k, catch = sub("catch::", "", term), b, se, n = nobs(cm[[k]]))] }))
CP[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
straddle <- C[, uniqueN(catch), by = lsoa21cd][V1 > 1, .N]

# ---- C. the 2025 redraw ---------------------------------------------------------------------------------
R <- S[!is.na(catch_old) & !is.na(catch_new)]
R <- merge(R, CQ[, .(catch_old = catch, a_old = att8)], by = "catch_old"); R <- merge(R, CQ[, .(catch_new = catch, a_new = att8)], by = "catch_new")
R[, `:=`(post = as.integer(date >= as.IDate("2025-03-01")), move = fifelse(catch_old == catch_new, "Kept catchment", fifelse(a_new > a_old, "Moved to higher-results catchment", "Moved to lower-results catchment")))]
R[, `:=`(up = as.integer(move == "Moved to higher-results catchment"), down = as.integer(move == "Moved to lower-results catchment"))]
mv <- feols(as.formula(sprintf("lp ~ up:post + down:post + %s | oa21cd + ym", ctrl)), data = R, cluster = ~oa21cd)
DID <- as.data.table(coeftable(mv), keep.rownames = "term")[grepl("post", term)]; setnames(DID, 2:3, c("b", "se"))
DID[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
moves <- R[, .(sales = .N, post = sum(post), postcodes = uniqueN(postcode)), by = move]
# local comparison: kept-catchment sales within 1 km of a moved sale, and the old and new catchment pair of the move
mvp <- st_as_sf(R[move != "Kept catchment", .(east1m, north1m)], coords = c("east1m", "north1m"), crs = 27700)
kp <- st_as_sf(R[, .(east1m, north1m)], coords = c("east1m", "north1m"), crs = 27700)
R[, near_move := lengths(st_is_within_distance(kp, st_union(mvp), 1000)) > 0]
RL <- R[near_move == TRUE]
mvl <- feols(as.formula(sprintf("lp ~ up:post + down:post + %s | oa21cd + ym", ctrl)), data = RL, cluster = ~oa21cd)
DIDL <- as.data.table(coeftable(mvl), keep.rownames = "term")[grepl("post", term)]; setnames(DIDL, 2:3, c("b", "se"))
DIDL[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
# dynamics for the larger group (moved down): half-years relative to the first half of 2025, local comparison
RL[, hy := (as.integer(format(date, "%Y")) - 2025L) * 2L + (as.integer(format(date, "%m")) >= 7L)]   # 2025H1 = 0
RL[, hyb := pmax(hy, -6L)]
dyn <- feols(as.formula(sprintf("lp ~ i(hyb, down, ref = -1) + %s | oa21cd + ym", ctrl)), data = RL[up == 0], cluster = ~oa21cd)
DYN <- as.data.table(coeftable(dyn), keep.rownames = "term")[grepl("^hyb::", term)]; setnames(DYN, 2:3, c("b", "se"))
DYN[, `:=`(hy = as.integer(sub("hyb::(-?[0-9]+):down", "\\1", term)), pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
moved_pairs <- R[move != "Kept catchment", .N, by = .(catch_old, catch_new, move)][order(-N)]
# dated from the announcement: the council published the redraw options on 2 October 2024 (Brighton and Hove News, 2 Oct 2024)
RL[, post_ann := as.integer(date >= as.IDate("2024-10-01"))]
mva <- feols(as.formula(sprintf("lp ~ up:post_ann + down:post_ann + %s | oa21cd + ym", ctrl)), data = RL, cluster = ~oa21cd)
DIDA <- as.data.table(coeftable(mva), keep.rownames = "term")[grepl("post", term)]; setnames(DIDA, 2:3, c("b", "se"))
DIDA[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]

res <- list(CQ = CQ, A = A, by_pair = Pb, catch_prem = CP, straddle_lsoas = straddle, did = DID, did_local = DIDL, did_announce = DIDA, did_dyn = DYN,
            moved_pairs = moved_pairs, n_local = nrow(RL), moves = moves,
            n_sales = nrow(S), n_old = nrow(O), within = O[, .(d300 = sum(dist <= 300), d500 = sum(dist <= 500), d800 = sum(dist <= 800))],
            boundary_km = B[, .(km = .N * 50 / 1000), by = pair], built = Sys.Date())
saveRDS(res, here::here("data", "bh_catchments.rds"))
options(width = 160)
print(A[, .(dist, term, controls, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), n, segs)])
print(Pb[, .(pair, gap = round(gap, 1), pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), n)])
print(CP[, .(model, catch, pct = round(pct, 1), lo = round(lo, 1), hi = round(hi, 1))]); cat("LSOAs straddling a catchment boundary:", straddle, "\n")
print(DID[, .(term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2))]); print(DIDL[, .(term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2))]); print(DIDA[, .(term, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2))]); print(DYN[, .(hy, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2))]); print(moved_pairs); print(moves); print(res$within); print(res$boundary_km)

# ---- figures ---------------------------------------------------------------------------------------
pal6 <- setNames(c("#361a54", "#5b2a99", "#993bff", "#c9a2f5", "#e8a33d", "#6b6680"), CQ$catch)
bl <- st_as_sf(B, coords = c("x", "y"), crs = 27700)
changed <- st_as_sf(R[catch_old != catch_new, .(east1m, north1m, move)], coords = c("east1m", "north1m"), crs = 27700)
g1 <- ggplot() + geom_sf(data = old, aes(fill = catch), colour = "white", linewidth = 0.4, alpha = 0.55) +
  geom_sf(data = new, fill = NA, colour = "#1d1b20", linewidth = 0.35, linetype = 2) +
  geom_sf(data = changed, aes(shape = move), size = 0.5, colour = "#1d1b20", alpha = 0.6) +
  scale_fill_manual(values = pal6, name = "Catchment (old map)", labels = function(x) paste0(x, " (Att8 ", formatC(CQ$att8[match(x, CQ$catch)], format = "f", digits = 1), ")")) +
  scale_shape_manual(values = c(16, 4), name = "Sales whose catchment\nchanged in 2025") +
  labs(title = "Brighton and Hove secondary catchments", subtitle = "Fill: map in force for entry to September 2025. Dashed: the map from September 2026 entry (Option Z).",
       caption = "Catchments: Brighton & Hove City Council (OGL v3). Attainment 8: DfE, four-year mean, pupil-weighted across each catchment's schools.") +
  theme_void(base_size = 10) + theme(legend.position = "right", plot.title.position = "plot")
ggsave(here::here("figures", "bh_catchments_map.png"), g1, width = 10, height = 6.5, dpi = 150)
O[, band := cut(fifelse(better == 1, dist, -dist), seq(-800, 800, 100))]
bins <- feols(as.formula(sprintf("lp ~ %s | seg + ym", ctrl)), data = O[dist <= 800], lean = FALSE)
O[dist <= 800, res := resid(bins)]
BIN <- O[dist <= 800, .(r = mean(res), n = .N, se = sd(res) / sqrt(.N)), by = band][, mid := as.numeric(sub("\\((-?[0-9]+),.*", "\\1", band)) + 50]
g2 <- ggplot(BIN, aes(mid, 100 * r)) + geom_vline(xintercept = 0, colour = "#6b6680", linetype = 2) + geom_hline(yintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(ymin = 100 * (r - 1.96 * se), ymax = 100 * (r + 1.96 * se)), colour = "#993bff", size = 0.3) +
  labs(x = "Metres from the catchment boundary (negative: lower-results side; positive: higher-results side)", y = "Price relative to like-for-like (%)",
       title = "Brighton and Hove: prices either side of secondary catchment boundaries",
       subtitle = "Residual log price after dwelling attributes, boundary segment and month effects, in 100 m bands; old map, Sept 2018 to Feb 2025") + theme_minimal(base_size = 10)
ggsave(here::here("figures", "bh_boundary.png"), g2, width = 9, height = 4.5, dpi = 150)
