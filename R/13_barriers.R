# 13_barriers.R - Drop "lines" that follow major roads, rivers and railways (R/09 robustness)
# ---------------------------------------------------------------------------
# Greaves and Venturin (2026) exclude catchment boundaries that coincide with major roads, rivers
# and railways, where neighbourhoods may differ for reasons other than the school. Here the
# "line" is where the nearest state secondary (by walking time) changes inside an LSOA split
# between two nearest secondaries (R/09):
#   1. 2021 Output Area polygons (ONS, Output Areas December 2021 Boundaries EW BFE V9, ArcGIS
#      FeatureServer OA_2021_EW_BFE_V9_RUC, layer 3) for the split LSOAs, cached in data-raw/boundaries.
#   2. Each LSOA's two sides = union of its OAs by nearest secondary; the line = the shared
#      boundary between the sides (boundary of one side within 0.5 m of the other).
#   3. Barriers:
#        major roads  OSM highway = motorway, motorway_link, trunk, trunk_link or primary (about the
#                     motorway and A-road network), within 25 m of the line
#        rivers       OSM waterway = river or canal, within 25 m
#        railways (a) Network Rail station list in ../rail/RailStations.csv: stations with coordinates,
#                     ordered by mileage along each Engineer's Line Reference (ELR) and joined in
#                     sequence; chords over 10 km dropped. Straight chords between stations, so within 150 m.
#                 (b) OSM railway = rail, within 25 m (cross-check)
#      OSM: Geofabrik England extract (../school_value_added/data-raw/r5/src/england-latest.osm.pbf,
#      8 November 2026), lines layer via GDAL.
#   4. A line "follows a barrier" if at least half its length is within the distance of one.
#   5. R/09's models again: all split LSOAs; without barrier lines (rail from (a), then from (b));
#      and on barrier lines only.
# Inputs: data/gap_models.rds (R/09), data/postcode_lookup.parquet (R/02), data/oa_nearest.parquet (R/04),
#   data/sales_panel_pop.parquet (R/07), DfE applications and offers (R/07)
# Outputs: data/barrier_lines.parquet (one row per split LSOA), data-raw/boundaries/oa21_split.gpkg,
#   data/barrier_models.rds, figures/barrier_premiums.png       Rscript R/13_barriers.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(sf); library(httr); library(fixest); library(ggplot2) })
setFixest_nthreads(8)
sf_use_s2(FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
gm <- readRDS(here::here("data", "gap_models.rds")); pr <- gm$pairs

# ---- 1. OA polygons for the split LSOAs -------------------------------------------------------------
dir.create(here::here("data-raw", "boundaries"), showWarnings = FALSE, recursive = TRUE)
oaf <- here::here("data-raw", "boundaries", "oa21_split.gpkg")
if (!file.exists(oaf)) {
  url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/OA_2021_EW_BFE_V9_RUC/FeatureServer/3/query"
  ls_ <- split(pr$lsoa21cd, ceiling(seq_along(pr$lsoa21cd) / 80))
  parts <- lapply(seq_along(ls_), function(i) {
    for (try in 1:4) {
      r <- try(POST(url, body = list(where = sprintf("LSOA21CD IN (%s)", paste0("'", ls_[[i]], "'", collapse = ",")),
                                     outFields = "OA21CD,LSOA21CD", outSR = "27700", returnGeometry = "true", f = "geojson"),
                    encode = "form", timeout(120)), silent = TRUE)
      if (!inherits(r, "try-error") && status_code(r) == 200) break
      Sys.sleep(5)
    }
    x <- st_read(content(r, "text", encoding = "UTF-8"), quiet = TRUE)
    if (i %% 10 == 0) message("OA batch ", i, " of ", length(ls_))
    x
  })
  oa <- do.call(rbind, parts); st_crs(oa) <- 27700
  st_write(oa, oaf, quiet = TRUE, delete_dsn = TRUE)
}
oa <- st_read(oaf, quiet = TRUE); names(oa)[1:2] <- c("oa21cd", "lsoa21cd")
oa <- st_make_valid(oa)
near <- as.data.table(read_parquet(here::here("data", "oa_nearest.parquet")))[, .(oa21cd, urn = near_secondary_urn)]
oa <- merge(oa, near, by = "oa21cd")
oa <- merge(oa, pr[, .(lsoa21cd, hi_urn, lo_urn)], by = "lsoa21cd")
oa$side <- ifelse(oa$urn == oa$hi_urn, "hi", ifelse(oa$urn == oa$lo_urn, "lo", NA))
oa <- oa[!is.na(oa$side), ]
message(nrow(oa), " OAs in ", length(unique(oa$lsoa21cd)), " split LSOAs")

# ---- 2. the line between the two sides ---------------------------------------------------------------
sides <- aggregate(oa["side"], by = list(lsoa21cd = oa$lsoa21cd, s = oa$side), FUN = function(x) x[1])
sides <- st_make_valid(sides)
L <- lapply(split(seq_len(nrow(sides)), sides$lsoa21cd), function(ix) {
  if (length(ix) != 2) return(NULL)
  a <- sides[ix[sides$s[ix] == "hi"], ]; b <- sides[ix[sides$s[ix] == "lo"], ]
  ln <- tryCatch(st_intersection(st_boundary(st_geometry(a)), st_buffer(st_geometry(b), 0.5)), error = function(e) NULL)
  if (is.null(ln) || length(ln) == 0) return(NULL)
  ln <- st_collection_extract(st_union(ln), "LINESTRING")
  if (length(ln) == 0) return(NULL)
  st_sf(lsoa21cd = a$lsoa21cd, geometry = st_union(ln))
})
lines <- do.call(rbind, Filter(Negate(is.null), L)); st_crs(lines) <- 27700
lines$line_m <- as.numeric(st_length(lines))
lines <- lines[lines$line_m > 0, ]
message(nrow(lines), " lines; median length ", round(median(lines$line_m)), " m")

# ---- 3. barriers -------------------------------------------------------------------------------
pbf <- normalizePath(file.path(dirname(here::here()), "school_value_added", "data-raw", "r5", "src", "england-latest.osm.pbf"), winslash = "/")
osm <- st_read(pbf, quiet = TRUE, query = paste("SELECT highway, waterway, railway FROM lines WHERE",
  "highway IN ('motorway','motorway_link','trunk','trunk_link','primary') OR waterway IN ('river','canal') OR railway = 'rail'"))
osm <- st_transform(osm, 27700)
osm$type <- ifelse(!is.na(osm$highway), "road", ifelse(!is.na(osm$waterway), "river", "rail_osm"))
rs <- fread(file.path(dirname(here::here()), "rail", "RailStations.csv"), encoding = "UTF-8")
setnames(rs, 1:5, c("lat", "lon", "elr", "grid", "mileage"))
rs <- rs[!is.na(lat) & !is.na(lon) & elr != ""]
rs[, miles := num(sub("m.*", "", mileage)) + num(sub(".*m\\s*([0-9]+)ch.*", "\\1", mileage)) / 80]
rs <- unique(rs[!is.na(miles)], by = c("elr", "miles"))[order(elr, miles)]
rs[, `:=`(lon2 = shift(lon, -1), lat2 = shift(lat, -1)), by = elr]   # each station to the next along its ELR
sg <- rs[!is.na(lon2)]
nr <- st_sf(elr = sg$elr, geometry = st_sfc(lapply(seq_len(nrow(sg)), function(i) st_linestring(rbind(c(sg$lon[i], sg$lat[i]), c(sg$lon2[i], sg$lat2[i])))), crs = 4326))
nr <- st_transform(nr, 27700)
nr <- nr[as.numeric(st_length(nr)) <= 10000, ]   # chords over 10 km (gaps in the list) cannot stand in for track
message(nrow(nr), " station-to-station chords (<= 10 km) on ", uniqueN(nr$elr), " ELRs from ", nrow(rs), " stations; OSM: ",
        paste(names(table(osm$type)), table(osm$type), collapse = ", "))

share_near <- function(bar, d) {
  idx <- st_intersects(st_buffer(lines, d), bar)
  vapply(seq_len(nrow(lines)), function(i) {
    if (!length(idx[[i]])) return(0)
    z <- tryCatch(st_intersection(st_geometry(lines)[i], st_union(st_buffer(st_geometry(bar)[idx[[i]]], d))), error = function(e) NULL)
    if (is.null(z) || !length(z)) 0 else min(1, sum(as.numeric(st_length(z))) / lines$line_m[i])
  }, numeric(1))
}
lines$road <- share_near(osm[osm$type == "road", ], 25)
lines$river <- share_near(osm[osm$type == "river", ], 25)
lines$rail_osm <- share_near(osm[osm$type == "rail_osm", ], 25)
lines$rail_nr <- share_near(nr, 150)
B <- as.data.table(st_drop_geometry(lines))
B[, `:=`(bar_nr = road >= 0.5 | river >= 0.5 | rail_nr >= 0.5, bar_osm = road >= 0.5 | river >= 0.5 | rail_osm >= 0.5)]
write_parquet(B, here::here("data", "barrier_lines.parquet"))
st_write(lines, here::here("data-raw", "boundaries", "switch_lines.gpkg"), quiet = TRUE, delete_dsn = TRUE)
print(B[, .(lsoas = .N, road = mean(road >= 0.5), river = mean(river >= 0.5), rail_nr = mean(rail_nr >= 0.5), rail_osm = mean(rail_osm >= 0.5),
            any_nr = mean(bar_nr), any_osm = mean(bar_osm), rail_agree = mean((rail_nr >= 0.5) == (rail_osm >= 0.5)))])

# ---- 4. R/09 again, with and without barrier lines --------------------------------------------------
S <- as.data.table(read_parquet(here::here("data", "sales_panel_pop.parquet")))
S <- S[categorytype == "A" & price > 0 & tfarea > 0]
S[, `:=`(lp = log(price), lfa = log(tfarea), ym = format(date, "%Y-%m"), rooms = pmin(numberrooms, 10), eff = current_energy_efficiency)]
for (v in c("built_form", "construction_age_band", "propertytype", "duration", "oldnew")) S[is.na(get(v)) | get(v) %in% c("", "NO DATA!", "INVALID!"), (v) := "unknown"]
S <- S[!is.na(rooms) & !is.na(eff) & !is.na(lsoa21cd) & !is.na(att8_4yr)]
ctrl <- "lfa + i(rooms) + i(propertytype) + i(built_form) + i(construction_age_band) + eff + i(duration) + i(oldnew)"
G <- merge(S, pr[, .(lsoa21cd, hi_urn, lo_urn, gap_q, aligned)], by = "lsoa21cd")
G[, better := as.integer(near_secondary_urn == hi_urn)]
G <- merge(G, B[, .(lsoa21cd, bar_nr, bar_osm)], by = "lsoa21cd", all.x = TRUE)
G[is.na(bar_nr), `:=`(bar_nr = FALSE, bar_osm = FALSE)]   # no shared edge found: kept as not a barrier line
fit <- function(rhs, d, keep_re) {
  m <- feols(as.formula(sprintf("lp ~ %s + %s | ym + lsoa21cd", rhs, ctrl)), data = d, cluster = ~msoa21cd, lean = TRUE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(keep_re, term)]; setnames(ct, 2:3, c("b", "se"))
  ct[, .(term, b, se, n = nobs(m), lsoas = uniqueN(d$lsoa21cd), pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
}
sets <- list("All split LSOAs" = G, "Without barrier lines (Network Rail stations)" = G[bar_nr == FALSE],
             "Without barrier lines (OSM rail)" = G[bar_osm == FALSE], "Barrier lines only" = G[bar_nr == TRUE])
R <- rbindlist(lapply(names(sets), function(k) rbind(
  fit("better", sets[[k]], "^better$")[, spec := "Overall"],
  fit("i(gap_q, better)", sets[[k]], "better")[, spec := "By Attainment 8 gap"],
  fit("i(aligned, better)", sets[[k]][!is.na(aligned)], "better")[, spec := "By free school meals"])[, set := k]))
R[, group := sub(".*::", "", sub(":better$", "", term))][group == "better", group := "Better side"]
saveRDS(list(R = R, B = B, n_lines = nrow(lines), built = Sys.Date()), here::here("data", "barrier_models.rds"))
options(width = 160); print(R[, .(set, spec, group, pct = round(pct, 2), lo = round(lo, 2), hi = round(hi, 2), lsoas, n)])

R[, set := factor(set, rev(names(sets)))][, spec := factor(spec, unique(spec))]
g <- ggplot(R, aes(pct, group, colour = set)) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.7), size = 0.3) +
  facet_grid(spec ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c("All split LSOAs" = "#c9a2f5", "Without barrier lines (Network Rail stations)" = "#993bff",
                                 "Without barrier lines (OSM rail)" = "#5b2a99", "Barrier lines only" = "#e8a33d"), name = NULL,
                      guide = guide_legend(reverse = TRUE, nrow = 2)) +
  labs(x = "Premium (%) on the better side, same LSOA, like-for-like homes (95% interval)", y = NULL,
       title = "The better-side premium, without lines that follow major roads, rivers or railways",
       subtitle = "A line follows a barrier if half or more of it lies within 25 m of a motorway, trunk or primary road, river or canal,\nor of a railway (150 m of Network Rail station-to-station chords; 25 m of OSM track)",
       caption = "OpenStreetMap (Geofabrik England, Nov 2026); Network Rail station list (ELR, mileage); ONS OA 2021 boundaries; R/09 design") +
  theme_minimal(base_size = 10) + theme(legend.position = "top", plot.title.position = "plot", strip.placement = "outside",
                                        strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1), panel.grid.minor = element_blank())
ggsave(here::here("figures", "barrier_premiums.png"), g, width = 10, height = 6.5, dpi = 150)
