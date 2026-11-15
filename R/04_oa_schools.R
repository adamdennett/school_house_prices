# 04_oa_schools.R - State schools within walking reach of every Output Area
# ---------------------------------------------------------------------------
# From the national walking routes (school_active_travel; Zenodo 10.5281/zenodo.24188820):
# every Census 2021 Output Area to every open mainstream school within 120 minutes' walk at
# 4.8 km/h (r5r on OpenStreetMap). Kept here: state-funded schools only (independent schools
# left out), with whether each teaches primary and secondary ages.
#
# Outputs:
#   data/oa_school_pairs20.parquet  every OA to state school pair within 20 minutes' walk:
#                                   oa21cd, urn, walk_min (walk_min_r5), route_m, primary, secondary
#   data/oa_nearest.parquet         each OA's nearest state primary and secondary by walking time
#                                   (at any distance up to 120 minutes): urn and minutes
# Rscript R/04_oa_schools.R   (a minute or two)
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow); library(dplyr) })
sat <- normalizePath(file.path(dirname(here::here()), "school_active_travel", "data", "out"), winslash = "/")
sch <- as.data.table(read_parquet(file.path(sat, "schools.parquet")))[independent == FALSE,
         .(urn, primary = serves_primary, secondary = serves_secondary)]
od <- open_dataset(file.path(sat, "od")) |> filter(urn %in% !!sch$urn) |>
  select(oa21cd, urn, walk_min = walk_min_r5, route_m) |> collect() |> as.data.table()
od <- merge(od, sch, by = "urn")
write_parquet(od[walk_min <= 20], here::here("data", "oa_school_pairs20.parquet"))
near <- function(flag) od[get(flag) == TRUE][order(oa21cd, walk_min), .SD[1], by = oa21cd][, .(oa21cd, urn, walk_min)]
np <- near("primary"); ns <- near("secondary")
nn <- merge(np[, .(oa21cd, near_primary_urn = urn, near_primary_min = walk_min)],
            ns[, .(oa21cd, near_secondary_urn = urn, near_secondary_min = walk_min)], by = "oa21cd", all = TRUE)
write_parquet(nn, here::here("data", "oa_nearest.parquet"))
message(sprintf("%s state-school pairs within 20 minutes; %s OAs with a state primary in reach, %s with a state secondary",
                format(nrow(od[walk_min <= 20]), big.mark = ","), format(nrow(np), big.mark = ","), format(nrow(ns), big.mark = ",")))
