# 02_postcode_lookup.R - Postcode to 2021 Output Area, LSOA, MSOA, district and coordinates
# ---------------------------------------------------------------------------
# Source: ONS Postcode Directory (ONSPD), August 2026, UK
#   https://www.arcgis.com/sharing/rest/content/items/9e5a92a3cfb14dc7ad43d6ea7a7b8c7f/data (ONSPD_AUG_2026.zip),
#   Data/ONSPD_AUG_2026_UK.csv extracted to data-raw/onspd/.
# Keeps England and Wales postcodes, live or terminated (sales go back to 1995, so terminated
# postcodes are kept), with their 2021 Output Area, LSOA and MSOA, 2026 district, grid
# reference (1 m), latitude and longitude, and the 2025 IMD rank (imd25ind).
# Output: data/postcode_lookup.parquet    Rscript R/02_postcode_lookup.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow) })
f <- here::here("data-raw", "onspd", "ONSPD_AUG_2026_UK.csv")
p <- fread(f, select = c("pcds", "doterm", "ctry26cd", "lad26cd", "oa21cd", "lsoa21cd", "msoa21cd", "east1m", "north1m", "lat", "long", "imd25ind", "ruc21ind"),
           showProgress = FALSE)
p <- p[ctry26cd %in% c("E92000001", "W92000004") & oa21cd != ""]
p[, pc := gsub(" ", "", toupper(pcds))]
write_parquet(p, here::here("data", "postcode_lookup.parquet"))
message(format(nrow(p), big.mark = ","), " postcodes (", format(sum(is.na(p$doterm) | p$doterm == ""), big.mark = ","), " live)")
