# 03_epc_attributes.R - The matched sales with their EPC's dwelling attributes
# ---------------------------------------------------------------------------
# R/01 (Beech's linkage) keeps only floor area and rooms from each EPC, and gives each
# certificate a row-number id. For hedonic models we also need the dwelling's age, built form,
# energy rating and the like. This joins each matched sale's EPC (data/ppd_epc.duckdb, table epc,
# by id) back to the raw certificates on the same keys Beech's script builds: postcode, the
# tidied address lines (upper case, trimmed, brackets to spaces), inspection and lodgement dates,
# floor area and habitable rooms. Where several certificates share every key (true duplicates in
# the register), the first is taken.
#
# Inputs:  data/ppd_epc_matched.parquet (R/01), data/ppd_epc.duckdb (R/01),
#          ../EPC_Data_Analysis/domestic-csv/certificates-*.csv (EPC bulk download)
# Output:  data/sales_epc.parquet: one row per matched sale with
#          construction_age_band, built_form, property_type_epc, current_energy_efficiency,
#          current_energy_rating, tenure_epc, number_heated_rooms, floor_level, extension_count,
#          main_fuel, mains_gas_flag, uprn
# Rscript R/03_epc_attributes.R   (several minutes: reads every certificate again)
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(DBI); library(duckdb) })
epc_dir <- normalizePath(file.path(dirname(here::here()), "EPC_Data_Analysis", "domestic-csv"), winslash = "/")
con <- dbConnect(duckdb(shared_home = FALSE))
q <- function(s) invisible(dbExecute(con, s))
q(sprintf("SET temp_directory = '%s'", here::here("data", "duckdb_tmp")))
q("SET memory_limit = '24GB'")
q("SET preserve_insertion_order = false")
q(sprintf("ATTACH '%s' AS db (READ_ONLY)", here::here("data", "ppd_epc.duckdb")))
q("CREATE OR REPLACE MACRO tidy(x) AS regexp_replace(trim(coalesce(upper(x), '')), '[\\[\\]\\(\\)\\{\\}]', ' ', 'g')")
q("CREATE OR REPLACE MACRO fld(x) AS nullif(nullif(trim(x, ' \t'), ''), 'NA')")

# the EPCs the sales were matched to, with their keys
q(sprintf("
  CREATE TEMP TABLE m AS
  SELECT s.*, e.add1, e.add, e.inspectiondate AS e_insp, e.lodgementdate AS e_lodg, e.tfarea AS e_tfarea, e.numberrooms AS e_rooms
  FROM read_parquet('%s') s JOIN db.epc e ON e.id = s.epc_id", here::here("data", "ppd_epc_matched.parquet")))
needed <- dbGetQuery(con, "SELECT count(*) FROM m")[[1]]

# the certificates, keyed as Beech's script keys them, keeping only postcodes with a matched sale
q(sprintf("
  CREATE TEMP TABLE cert AS
  SELECT tidy(fld(address1)) AS add1, tidy(fld(address)) AS add, regexp_replace(coalesce(fld(postcode), ''), '[\\[\\]\\(\\)\\{\\}]', ' ', 'g') AS postcode,
         TRY_CAST(fld(inspection_date) AS DATE) AS insp, TRY_CAST(fld(lodgement_date) AS DATE) AS lodg,
         TRY_CAST(fld(total_floor_area) AS DOUBLE) AS tfarea, TRY_CAST(fld(number_habitable_rooms) AS DOUBLE) AS rooms,
         fld(construction_age_band) AS construction_age_band, fld(built_form) AS built_form, fld(property_type) AS property_type_epc,
         TRY_CAST(fld(current_energy_efficiency) AS INTEGER) AS current_energy_efficiency, fld(current_energy_rating) AS current_energy_rating,
         fld(tenure) AS tenure_epc, TRY_CAST(fld(number_heated_rooms) AS DOUBLE) AS number_heated_rooms, fld(floor_level) AS floor_level,
         TRY_CAST(fld(extension_count) AS DOUBLE) AS extension_count, fld(main_fuel) AS main_fuel, fld(mains_gas_flag) AS mains_gas_flag,
         fld(uprn) AS uprn
  FROM read_csv('%s/certificates-*.csv', all_varchar = true, union_by_name = true)
  WHERE fld(postcode) IN (SELECT DISTINCT postcode FROM m)", epc_dir))
q("
  CREATE TEMP TABLE cert1 AS
  SELECT postcode, add1, add, insp, lodg, tfarea, rooms,
         first(COLUMNS(* EXCLUDE (postcode, add1, add, insp, lodg, tfarea, rooms)))
  FROM cert GROUP BY ALL")

q(sprintf("
  COPY (
    SELECT m.* EXCLUDE (add1, add, e_insp, e_lodg, e_tfarea, e_rooms),
           c.* EXCLUDE (postcode, add1, add, insp, lodg, tfarea, rooms)
    FROM m LEFT JOIN cert1 c
      ON c.postcode = m.postcode AND c.add1 = m.add1 AND c.add = m.add
     AND c.insp IS NOT DISTINCT FROM m.e_insp AND c.lodg IS NOT DISTINCT FROM m.e_lodg
     AND c.tfarea IS NOT DISTINCT FROM m.e_tfarea AND c.rooms IS NOT DISTINCT FROM m.e_rooms
  ) TO '%s' (FORMAT parquet)", here::here("data", "sales_epc.parquet")))
r <- dbGetQuery(con, sprintf("SELECT count(*) n, count(construction_age_band) n_age, count(current_energy_efficiency) n_eff, count(built_form) n_form
                              FROM read_parquet('%s')", here::here("data", "sales_epc.parquet")))
message(sprintf("%s matched sales; %s written; attributes found for %.1f%% (age band %s, energy %s, built form %s)",
                format(needed, big.mark = ","), format(r$n, big.mark = ","), 100 * r$n_eff / r$n,
                format(r$n_age, big.mark = ","), format(r$n_eff, big.mark = ","), format(r$n_form, big.mark = ",")))
dbDisconnect(con, shutdown = TRUE)
