# Do schools push up house prices? A national test

A national follow-up to Tan and Dennett's Brighton study ("Do good schools influence property prices? Disentangling school and neighbourhood effects", CUPUM). That study found that, once a home and its neighbourhood are accounted for, a good school nearby adds only a few per cent to its price.

The plan is in `school_value_added/writeup/house_prices_schools_backlog.md`:
1. Link every sale in England to its floor area and rooms.
2. Attach the walking time to each nearby school, and each school's Ofsted grade, value added and popularity at the date of sale.
3. Reproduce the raw premium near the most popular schools.
4. Add the home and its neighbourhood.
5. Test changes over time.

## Step 1: sales linked to EPCs (`R/01_ppd_epc_linkage.R`)

Chris Beech's DuckDB refactor of Bin Chi's Land Registry Price Paid to EPC linkage, as published (commit 8514d28 of <https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre>, academic use). Only the paths are changed (see the diff in the commit that added it).

- **Inputs, not committed:**
  - **EPC certificates:** the domestic EPC bulk download (`certificates-2008.csv` to `certificates-2026.csv`) from <https://get-energy-performance-data.communities.gov.uk/>, in `../EPC_Data_Analysis/domestic-csv/` (downloaded 15 November 2026).
  - **Price Paid:** Land Registry Price Paid Data, complete file (`pp-complete.csv`, 5.5 GB, last modified 28 October 2026), from <http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv>, in `data-raw/`.
  - **Address rules:** Bin Chi's `rulechi.csv`, from the clone of Beech's repository in `beech-ppd-epc/`:

```
git clone https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre.git beech-ppd-epc
curl -L http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv -o data-raw/pp-complete.csv
Rscript R/01_ppd_epc_linkage.R
```

- **Outputs, not committed:** `data/ppd_epc.duckdb`, `data/pp-psqm.csv` (one row per matched sale) and `figures/EPC match percentage.png`.

Licences:
- Price Paid Data: Open Government Licence; it contains HM Land Registry data © Crown copyright and database right.
- EPC data: open data from the Department for Levelling Up, Housing and Communities and its successor, under its own terms. Address data in EPCs is subject to Royal Mail and OS terms.
- The linkage code: academic use, per its header.

## Step 2: postcode lookup (`R/02_postcode_lookup.R`)

ONS Postcode Directory, August 2026 (ONSPD_AUG_2026.zip, from <https://www.arcgis.com/sharing/rest/content/items/9e5a92a3cfb14dc7ad43d6ea7a7b8c7f/data>, extracted to `data-raw/onspd/`). It gives each postcode in England and Wales, live or terminated, its 2021 OA, LSOA and MSOA, district, coordinates and 2025 IMD rank. Output: `data/postcode_lookup.parquet` (not committed).

### First run (15 November 2026)

- **Read:** 30,529,452 EPC certificates and 29,743,989 Price Paid transactions (1995 to September 2026).
- **Matched to an EPC:** 24,618,450 (83%), with stage 5 (Beech's unit signature rule) adding 66,719. The run took 5.9 minutes.
- **Two local changes to the script:** the paths, and DuckDB's old lambda syntax re-enabled for duckdb 2.0.
- **The output** (`data/pp-psqm.csv`) has date, postcode, county, district, price, floor area, price per m², property type and PPD category.
- **For the hedonic models,** rooms are in the `epc` table of `data/ppd_epc.duckdb`. Construction age, built form, energy rating and tenure must be joined back from the certificates by certificate number.

## Steps 3 to 6

- **`R/03_epc_attributes.R`:** joins each matched sale's EPC back to the raw certificates. It uses the same keys Beech's script builds: postcode, tidied address, dates, floor area and rooms. This adds construction age band, built form, energy rating, EPC tenure, heated rooms, floor level, extensions, main fuel and UPRN. All 24,618,450 matched sales found their certificate (age band for all but 153; built form for 98.6%).
- **`R/04_oa_schools.R`:** from the national walking routes (`../school_active_travel`, Zenodo 10.5281/zenodo.24188820), every Output Area to state school pair within 20 minutes (870,225 pairs), and each Output Area's nearest state primary and secondary.
- **`R/05_sales_panel.R`:** sales from September 2018 to September 2026 (6,401,832) with:
  - Output Area, LSOA, MSOA, district and IMD;
  - the nearest primary and secondary and their Ofsted grades at the date of sale, from the year-end files as at 31 August 2018–2023 and the latest graded inspection as at September 2026, with URNs followed through GIAS successor links;
  - minutes to the nearest Outstanding, and Good or better, primary and secondary;
  - the nearest secondary's Value Added v2.1 and four-year Attainment 8 (from `../school_value_added`).
- **`R/06_premium_models.R`:** the premium near "good" schools, raw, then for the same kind of home (dwelling attributes with district effects), then within the same LSOA. Also Attainment 8 and value added together.
- **`R/07_popularity.R`:** each sale's nearest primary and secondary, with the school's first preferences per place in the admissions round buyers could see building (DfE applications and offers, school level, entry 2014 to 2026). Output: `data/sales_panel_pop.parquet`. The figure is found for 86.9% of sales for the primary and 86.0% for the secondary.

### First results (R/06; 5,180,630 sales with complete attributes)

| Near a "good" school | Raw | Same kind of home | Same home, same LSOA |
|---|---|---|---|
| Outstanding secondary within 10 min walk | +37.0% | +2.7% | +0.6% |
| Outstanding primary within 10 min walk | +20.4% | +2.1% | −0.5% |
| Nearest secondary is Outstanding | +31.5% | +5.2% | +1.0% |
| Nearest secondary in top 10% for Attainment 8 | +21.9% | +4.1% | +0.9% |
| Nearest secondary in top 10% for value added | −10.8% | −4.4% | 0.0% |

Per standard deviation, together: Attainment 8 +0.196 log points raw, +0.055 like for like, +0.007 within LSOA; value added −0.073, −0.020, −0.002.
