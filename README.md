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
