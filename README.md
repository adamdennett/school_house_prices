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
- **`R/08_popularity_models.R`:** popularity (log first preferences per place, standardised) against results and value added, in the same three designs. Within LSOA (4.3m sales):
  - per standard deviation, together: Attainment 8 +1.15%, value added −0.38%, secondary popularity +0.11%;
  - alone: secondary popularity +0.27%, primary popularity +0.27%;
  - an over-subscribed nearest school (2+ first preferences per place): secondary +33% raw, 0% same kind of home, +1.0% within LSOA (1.9% of sales); primary +56% raw, +4.5% same kind of home, +1.4% within LSOA (0.4% of sales).
- **`R/09_quality_gap.R`:** a walking-time analogue of Greaves and Venturin's (2026) boundary design. It covers the 7,599 LSOAs split between two nearest state secondaries (1.07m sales), and compares homes on the side whose nearest school has the higher Attainment 8 with homes on the other side, like for like, within the LSOA.
  - **Better side overall:** +0.64% (0.34 to 0.93).
  - **By Attainment 8 gap quartile:** +0.1%, +0.6%, +0.8%, +1.1%.
  - **By pair position:**
    - both below the national median: +0.3% (not significant);
    - straddling: +0.8%;
    - both above: +0.75%;
    - Q3 against Q4 ("reaching the top"): +1.4%;
    - Q1 against Q2 ("escaping the bottom"): +0.4% (not significant).
  - **Free school meals:**
    - higher results and fewer FSM pupils: +0.9%;
    - higher results but more FSM pupils: −0.2%. This is the G&V "bundle".
  - **Value added:** the side with the higher value added gets −0.3%.
  - **Within 3 minutes of the line:** +0.4% (−0.2 to 1.0).
  - Magnitudes are about a quarter of G&V's. The nearest school by walking time is a noisier proxy for priority than a catchment, which attenuates.
- **`R/10_ofsted_event_study.R`:** prices before and after the nearest school's Ofsted grade changes (Sun and Abraham; OA and district × month fixed effects; 2019–2024 regradings). The events:
  - about 700 secondaries and 2,100 primaries upgraded, and 730 and 2,800 downgraded;
  - 25,000–37,000 treated OAs per model.

  Average over three years after publication:

  | | Secondary | Primary |
  |---|---|---|
  | Upgrade | +0.14% (−0.05 to 0.33) | +0.15% (−0.06 to 0.37) |
  | Downgrade | −0.10% (−0.29 to 0.08) | −0.02% (−0.23 to 0.19) |

  Secondary upgrades climb slowly to about +0.3% at two to three years and +0.8% at three and a half years, as fewer cohorts are observed that long. Downgrades barely move prices. Some pre-trends: prices near later-downgraded secondaries were already drifting down three years before, and near later-upgraded primaries drifting up.
- **`R/11_heterogeneity.R`:** the within-LSOA premium per standard deviation of the nearest secondary's Attainment 8 (school-level SD, value added held constant; overall +0.67%), by group:
  - **Admissions basis of the nearest secondary:**
    - mostly distance or catchment: +1.5% (1.1 to 1.9);
    - faith: +0.95%;
    - grammar: +0.15% (not significant).
    - In the R/09 split-LSOA design, when the better school is a faith school the better side gets −0.0%; when it admits by distance, +0.86%; when it is a grammar, +0.84% (0.0 to 1.7).
    - **Buyers pay where the address buys priority.**
  - **Brighton and Hove** (random allocation within catchments since 2008): +0.3% (−2.8 to 3.5). That is too few sales to say anything.
  - **Region:**
    - highest: Yorkshire and The Humber +1.7%, Inner London +1.5%;
    - lowest: South East +0.1%, East Midlands +0.3%.
  - **Deprivation:** no clear gradient; the least deprived quintile is lowest at +0.3%.
  - **Rural schools** +1.2%, urban +0.7%.
- **`R/12_three_groups.R`:** the nearest secondary's Attainment 8 and value added for all, disadvantaged and non-disadvantaged pupils (4.35m sales). Within LSOA, per SD:
  - **Attainment 8:** all +0.67%, disadvantaged +0.48%, non-disadvantaged +0.68%.
  - **Value added:** −0.22%, −0.33% and −0.32%.
  - **Together with the free school meals share,** the FSM share carries it: −0.99% per 10 percentage points. Non-disadvantaged Attainment 8 +0.5% (not significant); disadvantaged −0.8%.
  - **The intake is what is priced.**

## Report

`report/findings.qmd` gathers R/06 to R/12, with tables, dot-and-interval charts, models and references. Every number is read from `data/*.rds`. Render with `quarto render report/findings.qmd`, which gives `report/findings.html`, self-contained. These are working notes, not for circulation.
- **`R/14_epc_controls.R`:** two checks on the like-for-like results.

  **A. Control ladder.** Property controls go from none (C0), to Price Paid's type, tenure and new build (C1), to EPC floor area and rooms (C2), to EPC age band, built form and efficiency (C3, the main specification), to every other usable EPC field (C4). Each is run with month effects only, and with month and LSOA effects.
  - **Month effects only:** Attainment 8 per SD goes from +22.3% (C0) to +18.6% (C3) and +18.2% (C4); nearest secondary Outstanding from +31.5% to +27.2%. The EPC raises the within R² from 0.09 to 0.47, but leaves most of the school premium in place.
  - **With LSOA effects:** Attainment 8 is +0.81% (C0), +0.68% (C3), +0.66% (C4); Outstanding is +1.07%, +1.00%, +0.97%.
  - **So the home's characteristics explain little of the raw premium; the neighbourhood explains most of it.**

  **B. Repeat sales.** Same property by EPC UPRN, with property and district × month fixed effects: 554,250 properties, 1.14m sales.
  - **Nearest secondary Outstanding:** +0.55% (0.32 to 0.78), against +0.80% within LSOA on the same sales.
  - **Nearest primary Outstanding:** +0.20%.
  - **Popularity:** secondary +0.17% per SD, primary +0.08% per SD.
- **`R/13_barriers.R`:** drops the R/09 lines that follow major roads, rivers or railways.
  - **The lines:** 7,510 shared edges between the two sides of split LSOAs (2021 OA boundaries from ONS), median 749 m.
  - **Barriers, using OpenStreetMap from the Geofabrik England extract:**
    - major roads (motorway, trunk, primary) within 25 m: 3.4% of lines;
    - rivers and canals within 25 m: 0.9%;
    - railways from `../rail/RailStations.csv` (Network Rail stations joined along each ELR by mileage, chords up to 10 km, 150 m tolerance): 5.4%; OSM rail within 25 m: 1.5%. The two agree on 94% of lines.
    - Any barrier: 9.4% (5.8% with OSM rail).
  - **Better side without barrier lines:** +0.67% (+0.61% with OSM rail), against +0.64% for all. The gap and FSM patterns are unchanged. Barrier lines only: +0.37% (−0.7 to 1.5).
  - Lines are in `data-raw/boundaries/switch_lines.gpkg`.
