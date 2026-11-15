# 07_popularity.R - How over-subscribed each sale's nearest schools were, at the date of sale
# ---------------------------------------------------------------------------
# DfE, Primary and secondary school applications and offers, 2026/27 release (18 June 2026),
# supporting file AppsandOffers_2026_SchoolLevel29062026.csv: every state school's national offer
# day figures for entry in September 2014 to September 2026 (Reception for primary, Year 7 for
# secondary). Popularity = first preferences per place offered (times_put_as_1st_preference /
# total_number_places_offered); above 1, more families put the school first than it has places.
#
# A sale between September of year t and August of year t+1 gets the figure from the round for
# entry in September t+1 (applications made in autumn t, offers in March/April t+1): the demand
# buyers in that year could see building. URNs follow GIAS successor links, as in R/05.
#
# Download (about 11 MB zip):
#   curl -L -o data-raw/admissions/all.zip "https://content.explore-education-statistics.service.gov.uk/api/releases/0bf4f9cf-721d-44e3-8473-266c87e17e00/files?fromPage=ReleaseDownloads"
#   unzip -o data-raw/admissions/all.zip -d data-raw/admissions
# Input: data/sales_panel.parquet (R/05). Output: data/sales_panel_pop.parquet, data/school_popularity.parquet
#   Rscript R/07_popularity.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(arrow) })
sva <- normalizePath(file.path(dirname(here::here()), "school_value_added"), winslash = "/")
num <- function(x) suppressWarnings(as.numeric(x))
A <- fread(here::here("data-raw", "admissions", "supporting-files", "AppsandOffers_2026_SchoolLevel29062026.csv"),
           select = c("time_period", "school_phase", "school_urn", "total_number_places_offered", "times_put_as_1st_preference",
                      "times_put_as_any_preferred_school", "admissions_policy"), colClasses = "character", encoding = "UTF-8")
A[, `:=`(entry = as.integer(substr(time_period, 1, 4)), urn = as.integer(school_urn),
         places = num(total_number_places_offered), first = num(times_put_as_1st_preference), any = num(times_put_as_any_preferred_school))]
A <- A[!is.na(urn) & places > 0]
L <- fread(file.path(sva, "data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
succ <- unique(L[LinkType == "Successor", .(from = URN, to = LinkURN)]); succ <- succ[!from %in% succ[duplicated(from), from]]
final_of <- function(u) { f <- as.integer(u); for (i in 1:10) { nx <- succ$to[match(f, succ$from)]; f <- ifelse(is.na(nx), f, nx) }; f }
A[, urn := final_of(urn)]
pop <- A[, .(first_per_place = sum(first, na.rm = TRUE) / sum(places), any_per_place = sum(any, na.rm = TRUE) / sum(places),
             places = sum(places), selective = any(admissions_policy == "Selective")), by = .(urn, phase = tolower(school_phase), entry)]
write_parquet(pop, here::here("data", "school_popularity.parquet"))

S <- as.data.table(read_parquet(here::here("data", "sales_panel.parquet")))
S[, entry := as.integer(format(date, "%Y")) + as.integer(as.integer(format(date, "%m")) >= 9)]
for (ph in c("primary", "secondary")) {
  p <- pop[phase == ph, .(u = urn, entry, fpp = first_per_place, sel = selective)]
  u <- sprintf("near_%s_urn", ph)
  S[p, on = c(setNames("u", u), "entry"), `:=`(fpp = i.fpp, sel = i.sel)]
  setnames(S, c("fpp", "sel"), c(sprintf("%s_first_per_place", ph), sprintf("%s_selective", ph)))
}
S[, entry := NULL]
write_parquet(S, here::here("data", "sales_panel_pop.parquet"))
message(sprintf("%s sales; nearest primary's popularity for %.1f%%, nearest secondary's for %.1f%%; %s school-year rows",
                format(nrow(S), big.mark = ","), 100 * mean(!is.na(S$primary_first_per_place)),
                100 * mean(!is.na(S$secondary_first_per_place)), format(nrow(pop), big.mark = ",")))
