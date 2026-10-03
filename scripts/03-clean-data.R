# 02-clean-data.R
#
# Builds one clean table for RQ3 (school_rq3), with ONE ROW PER SCHOOL:
#   - avg_advice      : average advice on a 1-6 scale        (outcome)
#   - pct_havo_plus   : % HAVO-or-higher advice              (robustness)
#   - pct_maths_1s    : % of pupils reaching 1S in maths     (test result)
#   - pct_reading_2f  : % of pupils reaching 2F in reading   (test result)
#   - provider        : test provider used by the school     (control)
#   - schoolweging    : disadvantage score of the school     (moderator)
#
# HOW TO USE
#   1. Open the project via the .Rproj file (so paths start at the root)
#   2. Run this script once:   source("scripts/02-clean-data.R")
#      -> creates `school_rq3` in your environment, and saves it to
#         data/processed/school_rq3.rds
#   3. Next time, load it quickly with:
#      school_rq3 <- readRDS("data/processed/school_rq3.rds")
#
# To change an assumption (robustness checks), edit section 0 and rerun.

source("scripts/00-packages.R")
source("scripts/01-get-data.R")


# ---- 0. settings -----------------------------------------------------------

# DUO hides counts below 5 as "<5". We replace them with the midpoint of 1-4.
# Robustness: rerun with 1 and 4.
lt5_value <- 2.5

# minimum number of advised pupils per school (robustness: 5 and 20)
min_pupils <- 10

# helper: turn a count column that may contain "<5" into numbers
fix_counts <- function(x) {
  x <- as.character(x)
  x[x == "<5"] <- lt5_value
  as.numeric(x)
}


# ---- 1. advice per school --------------------------------------------------

# numeric scale: combined advices sit halfway between their two tracks
advice_scale <- c(
  PRO = 1, VMBO_B = 2, VMBO_B_K = 2.5, VMBO_K = 3, VMBO_K_GT = 3.5,
  VMBO_GT = 4, VMBO_GT_HAVO = 4.5, HAVO = 5, HAVO_VWO = 5.5, VWO = 6
)
scale_cols <- names(advice_scale)
all_advice_cols <- c("VSO", scale_cols, "ADVIES_NIET_MOGELIJK")

advice <- schooladviezen |>
  # regular primary schools only (drop Sbo schools)
  filter(SOORT_PO == "Bo") |>
  mutate(across(all_of(all_advice_cols), fix_counts)) |>
  # sum the locations of each school
  group_by(INSTELLINGSCODE) |>
  summarise(across(all_of(all_advice_cols), sum), .groups = "drop") |>
  mutate(
    # pupils on the scale (VSO and "no advice" left out)
    n_advised = rowSums(pick(all_of(scale_cols))),
    # average advice: each track count x its score, divided by total
    avg_advice = rowSums(
      across(all_of(scale_cols), ~ .x * advice_scale[[cur_column()]])
    ) / n_advised,
    # kept for robustness checks / reporting
    pct_havo_plus = 100 * (HAVO + HAVO_VWO + VWO) / n_advised,
    n_vso = VSO,
    n_no_advice = ADVIES_NIET_MOGELIJK
  ) |>
  filter(n_advised > 0) |>
  select(
    INSTELLINGSCODE, n_advised, avg_advice, pct_havo_plus,
    n_vso, n_no_advice
  )

# checks
nrow(advice) == n_distinct(advice$INSTELLINGSCODE) # should be TRUE
summary(advice$avg_advice) # all between 1 and 6
summary(advice$n_advised) # typical school size
sum(advice$n_vso) + sum(advice$n_no_advice) # pupils outside the scale


# ---- 2. test results per school --------------------------------------------

ref_cols <- c(
  "REKENEN_LAGER1F", "REKENEN_1F", "REKENEN_1S",
  "LV_LAGER1F", "LV_1F", "LV_2F"
)

test_results <- referentieniveaus |>
  # file contains Bo schools only, so no SOORT_PO filter needed
  mutate(across(all_of(ref_cols), fix_counts)) |>
  group_by(INSTELLINGSCODE) |>
  summarise(across(all_of(ref_cols), sum), .groups = "drop") |>
  mutate(
    n_maths = REKENEN_LAGER1F + REKENEN_1F + REKENEN_1S,
    n_reading = LV_LAGER1F + LV_1F + LV_2F,
    pct_maths_1s = 100 * REKENEN_1S / n_maths,
    pct_reading_2f = 100 * LV_2F / n_reading
  ) |>
  filter(n_maths > 0, n_reading > 0) |>
  select(INSTELLINGSCODE, n_maths, pct_maths_1s, pct_reading_2f)

# checks
nrow(test_results) == n_distinct(test_results$INSTELLINGSCODE) # TRUE
nrow(test_results) # how many schools
summary(test_results$pct_maths_1s) # between 0 and 100
summary(test_results$pct_reading_2f) # between 0 and 100


# ---- 3. test provider per school -------------------------------------------

provider <- eindscores |>
  filter(SOORT_PO == "Bo") |>
  select(INSTELLINGSCODE, ends_with("_AANTAL")) |>
  # one row per school location per provider
  pivot_longer(
    -INSTELLINGSCODE,
    names_to = "provider",
    values_to = "n_tested"
  ) |>
  mutate(
    provider = str_remove(provider, "_AANTAL"),
    n_tested = fix_counts(n_tested)
  ) |>
  # sum locations, then keep the provider that tested the most pupils
  group_by(INSTELLINGSCODE, provider) |>
  summarise(n_tested = sum(n_tested), .groups = "drop") |>
  filter(n_tested > 0) |>
  group_by(INSTELLINGSCODE) |>
  slice_max(n_tested, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(INSTELLINGSCODE, provider)

# checks
nrow(provider) == n_distinct(provider$INSTELLINGSCODE) # TRUE
nrow(provider)
count(provider, provider, sort = TRUE) # schools per provider


# ---- 4. schoolweging per school --------------------------------------------

weging <- schoolweging |>
  # 119 locations have no schoolweging: they can't be used
  filter(!is.na(schoolweging)) |>
  # OVT looks like "00AP|C1": the part before "|" is the INSTELLINGSCODE
  mutate(INSTELLINGSCODE = str_remove(OVT, "\\|.*$")) |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    # weighted by pupils when all counts are known, plain mean otherwise
    schoolweging = if (anyNA(aantal_leerlingen)) {
      mean(schoolweging)
    } else {
      weighted.mean(schoolweging, w = aantal_leerlingen)
    },
    n_locations = n(),
    .groups = "drop"
  )

# checks
nrow(weging) == n_distinct(weging$INSTELLINGSCODE) # TRUE
nrow(weging)
summary(weging$schoolweging) # roughly 20 to 40
count(weging, n_locations) # schools with several locations


# ---- 5. join ---------------------------------------------------------------

# inner_join keeps only schools present in both tables
school_rq3 <- advice |>
  inner_join(test_results, by = "INSTELLINGSCODE") |>
  inner_join(provider, by = "INSTELLINGSCODE") |>
  inner_join(weging, by = "INSTELLINGSCODE")

# how many schools survive each step?
join_log <- tibble(
  step = c("advice", "+ test results", "+ provider", "+ schoolweging"),
  n_schools = c(
    nrow(advice),
    nrow(inner_join(advice, test_results, by = "INSTELLINGSCODE")),
    nrow(
      advice |>
        inner_join(test_results, by = "INSTELLINGSCODE") |>
        inner_join(provider, by = "INSTELLINGSCODE")
    ),
    nrow(school_rq3)
  )
)
join_log

# checks
nrow(school_rq3) == n_distinct(school_rq3$INSTELLINGSCODE) # TRUE
colSums(is.na(school_rq3)) # should be all 0


# ---- 6. validate and filter ------------------------------------------------

# how many schools would each minimum size remove?
tibble(min_size = c(5, 10, 15, 20)) |>
  mutate(
    n_removed = map_int(min_size, ~ sum(school_rq3$n_advised < .x))
  )

school_rq3 <- school_rq3 |>
  filter(n_advised >= min_pupils)

# final checks
nrow(school_rq3)
summary(
  select(school_rq3, avg_advice, pct_maths_1s, pct_reading_2f, schoolweging)
)
# sanity check: advice should rise with test results
cor(school_rq3$avg_advice, school_rq3$pct_maths_1s) # expect clearly positive


# ---- 7. save ---------------------------------------------------------------

dir.create("data/processed", showWarnings = FALSE)
saveRDS(school_rq3, "data/processed/school_rq3.rds")