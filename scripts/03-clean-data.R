# 03-clean-data.R
#
# Additional updates from Sophie:
#   - Main outcome is now pct_havo_plus; avg_advice (1-6 scale) is kept
#     as a robustness check
#   - Added language (pct_language_2f) and a combined `score`: the average
#     of maths, reading and language
#   - Added school_name, for labelling schools in plots
#   - Paths use here::here(), so the script also works when sourced from
#     an .Rmd in report/
#   - Counts are summed with na.rm = TRUE, so schools with an empty cell
#     are no longer dropped silently
#   - Added a check of the correlation between the three subjects
#
# Builds one clean table for RQ3 (school_rq3), with ONE ROW PER SCHOOL:
#   - pct_havo_plus   : % HAVO-or-higher advice              (main outcome)
#   - avg_advice      : average advice on a 1-6 scale        (robustness)
#   - pct_maths_1s    : % of pupils reaching 1S in maths     (test result)
#   - pct_reading_2f  : % of pupils reaching 2F in reading   (test result)
#   - pct_language_2f : % of pupils reaching 2F in language  (test result)
#   - score           : average of the three percentages     (main predictor)
#   - provider        : test provider used by the school     (control)
#   - schoolweging    : disadvantage score of the school     (moderator)
#   - n_advised       : pupils with an advice                (weight / filter)
#   - school_name     : name of the school (first location)  (labels)
#
# HOW TO USE
#   1. Open the project via the .Rproj file
#   2. Run once: source(here::here("scripts", "03-clean-data.R"))
#      -> creates `school_rq3` and saves data/processed/school_rq3.rds
#   3. Later (also in the report .Rmd), load it quickly with:
#      school_rq3 <- readRDS(here::here("data", "processed", "school_rq3.rds"))
#
# To change an assumption (robustness checks), edit section 0 and rerun.

# here() builds paths from the project root, so this script also works
# when it is sourced from an .Rmd in report/
source(here::here("scripts", "00-packages.R"))
source(here::here("scripts", "01-get-data.R"))


# ---- 0. Settings -----------------------------------------------------------

# Replace "<5" (1-4 pupils) with 2.5, the midpoint.
# Robustness: can be set to 1 or 4 before sourcing this script.
if (!exists("lt5_value")) lt5_value <- 2.5

# Keep only schools with at least 10 advised pupils.
# Robustness: rerun with 5 and 20.
min_pupils <- 10

# Helper: turn a count column that may contain "<5" into numbers
fix_counts <- function(x) {
  x <- as.character(x)
  x[x == "<5"] <- lt5_value
  as.numeric(x)
}


# ---- 1. Advice per school --------------------------------------------------

# Numeric advice scale (robustness check): combined advices sit halfway
# between their two tracks
advice_scale <- c(
  PRO = 1, VMBO_B = 2, VMBO_B_K = 2.5, VMBO_K = 3, VMBO_K_GT = 3.5,
  VMBO_GT = 4, VMBO_GT_HAVO = 4.5, HAVO = 5, HAVO_VWO = 5.5, VWO = 6
)
scale_cols <- names(advice_scale)
all_advice_cols <- c("VSO", scale_cols, "ADVIES_NIET_MOGELIJK")

advice <- schooladviezen |>
  # Keep regular primary schools only (drop special primary, Sbo)
  filter(SOORT_PO == "Bo") |>
  # Replace "<5" with 2.5
  mutate(across(all_of(all_advice_cols), fix_counts)) |>
  # Add up the locations of each school
  group_by(INSTELLINGSCODE) |>
  summarise(
    across(all_of(all_advice_cols), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    # Pupils on the PRO-VWO scale (VSO and "no advice" left out)
    n_advised = rowSums(pick(all_of(scale_cols))),
    # Main outcome: % HAVO, HAVO/VWO or VWO advice
    pct_havo_plus = 100 * (HAVO + HAVO_VWO + VWO) / n_advised,
    # Robustness outcome: average advice on the 1-6 scale
    avg_advice = rowSums(
      across(all_of(scale_cols), ~ .x * advice_scale[[cur_column()]])
    ) / n_advised,
    n_vso = VSO,
    n_no_advice = ADVIES_NIET_MOGELIJK
  ) |>
  # Drop schools without any advice on the scale
  filter(n_advised > 0) |>
  select(
    INSTELLINGSCODE, n_advised, pct_havo_plus, avg_advice,
    n_vso, n_no_advice
  )

# Checks
nrow(advice) == n_distinct(advice$INSTELLINGSCODE) # should be TRUE
summary(advice$pct_havo_plus) # between 0 and 100
summary(advice$avg_advice) # between 1 and 6


# ---- 2. Test results per school --------------------------------------------

ref_cols <- c(
  "REKENEN_LAGER1F", "REKENEN_1F", "REKENEN_1S",
  "LV_LAGER1F", "LV_1F", "LV_2F",
  "TV_LAGER1F", "TV_1F", "TV_2F"
)

test_results <- referentieniveaus |>
  # File contains Bo schools only, so no SOORT_PO filter needed
  # Replace "<5" with 2.5
  mutate(across(all_of(ref_cols), fix_counts)) |>
  # Add up the locations of each school
  group_by(INSTELLINGSCODE) |>
  summarise(
    across(all_of(ref_cols), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    # Pupils who took each part of the test
    n_maths = REKENEN_LAGER1F + REKENEN_1F + REKENEN_1S,
    n_reading = LV_LAGER1F + LV_1F + LV_2F,
    n_language = TV_LAGER1F + TV_1F + TV_2F,
    # % reaching the target level per subject
    pct_maths_1s = 100 * REKENEN_1S / n_maths,
    pct_reading_2f = 100 * LV_2F / n_reading,
    pct_language_2f = 100 * TV_2F / n_language,
    # Main predictor: average of the three subjects (equal weights)
    score = (pct_maths_1s + pct_reading_2f + pct_language_2f) / 3
  ) |>
  # Drop schools without results for one of the subjects
  filter(n_maths > 0, n_reading > 0, n_language > 0) |>
  select(
    INSTELLINGSCODE, n_maths, pct_maths_1s, pct_reading_2f,
    pct_language_2f, score
  )

# Checks
nrow(test_results) == n_distinct(test_results$INSTELLINGSCODE) # TRUE
summary(test_results$score) # between 0 and 100
# Correlation between the subjects (justifies the average score)
cor(select(test_results, pct_maths_1s, pct_reading_2f, pct_language_2f))


# ---- 3. Test provider per school -------------------------------------------

provider <- eindscores |>
  # Keep regular primary schools only
  filter(SOORT_PO == "Bo") |>
  select(INSTELLINGSCODE, ends_with("_AANTAL")) |>
  # One row per school location per provider
  pivot_longer(
    -INSTELLINGSCODE,
    names_to = "provider",
    values_to = "n_tested"
  ) |>
  mutate(
    provider = str_remove(provider, "_AANTAL"),
    # Replace "<5" with 2.5
    n_tested = fix_counts(n_tested)
  ) |>
  # Add up locations, then keep the provider that tested the most pupils
  group_by(INSTELLINGSCODE, provider) |>
  summarise(n_tested = sum(n_tested, na.rm = TRUE), .groups = "drop") |>
  filter(n_tested > 0) |>
  group_by(INSTELLINGSCODE) |>
  slice_max(n_tested, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(INSTELLINGSCODE, provider)

# Checks
nrow(provider) == n_distinct(provider$INSTELLINGSCODE) # TRUE
count(provider, provider, sort = TRUE) # schools per provider


# ---- 4. School name per school ---------------------------------------------

# Used only for labelling schools in plots: one name per school
# (the first location's name)
school_names <- eindscores |>
  select(INSTELLINGSCODE, school_name = INSTELLINGSNAAM_VESTIGING) |>
  group_by(INSTELLINGSCODE) |>
  slice(1) |>
  ungroup()


# ---- 5. Schoolweging per school --------------------------------------------

weging <- schoolweging |>
  # Drop locations without a schoolweging
  filter(!is.na(schoolweging)) |>
  # OVT looks like "00AP|C1": the part before "|" is the INSTELLINGSCODE
  mutate(INSTELLINGSCODE = str_remove(OVT, "\\|.*$")) |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    # Weighted by pupils when all counts are known, plain mean otherwise
    schoolweging = if (anyNA(aantal_leerlingen)) {
      mean(schoolweging)
    } else {
      weighted.mean(schoolweging, w = aantal_leerlingen)
    },
    n_locations = n(),
    .groups = "drop"
  )

# Checks
nrow(weging) == n_distinct(weging$INSTELLINGSCODE) # TRUE
summary(weging$schoolweging) # roughly 20 to 40


# ---- 6. Join everything ----------------------------------------------------

# Keep only schools present in all tables (name is optional: left_join)
school_rq3 <- advice |>
  inner_join(test_results, by = "INSTELLINGSCODE") |>
  inner_join(provider, by = "INSTELLINGSCODE") |>
  inner_join(weging, by = "INSTELLINGSCODE") |>
  left_join(school_names, by = "INSTELLINGSCODE")

# How many schools survive each step?
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

# Checks
nrow(school_rq3) == n_distinct(school_rq3$INSTELLINGSCODE) # TRUE
colSums(is.na(school_rq3)) # all 0 (school_name may have a few)


# ---- 7. Keep schools with at least 10 advised pupils -----------------------

# How many schools would each minimum size remove?
tibble(min_size = c(5, 10, 15, 20)) |>
  mutate(
    n_removed = map_int(min_size, ~ sum(school_rq3$n_advised < .x))
  )

school_rq3 <- school_rq3 |>
  filter(n_advised >= min_pupils)

# Final checks
nrow(school_rq3)
summary(select(school_rq3, pct_havo_plus, score, schoolweging))
# Sanity check: advice should rise with test results (clearly positive)
cor(school_rq3$pct_havo_plus, school_rq3$score)


# ---- 8. Save ---------------------------------------------------------------

# Only save the main version, so robustness runs don't overwrite it
if (lt5_value == 2.5) {
  dir.create(here::here("data", "processed"), showWarnings = FALSE)
  saveRDS(school_rq3, here::here("data", "processed", "school_rq3.rds"))
}