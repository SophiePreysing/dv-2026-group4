knitr::opts_chunk$set(echo = TRUE) # set default echo = TRUE for all code blocks
rm(list = ls()) # Remove any existing objects in memory

# installs (if missing) and loads every package - add any new packages to
# scripts/00-packages.R, not here
source("../scripts/00-packages.R")

# downloads the data into data/raw/ if you don't already have it, then loads
# eindscores, referentieniveaus, schooladviezen and schoolweging
source(here("scripts", "01-get-data.R"))


school_provider <- eindscores |>
  select(INSTELLINGSCODE, matches("_(AANTAL|GEM)$")) |>
  pivot_longer(
    cols = matches("_(AANTAL|GEM)$"),
    names_to = c("provider", ".value"),
    names_pattern = "(.+)_(AANTAL|GEM)"
  ) |>
  # DUO writes "<5" for small counts -> NA
  mutate(AANTAL = as.numeric(AANTAL)) |>
  filter(!is.na(AANTAL), AANTAL > 0, !is.na(GEM), GEM > 0) |>
  group_by(INSTELLINGSCODE, provider) |>
  summarise(AANTAL = sum(AANTAL), .groups = "drop")

# most schools only use one provider - take whichever tested the most students
primary_provider <- school_provider |>
  group_by(INSTELLINGSCODE) |>
  slice_max(AANTAL, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(INSTELLINGSCODE, provider)

school_weights <- schoolweging |>
  mutate(INSTELLINGSCODE = str_remove(OVT, "\\|.*$")) |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    schoolweging = weighted.mean(schoolweging, w = aantal_leerlingen),
    .groups = "drop"
  )

count_cols <- c(
  "REKENEN_LAGER1F", "REKENEN_1F", "REKENEN_1S",
  "LV_LAGER1F", "LV_1F", "LV_2F",
  "TV_LAGER1F", "TV_1F", "TV_2F"
)

ref_levels <- referentieniveaus |>
  mutate(across(all_of(count_cols), ~ suppressWarnings(as.numeric(.)))) |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    rekenen_n = REKENEN_LAGER1F + REKENEN_1F + REKENEN_1S,
    taal_lv_n = LV_LAGER1F + LV_1F + LV_2F,
    taal_tv_n = TV_LAGER1F + TV_1F + TV_2F,
    score = (
      100 * REKENEN_1S / rekenen_n +
        100 * LV_2F / taal_lv_n +
        100 * TV_2F / taal_tv_n
    ) / 3,
    n_students = rekenen_n
  ) |>
  filter(rekenen_n > 0, taal_lv_n > 0, taal_tv_n > 0) |>
  select(INSTELLINGSCODE, score, n_students)

school_data <- ref_levels |>
  inner_join(primary_provider, by = "INSTELLINGSCODE") |>
  left_join(school_weights, by = "INSTELLINGSCODE")

