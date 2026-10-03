source("scripts/00-packages.R")
source("scripts/01-get-data.R")


# ---- 1. schooladviezen: how many pupils nationally got each advice -----------

advice_cols <- c(
  "VSO", "PRO", "VMBO_B", "VMBO_B_K", "VMBO_K", "VMBO_K_GT",
  "VMBO_GT", "VMBO_GT_HAVO", "HAVO", "HAVO_VWO", "VWO"
)

schooladviezen |>
  pivot_longer(all_of(advice_cols), names_to = "advice", values_to = "n") |>
  mutate(
    n = suppressWarnings(as.numeric(n)),  # "<5" becomes NA
    advice = factor(advice, levels = advice_cols)
  ) |>
  group_by(advice) |>
  summarise(total = sum(n, na.rm = TRUE)) |>
  ggplot(aes(advice, total)) +
  geom_col(fill = "#4C92D8") +
  coord_flip() +
  labs(x = NULL, y = "Number of pupils", title = "School advice, 2024-2025") +
  theme_minimal()

# ---- 2. referentieniveaus: national totals per subject and level -------------

ref_cols <- c(
  "REKENEN_LAGER1F", "REKENEN_1F", "REKENEN_1S",
  "LV_LAGER1F", "LV_1F", "LV_2F",
  "TV_LAGER1F", "TV_1F", "TV_2F"
)

referentieniveaus |>
  pivot_longer(
    all_of(ref_cols),
    names_to = c("subject", "level"),
    names_pattern = "(.+)_(LAGER1F|1F|1S|2F)",
    values_to = "n"
  ) |>
  mutate(
    n = suppressWarnings(as.numeric(n)),  # "<5" becomes NA
    subject = recode(
      subject,
      REKENEN = "Maths", LV = "Reading", TV = "Spelling & grammar"
    ),
    level = recode(level, LAGER1F = "Below 1F", `1S` = "1S / 2F", `2F` = "1S / 2F"),
    level = factor(level, levels = c("Below 1F", "1F", "1S / 2F"))
  ) |>
  group_by(subject, level) |>
  summarise(total = sum(n, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(subject, total, fill = level)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("Below 1F" = "#F87E63", "1F" = "grey70", "1S / 2F" = "#4C92D8")
  ) +
  labs(
    x = NULL, y = "Share of pupils", fill = "Level reached",
    title = "Reference levels reached, 2024-2025"
  ) +
  theme_minimal()

# ---- 3. schoolweging: how the scores are spread across schools ---------------

ggplot(schoolweging, aes(schoolweging)) +
  geom_histogram(binwidth = 0.5, fill = "#4C92D8", colour = "white") +
  geom_vline(
    xintercept = mean(schoolweging$schoolweging, na.rm = TRUE),
    linetype = "dashed"
  ) +
  labs(
    x = "School weighting (higher = more disadvantaged)",
    y = "Number of schools",
    title = "School weighting, 2024-2025"
  ) +
  theme_minimal()

# ---- 4. eindscores: how many pupils took each providers test -----------------

eindscores |>
  select(ends_with("_AANTAL")) |>
  pivot_longer(everything(), names_to = "provider", values_to = "n") |>
  mutate(
    n = suppressWarnings(as.numeric(n)),  # "<5" becomes NA
    provider = str_remove(provider, "_AANTAL")
  ) |>
  group_by(provider) |>
  summarise(total = sum(n, na.rm = TRUE)) |>
  ggplot(aes(reorder(provider, total), total)) +
  geom_col(fill = "#4C92D8") +
  coord_flip() +
  labs(
    x = NULL, y = "Number of pupils tested",
    title = "Doorstroomtoets pupils per test provider, 2024-2025"
  ) +
  theme_minimal()

