# 05-explain-data.R
#
# Part 2: EXPLANATORY version
#
# Research question:
#   Among schools with similar test results, do schools with a more
#   disadvantaged population give lower secondary-school advice?
#
# Approach:
#   - Only the three most used test providers (LIB, IEP, ROUTE8): together
#     they cover most schools, and the smaller providers have too few
#     schools for reliable estimates
#   - INFERENCE: one regression explains each school's advice from its test
#     results, test provider and schoolweging. The schoolweging effect
#     answers the research question
#   - The advice gap shown in the plot comes from that same model
#   - Robustness checks at the end
#
# Structure:
#   0. Setup and settings
#   1. Data
#   2. Model
#   3. Advice gap per school
#   4. Plot: average advice gap per provider
#   5. Robustness checks
#
# HOW TO USE: open the .Rproj, then source this script. It runs
# 03-clean-data.R first, so school_rq3 is always built fresh.


# =============================================================================
# 0. SETUP AND SETTINGS -------------------------------------------------------
# =============================================================================

source(here::here("scripts", "03-clean-data.R")) # creates school_rq3

# main outcome: average advice on the 1-6 scale (unit = tracks).
# pct_havo_plus is used as a robustness check in section 5.
outcome_var <- "avg_advice"

# test results: % reaching the higher target level per subject
# (1S maths, 2F reading, 2F spelling and grammar), as separate predictors so
# the model weighs each subject itself
test_vars <- c("pct_maths_1s", "pct_reading_2f", "pct_language_2f")

# the three most used test providers
main_providers <- c("LIB", "IEP", "ROUTE8")

# distinct, colourblind-safe colours per provider (Okabe-Ito)
provider_colours <- c(
  "LIB" = "#0072B2", "IEP" = "#E69F00", "ROUTE8" = "#009E73"
)


# =============================================================================
# 1. DATA ---------------------------------------------------------------------
# =============================================================================

explain_data <- school_rq3 |>
  filter(provider %in% main_providers) |>
  # LIB (largest) first, as the reference category in the model
  mutate(provider = factor(provider, levels = main_providers))

# number of schools per provider: report this in the text
count(explain_data, provider)


# =============================================================================
# 2. MODEL --------------------------------------------------------------------
# =============================================================================
#
# Advice explained by test results, test provider AND schoolweging.
# - Provider is included because reaching a level is easier on some tests
#   than others, so test results only compare within the same test
# - Schools are weighted by number of advised pupils, so tiny schools count
#   less
# The schoolweging coefficient answers the research question: at the same
# test results and the same test, how does advice change with the school's
# population?

fit_main <- lm(
  reformulate(c(test_vars, "provider", "schoolweging"), response = outcome_var),
  data = explain_data,
  # n_advised = number of pupils at each school who got an advice on the 
  # PRO–VWO scale
  # makes a school with 60 pupils count six times as much as one with 10, so 
  # tiny schools with unreliable averages count less.
  weights = n_advised
)
summary(fit_main)

# x10 makes the effect easier to picture (school 25 vs 35) without changing
# the result: -0.052 per point is the same as -0.52 per 10 points.
effect_10 <- 10 * coef(fit_main)[["schoolweging"]]
effect_10_ci <- 10 * confint(fit_main)["schoolweging", ]
effect_p <- summary(fit_main)$coefficients["schoolweging", 4]

effect_10
effect_10_ci
effect_p

# does the schoolweging effect differ between the three providers?
fit_provider <- update(fit_main, . ~ . + provider:schoolweging)
anova(fit_main, fit_provider)


# =============================================================================
# 3. ADVICE GAP PER SCHOOL ----------------------------------------------------
# =============================================================================
#
# Advice gap = Actual advice - Expected advice
# Expected advice = what an average-population school with the same results 
# and test would advise.
# Above 0: the school advises higher than a school with the same results,
# the same test and an average population. Below 0: lower.

average_population <- explain_data |>
  mutate(schoolweging = mean(schoolweging))

explain_data <- explain_data |>
  mutate(
    expected_advice = predict(fit_main, newdata = average_population),
    advice_gap = .data[[outcome_var]] - expected_advice
  )


# =============================================================================
# 4. PLOT: AVERAGE ADVICE GAP PER PROVIDER ------------------------------------
# =============================================================================

# one line per provider (the same lines geom_smooth draws), used to place
# the labels at the right end of each line
fit_lines <- lm(
  advice_gap ~ schoolweging * provider,
  data = explain_data, weights = n_advised
)

# labels at the right end of each line, with the number of schools
line_labels <- explain_data |>
  group_by(provider) |>
  summarise(
    schoolweging = max(schoolweging),
    n_schools = n(),
    .groups = "drop"
  ) |>
  mutate(
    advice_gap = predict(fit_lines, newdata = pick(everything())),
    label = paste0(provider, " (n = ", n_schools, ")")
  )

# text on the plot
plot_title <- paste0(
  "Schools with more disadvantaged pupils give lower advice than their ",
  "test results predict,\ncompared to schools with more affluent pupils."
)

effect_label <- sprintf(
  "+10 school weighting = %+.2f tracks\n(95%% CI %.2f to %.2f, p %s)",
  effect_10, effect_10_ci[1], effect_10_ci[2],
  format.pval(effect_p, digits = 2, eps = 0.001)
)

plot_caption <- paste0(
  "Dots: individual schools. Lines: average advice gap per test provider, ",
  "with 95% interval.\n",
  "N = ", nrow(explain_data), " regular primary schools with 10+ advised ",
  "pupils, 2024-2025. Excluded: special primary schools (no school ",
  "weighting),\nschools with missing data, and schools with fewer than 10 ",
  "advised pupils. Counts hidden as '<5' were replaced by 2.5.\n",
  "The effect is similar for other advice measures, test-score ",
  "definitions, weighting and minimum school sizes."
)

final_plot <- ggplot(
  explain_data,
  aes(schoolweging, advice_gap, colour = provider, fill = provider)
) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
  geom_point(alpha = 0.08, size = 0.6) +
  geom_smooth(
    aes(weight = n_advised),
    method = "lm", formula = y ~ x, alpha = 0.2, linewidth = 1.3
  ) +
  geom_text(
    data = line_labels, aes(label = label),
    hjust = -0.05, size = 4, fontface = "bold"
  ) +
  # explain what above and below zero means, right next to the zero line
  annotate(
    "text", x = 34, y = 0.04,
    label = "\u2191 higher advice than comparable schools",
    hjust = 0, vjust = 0, colour = "grey30", size = 4, fontface = "italic"
  ) +
  annotate(
    "text", x = min(explain_data$schoolweging), y = -0.04,
    label = "\u2193 lower advice than comparable schools",
    hjust = 0, vjust = 1, colour = "grey30", size = 4, fontface = "italic"
  ) +
  # model result in the empty top-right corner, in bold
  annotate(
    "label",
    x = max(explain_data$schoolweging), y = 0.55,
    label = effect_label,
    hjust = 1, vjust = 1, size = 4, fontface = "bold", fill = "white"
  ) +
  scale_colour_manual(values = provider_colours) +
  scale_fill_manual(values = provider_colours) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
  coord_cartesian(ylim = c(-0.8, 0.8)) +
  labs(
    title = plot_title,
    subtitle = paste0(
      "Advice gap = actual advice minus the advice of a comparable school ",
      "(same test results, same test, average school weighting).\n",
      "1 track = one step on the advice scale, e.g. from VMBO-GT to HAVO."
    ),
    x = "School weighting (higher = more disadvantaged pupils)",
    y = "Advice gap (tracks)",
    caption = plot_caption
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, colour = "grey40", size = 10),
    panel.grid.minor = element_blank()
  )

final_plot


# =============================================================================
# 5. ROBUSTNESS CHECKS --------------------------------------------------------
# =============================================================================
#
# Does the schoolweging effect hold under other reasonable choices?
# Each row refits the model with one change and reports the effect of
# +10 school weighting.

schoolweging_effect <- function(data, check, outcome, predictors,
                                weighted = TRUE) {
  data$w <- if (weighted) data$n_advised else 1
  fit <- lm(
    reformulate(c(predictors, "provider", "schoolweging"), response = outcome),
    data = data,
    weights = w
  )
  tibble(
    check = check,
    outcome = outcome,
    effect_10 = 10 * coef(fit)[["schoolweging"]],
    ci_low = 10 * confint(fit)["schoolweging", 1],
    ci_high = 10 * confint(fit)["schoolweging", 2],
    p_value = summary(fit)$coefficients["schoolweging", 4],
    n_schools = nrow(data)
  )
}

robustness <- bind_rows(
  schoolweging_effect(
    explain_data, "Main model", "avg_advice", test_vars
  ),
  schoolweging_effect(
    explain_data, "% HAVO-or-higher advice", "pct_havo_plus", test_vars
  ),
  schoolweging_effect(
    explain_data, "Combined test score", "avg_advice", "score"
  ),
  schoolweging_effect(
    explain_data, "Not weighted by school size", "avg_advice", test_vars,
    weighted = FALSE
  ),
  schoolweging_effect(
    filter(explain_data, n_advised >= 20), "Only schools with 20+ pupils",
    "avg_advice", test_vars
  )
)
robustness

# TODO: "<5" robustness (1 and 4 instead of 2.5). Change lt5_value in
# section 0 of 03-clean-data.R, rerun this script, and add the effect to the
# report. Set it back to 2.5 afterwards.
# Note: avg_advice is in tracks and pct_havo_plus in percentage points, so
# compare their DIRECTION and significance, not size.
# p-values shown as 0 are smaller than R can display: report as p < 0.001.

# =============================================================================
# 6. WHERE ON THE ADVICE SCALE IS THE EFFECT? ---------------------------------
# =============================================================================
#
# Answers Annie's question about how the gap plays out across the advice scale
# The effect shows up at every level of the advice scale, and it's biggest at 
# the HAVO line.
# % of pupils advised AT OR ABOVE each level, per school. If schoolweging
# has an effect at every cut-off, the gap is spread across the whole scale.

advice_cutoffs <- schooladviezen |>
  filter(SOORT_PO == "Bo") |>
  mutate(across(all_of(scale_cols), fix_counts)) |>
  group_by(INSTELLINGSCODE) |>
  summarise(across(all_of(scale_cols), sum), .groups = "drop") |>
  mutate(
    n_scale = rowSums(pick(all_of(scale_cols))),
    pct_vmbo_k_plus = 100 * (n_scale - PRO - VMBO_B - VMBO_B_K) / n_scale,
    pct_vmbo_gt_plus = 100 * rowSums(
      pick(VMBO_GT, VMBO_GT_HAVO, HAVO, HAVO_VWO, VWO)
    ) / n_scale,
    pct_vwo = 100 * VWO / n_scale
  ) |>
  select(INSTELLINGSCODE, pct_vmbo_k_plus, pct_vmbo_gt_plus, pct_vwo)

cutoff_data <- explain_data |>
  left_join(advice_cutoffs, by = "INSTELLINGSCODE")

cutoff_effects <- bind_rows(
  schoolweging_effect(cutoff_data, ">= VMBO-K", "pct_vmbo_k_plus", test_vars),
  schoolweging_effect(
    cutoff_data, ">= VMBO-GT", "pct_vmbo_gt_plus", test_vars
  ),
  schoolweging_effect(cutoff_data, ">= HAVO", "pct_havo_plus", test_vars),
  schoolweging_effect(cutoff_data, "VWO", "pct_vwo", test_vars)
)
cutoff_effects