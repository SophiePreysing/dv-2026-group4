# 05-explain-data.R
#
# Part 2: EXPLANATORY version
#
# Research question:
#   Do schools with similar test results give similar secondary-school
#   advice, regardless of how disadvantaged their student population is?
#
# Hypotheses:
#   H1: At the same test results, schools with more disadvantaged pupils
#       (a higher schoolweging) advise a smaller share of pupils to HAVO or
#       higher. (Main effect of schoolweging, controlling for test results)
#   H2: This gap depends on test results: the difference between advantaged
#       and disadvantaged schools is larger at higher test results.
#       (Interaction between test results and schoolweging)
#
# Model:
#   pct_havo_plus ~ score_c * schoolweging_c + provider,
#   weighted by n_advised
#   - pct_havo_plus  : % of pupils per school with a HAVO, HAVO/VWO or VWO
#                      advice, out of all pupils with a PRO-VWO advice
#                      (outcome)
#   - score_c        : centred average % of pupils reaching the target level
#                      in maths, reading and language
#   - schoolweging_c : centred schoolweging (moderator)
#   - provider       : control, because tests differ in how often pupils
#                      reach the target level
#
# Structure:
#   0. Setup
#   1. Data
#   2. Model
#   3. Predicted lines at the 10th, 50th and 90th percentile of schoolweging
#   4. Size of the gap at an average score
#   5. Final plot (version 1 and version 2)
#   6. Robustness checks
#
# HOW TO USE: open the .Rproj, then source this script. It runs
# 03-clean-data.R first, so school_rq3 is always built fresh.
#
# -----------------------------------------------------------------------------
# REASONING BEHIND THE VARIABLES (for the report)
# -----------------------------------------------------------------------------
#
# Score: we use the average % of pupils reaching the target level in maths,
# reading and language. All three subjects declined with schoolweging in a
# similar way in our exploration, and they correlate moderately at school
# level (r = 0.57-0.68), so the average is a more stable measure of overall
# performance than a single subject.
#
# Provider as control: schools using different tests have similar
# populations, but the share of pupils reaching the target level differs
# strongly between tests. The same score therefore means something different
# per test, and so may the advice that follows from it. A school using an
# "easier" test can have a high score but give relatively less HAVO+ advice,
# simply because its teachers know the score overstates their pupils' level.
# To avoid mistaking such test differences for an advice gap, we control for
# provider and compare schools with the same score on the same test.
#
# Schoolweging as moderator: our exploration suggested that more
# disadvantaged schools give less HAVO+ advice than their results predict,
# and more advantaged schools more. The model tests whether this holds
# (H1: level difference) and whether advice follows results more or less
# closely depending on schoolweging (H2: interaction).
#
# Point size: point size shows the number of pupils with an advice;
# percentages of small schools are less reliable, which is also why the
# model is weighted by school size.
#
# Why % of HAVO-or-higher instead of an average advice score: a
# percentage is easier for policymakers to interpret, needs only one cut-off
# instead of assuming equal distances between tracks (PRO = 1, ..., VWO = 6),
# and captures the most consequential boundary: HAVO largely determines
# access to higher education.
#
# Why we centre score and schoolweging: because the model includes an
# interaction, each main effect is estimated where the other variable is
# zero, which is meaningless here (no school has a schoolweging or score of
# 0). We therefore centre both variables on their mean, so the schoolweging
# coefficient shows the difference in HAVO+ advice for a school with average
# test results. Centring does not change the model's fit or predictions.


# =============================================================================
# 0. SETUP --------------------------------------------------------------------
# =============================================================================

install.packages("ggtext")
library(ggtext)

library(tidyverse)

source(here::here("scripts", "03-clean-data.R")) # creates school_rq3


# =============================================================================
# 1. DATA ---------------------------------------------------------------------
# =============================================================================

# Centre score and schoolweging; LIB (largest provider) as reference
model_data <- school_rq3 |>
  mutate(
    score_c = score - mean(score),
    schoolweging_c = schoolweging - mean(schoolweging),
    provider = relevel(factor(provider), ref = "LIB")
  )

# Number of schools per provider
count(model_data, provider, sort = TRUE)

# Provider groups differ strongly in size (LIB: n = 2683, IEP: n = 2311,
# Route 8: n = 333, DIA: n = 296, AMN: n = 64, DOE: n = 31), so estimates
# for the smaller providers are less precise. We keep all providers:
# provider is only a control variable and its coefficients are not
# interpreted, so this does not affect our conclusions.


# =============================================================================
# 2. MODEL --------------------------------------------------------------------
# =============================================================================

# Weighted by n_advised: percentages of larger schools are more reliable
rq3_model <- lm(
  pct_havo_plus ~ score_c * schoolweging_c + provider,
  data = model_data,
  weights = n_advised
)

summary(rq3_model)
confint(rq3_model) # 95% confidence intervals

# Results (same test provider, LIB as reference; R-squared = 0.66):
#   - score_c (0.58): 1 percentage point (pp) higher score -> 0.58 pp more
#     HAVO+ advice, at average schoolweging (95% CI 0.56 to 0.60)
#   - schoolweging_c (-1.76): 1 point more disadvantaged -> 1.76 pp less
#     HAVO+ advice, at the same (average) test results
#     (95% CI -1.84 to -1.68). Two schools with the same results but
#     10 points apart in schoolweging differ by about 18 pp. Supports H1.
#   - interaction (-0.033, 95% CI -0.037 to -0.028): for every point of
#     schoolweging, the score-advice slope becomes 0.033 weaker. Higher test
#     results translate less into HAVO+ advice in more disadvantaged
#     schools, so the gap is largest among high-scoring schools. Supports H2.
#   - intercept (43.8%, 95% CI 43.4 to 44.2): predicted HAVO+ advice for a
#     LIB school with average test results and average schoolweging.
#   - test results, schoolweging and provider explain about two thirds of
#     the variation in HAVO+ advice between schools (R-squared = 0.66).
# All p < 0.001.
#
# Interpretation: these results are not causal. They show that schools with
# similar test results but more disadvantaged pupils give less HAVO+ advice,
# but not why, e.g. whether this reflects teachers' expectations or
# information about pupils that the test does not capture.


# =============================================================================
# 3. PREDICTED LINES AT THE 10TH, 50TH AND 90TH PERCENTILE --------------------
# =============================================================================
#
# geom_smooth() cannot draw trend lines from our own model. So we let the
# model predict % HAVO+ advice across all scores, for a LIB school, at low,
# medium and high schoolweging, and plot those predictions. This keeps the
# plot and the model equal.

# 1. The three schoolweging values for the lines (10th, 50th, 90th pct)
sw_levels <- quantile(model_data$schoolweging, c(0.1, 0.5, 0.9))

# 2. All combinations of 100 scores and the 3 schoolweging values,
#    for a LIB school (the most common provider)
pred_grid <- expand_grid(
  score = seq(min(model_data$score), max(model_data$score),
              length.out = 100),
  schoolweging = sw_levels
) |>
  mutate(
    score_c = score - mean(model_data$score),
    schoolweging_c = schoolweging - mean(model_data$schoolweging),
    provider = factor("LIB", levels = levels(model_data$provider))
  )

# 3. Ask the model for the predicted % HAVO+ (fit) and its 95% CI
pred_grid <- bind_cols(
  pred_grid,
  as_tibble(predict(rq3_model, newdata = pred_grid, interval = "confidence"))
)


# =============================================================================
# 4. SIZE OF THE GAP AT AN AVERAGE SCORE --------------------------------------
# =============================================================================
#
# Both schools have the same (average) % reaching the target level and use
# LIB; only their schoolweging differs (10th pct: advantaged vs 90th pct:
# disadvantaged). The arrow in the plot shows the difference in their
# predicted % HAVO+ advice.

gap_data <- tibble(schoolweging = unname(sw_levels[c(1, 3)])) |>
  mutate(
    score = mean(model_data$score),
    score_c = 0,
    schoolweging_c = schoolweging - mean(model_data$schoolweging),
    provider = factor("LIB", levels = levels(model_data$provider))
  )
gap_data$fit <- predict(rq3_model, newdata = gap_data)

gap_size <- round(gap_data$fit[1] - gap_data$fit[2], 1)
gap_size # percentage points less HAVO+ advice in more disadvantaged schools


# =============================================================================
# 5. FINAL PLOT ---------------------------------------------------------------
# =============================================================================
#
# Although the trend lines are drawn for a LIB school, the model and the
# points in the plot include all schools and all providers, with provider
# as a control variable. To make predictions, every variable needs a value,
# so we use LIB (the most common test). Provider only shifts all lines up or
# down by the same amount, so their shape and the gap between them are the
# same for every test.
#

# -----------------------------------------------------------------------------
# Final version: one point size, plain-language labels, bold gap arrow
# -----------------------------------------------------------------------------

line_labels <- pred_grid |>
  filter(score == max(score)) |>
  mutate(
    label = c(
      "Advantaged\nschools",
      "Average\nschools",
      "Disadvantaged\nschools"
    )[match(schoolweging, sw_levels)]
  )

final_plot <- ggplot() +
  # Schools (all providers)
  geom_point(
    data = model_data,
    aes(x = score, y = pct_havo_plus, colour = schoolweging),
    size = 1.2, alpha = 0.07
  ) +
  # Uncertainty bands of the lines
  geom_ribbon(
    data = pred_grid,
    aes(
      x = score, ymin = lwr, ymax = upr,
      fill = schoolweging, group = schoolweging
    ),
    alpha = 0.4
  ) +
  # Expected advice at three levels of disadvantage
  geom_line(
    data = pred_grid,
    aes(x = score, y = fit, colour = schoolweging, group = schoolweging),
    linewidth = 1.5
  ) +
  # Line labels
  geom_text(
    data = line_labels,
    aes(x = score, y = fit, label = label, colour = schoolweging),
    hjust = 0, nudge_x = 1, size = 4.2, lineheight = 0.9,
    fontface = "bold"
  ) +
  # Gap arrow: 
  annotate(
    "segment",
    x = mean(model_data$score), xend = mean(model_data$score),
    y = gap_data$fit[1], yend = gap_data$fit[2],
    arrow = arrow(ends = "last", length = unit(0.25, "cm"), type = "closed"),
    colour = "#FF8C00", linewidth = 1
  ) +
  # Gap label: "17 percentage points" in the same orange as the arrow
  annotate(
    "richtext",
    x = mean(model_data$score), y = max(gap_data$fit) + 3,
    label = paste0(
      "<b>Same test results:<br>",
      "<span style='color:#FF8C00'>", round(gap_size),
      " percentage points</span><br>",
      "fewer pupils advised<br>HAVO or higher</b>"
    ),
    hjust = 1.05, vjust = 0, size = 4.5, lineheight = 1.2,
    fill = alpha("white", 0.7), label.colour = NA
  ) +
  scale_colour_viridis_c(
    name = NULL,
    breaks = range(model_data$schoolweging),
    labels = c("Least\ndisadvantaged\nschools", "Most\ndisadvantaged\nschools"),
    direction = -1, end = 0.9,
    guide = guide_colourbar(barheight = unit(5, "cm"))
  ) +
  scale_fill_viridis_c(direction = -1, end = 0.9, guide = "none") +
  scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    labels = \(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.3))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = \(x) paste0(x, "%")
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste(
      "At the same test results, schools with more disadvantaged pupils",
      "advise\nfewer pupils to HAVO or higher"
    ),
    subtitle = paste0(
      "Lines show the average share of pupils advised HAVO or higher for ",
      "advantaged, average and disadvantaged\nschools, estimated by a ",
      "regression model. The gap grows as test results rise."
    ),
    x = "Test results: pupils reaching the target level",
    y = "Pupils advised HAVO or higher",
    caption = paste0(
      "Dots: schools (all test providers), coloured by schoolweging ",
      "(how disadvantaged a school's pupils are). Test results: average % ",
      "of pupils reaching\nthe target level in maths, reading and ",
      "language. Lines: regression predictions at the 10th, 50th and 90th ",
      "percentile of schoolweging,\ncontrolling for test provider (shown ",
      "for LIB, the most common test); shaded bands: 95% confidence ",
      "intervals. N = ", format(nrow(model_data), big.mark = ","),
      "\nregular primary schools with 10+ advised pupils, 2024-2025; ",
      "'<5' counts replaced by 2.5. Source: DUO."
    )
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, colour = "grey25"),
    plot.caption = element_text(
      size = 9.5, colour = "grey40", hjust = 0, lineheight = 1.15
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.ticks = element_blank(),
    panel.grid.minor = element_blank()
  )

final_plot



# =============================================================================
# 6. ROBUSTNESS CHECKS --------------------------------------------------------
# =============================================================================
#
# Does the schoolweging effect (H1) and the interaction (H2) hold under
# other reasonable choices? Each model changes one thing.
#
# TODO: "<5" robustness (1 and 4 instead of 2.5). Change the "<5" value in
# 03-clean-data.R, rerun this script, and add the result to the report.
# Set it back to 2.5 afterwards.

# Maths only, instead of the average of three subjects
rob1_model_data <- model_data |>
  mutate(maths_c = pct_maths_1s - mean(pct_maths_1s))

rob1_model <- lm(
  pct_havo_plus ~ maths_c * schoolweging_c + provider,
  data = rob1_model_data,
  weights = n_advised
)

# Average advice on the 1-6 scale, instead of % HAVO+ advice
rob2_model <- lm(
  avg_advice ~ score_c * schoolweging_c + provider,
  data = model_data,
  weights = n_advised
)

# Without AMN and DOE (smallest providers)
rob3_model_data <- model_data |>
  filter(!provider %in% c("DOE", "AMN")) |>
  droplevels()

rob3_model <- lm(
  pct_havo_plus ~ score_c * schoolweging_c + provider,
  data = rob3_model_data,
  weights = n_advised
)


# Refit the main model on data with "<5" replaced by 1 instead of 2.5

env_lt5_1 <- new.env()
env_lt5_1$lt5_value <- 1
source(here::here("scripts", "03-clean-data.R"), local = env_lt5_1)

rob4_model_data <- env_lt5_1$school_rq3 |>
  mutate(
    score_c = score - mean(score),
    schoolweging_c = schoolweging - mean(schoolweging),
    provider = relevel(factor(provider), ref = "LIB")
  )

rob4_model <- lm(
  pct_havo_plus ~ score_c * schoolweging_c + provider,
  data = rob4_model_data,
  weights = n_advised
)


# Refit the main model on data with "<5" replaced by 4 instead of 2.5
env_lt5_4 <- new.env()
env_lt5_4$lt5_value <- 4
source(here::here("scripts", "03-clean-data.R"), local = env_lt5_4)

rob5_model_data <- env_lt5_4$school_rq3 |>
  mutate(
    score_c = score - mean(score),
    schoolweging_c = schoolweging - mean(schoolweging),
    provider = relevel(factor(provider), ref = "LIB")
  )

rob5_model <- lm(
  pct_havo_plus ~ score_c * schoolweging_c + provider,
  data = rob5_model_data,
  weights = n_advised
)


# Schoolweging effect and interaction (estimate and p-value) per model
get_effect <- function(model, interaction_term) {
  coefs <- coef(summary(model))
  tibble(
    "schoolweging estimate" = coefs["schoolweging_c", "Estimate"],
    "schoolweging p-value" = coefs["schoolweging_c", "Pr(>|t|)"],
    "interaction estimate" = coefs[interaction_term, "Estimate"],
    "interaction p-value" = coefs[interaction_term, "Pr(>|t|)"]
  )
}

robustness <- bind_rows(
  "Main model" = get_effect(rq3_model, "score_c:schoolweging_c"),
  "Maths only" = get_effect(rob1_model, "maths_c:schoolweging_c"),
  "Average advice (1-6)" = get_effect(rob2_model, "score_c:schoolweging_c"),
  "Without AMN and DOE" = get_effect(rob3_model, "score_c:schoolweging_c"),
  .id = "model",
  "<5 replaced by 1" = get_effect(rob4_model, "score_c:schoolweging_c"),
  "<5 replaced by 4" = get_effect(rob5_model, "score_c:schoolweging_c")
)

robustness

# The results are robust in direction and significance. The negative
# schoolweging effect and the negative interaction remain significant
# (all p < 0.001) when using maths results only (-2.10 pp per point of
# schoolweging), when excluding the two smallest providers (-1.76 pp,
# practically unchanged) and when measuring advice as the average on a
# 1-6 scale (-0.05 per point, i.e. about half a track lower advice for a
# 10-point difference in schoolweging).

# The size of the effect depends on the "<5" assumption. "<5" means 1 to
# 4 pupils, so replacing it with 1 or 4 gives the most extreme cases.
# Replacing "<5" with 1 or 4 instead of 2.5 keeps the effect of
# schoolweging on advice (at the same test results) negative and
# significant, but stronger with 1 and weaker with 4. The interaction
# also stays negative and significant, but the gap grows more with test
# results with 4 and less with 1. 2.5 (the midpoint) is our best
# estimate.