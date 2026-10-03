# 04-explore-data.R
#
# Part 1 exploratory plots for RQ3:
#   "Do schools with similar test results give similar secondary-school
#    advice?"
#
# Structure:
#   0. Setup (shared)
#   PLOT 1 - Advice gap          (Natasha)
#   PLOT 2 - <topic>             (Sophie)
#   PLOT 3 - <topic>             (Sander)
#   Combine all plots
#
# Rules for working in this file:
#   - only edit your own PLOT block
#   - pull before you start, commit + push as soon as you're done
#   - each block ends with one object: plot_1, plot_2, plot_3
#
# All plots are EXPLORATORY: quick looks to understand the data and get
# feedback. The proper analysis follows in Part 2.


# =============================================================================
# 0. SETUP (shared) -----------------------------------------------------------
# =============================================================================

source("scripts/03-clean-data.R") # creates school_rq3
library(cowplot)

# Decide on shared colors (we can do this next week)
# Current colours: viridis palette (colourblind-safe), comes with ggplot2

viridis_colours <- viridisLite::viridis(10)
highlight_colour <- viridis_colours[1] # dark purple
trend_colour <- viridis_colours[5] # teal

# shared theme, so all plots look alike
theme_explore <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


# =============================================================================
# PLOT 1 - ADVICE GAP (Natasha) -----------------------------------------------
# =============================================================================
#
# For each school we compare its actual average advice with the advice
# expected from its test results alone. If schools with a more disadvantaged
# population (higher schoolweging) mostly advise LOWER than expected, school
# background is related to advice beyond what test results explain.
# Quick version: ignores test provider and school size. 

# ---- 1a. compute the advice gap ----

# expected advice from test results only (schoolweging left out on purpose)
# test results = % reaching the higher target level (1S maths, 2F reading),
# as the brief suggests; % below 1F is ignored here (we can add this in Part 2
# as a robustness check if we want).
fit_tests <- lm(avg_advice ~ pct_maths_1s + pct_reading_2f + pct_language_2f, data = school_rq3)

# advice gap = actual advice - expected advice (the model's residual)
explore_gap <- school_rq3 |>
  mutate(advice_gap = residuals(fit_tests))

# ---- 1b. schools to highlight ----

# largest positive and negative gap among schools with 20+ advised pupils
highlight_gap <- explore_gap |>
  # only consider schools with at least 20 pupils otherwise extreme gaps would
  # probably come from very small schools.
  filter(n_advised >= 20) |>
  filter(advice_gap == max(advice_gap) | advice_gap == min(advice_gap)) |>
  mutate(
    label = paste0(
      school_name, "\n", sprintf("%+.2f", advice_gap), " tracks"
    ),
    # put labels on the side with the most room
    label_hjust = if_else(schoolweging > 30, 1.1, -0.1)
  )

# ---- 1c. plot ----

plot_1 <- ggplot(explore_gap, aes(schoolweging, advice_gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.2, size = 1, colour = "grey40") +
  geom_smooth(
    method = "loess", formula = y ~ x,
    colour = trend_colour, linewidth = 1.2
  ) +
  geom_point(data = highlight_gap, colour = highlight_colour, size = 3) +
  geom_text(
    data = highlight_gap,
    aes(label = label, hjust = label_hjust),
    colour = highlight_colour, size = 3, lineheight = 0.9
  ) +
  labs(
    title = paste(
      "Do schools advise higher or lower",
      "than their test results predict?"
    ),
    subtitle = paste(
      "Advice gap = actual average advice minus advice expected",
      "from test results (first, simplified look)"
    ),
    x = "School weighting (higher = more disadvantaged pupils)",
    y = "Advice gap (tracks)",
    caption = "Above 0: advises higher than predicted. Below 0: lower."
  ) +
  theme_explore

plot_1


# =============================================================================
# PLOT 2 - Test results by school disadvantage, per subject (Sophie) ----------------------------------------------------
# =============================================================================

# Data Preparation:

school_rq3 <- readRDS(here::here("data", "processed", "school_rq3.rds"))

school_rq3_long <- school_rq3 |>
  rename(
    "Maths (1S)" = "pct_maths_1s",
    "Reading (2F)" = "pct_reading_2f",
    "Language (2F)" = "pct_language_2f"
  ) |>
  pivot_longer(
    c("Maths (1S)", "Reading (2F)", "Language (2F)"),
    names_to = "subject",
    values_to = "test_score"
  )

# Extreme scores only:
extreme_scores <- school_rq3_long |>
  filter(test_score %in% c(0, 100))

# Median number of students extreme vs all scores:
extreme_scores |> summarise(median_students = median(n_maths)) #9.5 students
school_rq3 |> summarise(median_students = median(n_maths)) #25 students

# Correlation between subjects:
school_rq3 |>
  filter(n_maths >= 20) |>
  select(pct_maths_1s, pct_reading_2f, pct_language_2f) |>
  cor(use = "complete.obs")


# Exploratory Plot:

# Plot trend line with Loess method: draws a flexible curved line,
# can show whether the relationship is straight or bends somewhere.

# All three subjects decline with schoolweging and follow similar trends.
# They correlate moderately between schools (r = 0.57-0.68), so we combine
# them into one average score. The large spread shows that schools with the
# same schoolweging differ widely in results. The extreme values of 0% and
# 100% (black circles) mostly come from small schools, where one or two
# students change the percentage a lot. Their percentages are therefore
# unreliable and add noise. We will set a minimum school size, so that the
# analysis is based on schools whose results reflect more than chance.

plot_2 <- school_rq3_long |>
  filter(!is.na(schoolweging)) |>
  ggplot(aes(x = schoolweging, y = test_score, colour = subject)) +
  geom_point(alpha = 0.08, size = 0.6) +
  geom_point(
    data = extreme_scores,
    shape = 21,
    colour = "black",
    size = 1.5,
    alpha = 0.5
  ) +
  geom_smooth(method = "loess", formula = y ~ x, linewidth = 1.2) +
  annotate(
    "text",
    x = 40, y = 106,
    label = "Black circles: 0% or 100%, mostly small schools",
    hjust = 1, size = 3.5
  ) +
  scale_colour_manual(
    values = c(
      "Maths (1S)" = "#E69F00",
      "Reading (2F)" = "#0072B2",
      "Language (2F)" = "#009E73"
    )
  ) +
  labs(
    x = "Schoolweging (higher = more disadvantaged)",
    y = "% of students reaching target level",
    colour = "Subject"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

plot_2

# =============================================================================
# PLOT 3 - <TOPIC> (Sander) ----------------------------------------------------
# =============================================================================
#
# <one or two lines: what does this plot look at, and why?>

# plot_3 <- ggplot(school_rq3, aes(...)) +
#   ... +
#   theme_explore


# =============================================================================
# COMBINE ALL PLOTS -----------------------------------------------------------
# =============================================================================
#
# Uncomment once plot_2 and plot_3 exist.

# explore_figure <- plot_grid(plot_1, plot_2, plot_3, nrow = 1)
# explore_figure