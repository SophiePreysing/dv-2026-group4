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
fit_tests <- lm(avg_advice ~ pct_maths_1s + pct_reading_2f, data = school_rq3)

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
# PLOT 2 - <TOPIC> (Sophie) ----------------------------------------------------
# =============================================================================
#
# <one or two lines: what does this plot look at, and why?>

# plot_3 <- ggplot(school_rq3, aes(...)) +
#   ... +
#   theme_explore

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