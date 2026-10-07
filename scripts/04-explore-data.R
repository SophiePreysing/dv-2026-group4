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
# test results = % reaching the higher target level (1S maths, 2F reading, 2F language),
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
  geom_point(alpha = 0.15, size = 1, colour = "grey40") +
  geom_smooth(
    method = "loess", formula = y ~ x,
    colour = trend_colour, linewidth = 1.2, se=TRUE
  ) +
  geom_point(data = highlight_gap, colour = "white", size = 3) +
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
<<<<<<< HEAD
      "from test results"
=======
      "\nfrom test results (first, simplified look)"
>>>>>>> 646691e8d0986a4367b77eb757b4ea9d94588701
    ),
    x = "Schoolweging (higher = more disadvantaged)",
    y = "Advice gap (tracks above/below prediction)",
    caption = "Above 0: advises higher than predicted. Below 0: lower."
  ) +
  theme_explore +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey35", size = 11,
                                      margin = margin(b = 12)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x       = element_text(margin = margin(t = 10)),
    axis.title.y       = element_text(margin = margin(r = 10)),
    plot.caption       = element_text(hjust = 0, colour = "grey40", size = 9,
                                      lineheight = 1.15, margin = margin(t = 14)),
    plot.margin        = margin(14, 18, 14, 14)
  )


plot_1

plot_1 <- ggplot(explore_gap, aes(schoolweging, advice_gap)) +
  # Zero line -> linetype legend
  geom_hline(aes(yintercept = 0, linetype = "No gap (advice = prediction)"),
             colour = "grey50") +
  geom_point(alpha = 0.15,
             size = 1,
             colour = "grey40") +
  # Trend + confidence band -> colour/fill legend
  geom_smooth(
    aes(colour = "Smoothed trend (95% CI)", fill   = "Smoothed trend (95% CI)"),
    method = "loess",
    formula = y ~ x,
    linewidth = 1.2,
    alpha = 0.15,
    se = TRUE
  ) +
  # Highlighted schools -> shape legend
  geom_point(
    data = highlight_gap,
    aes(shape = "Largest gaps (20+ advised pupils)"),
    fill = highlight_colour,
    colour = "white",
    stroke = 0.7,
    size = 3.2
  ) +
  geom_text(
    data = highlight_gap,
    aes(label = label, hjust = label_hjust),
    colour = highlight_colour,
    size = 3,
    lineheight = 0.9
  ) +
  scale_linetype_manual(name = NULL,
                        values = c("No gap (advice = prediction)" = "dashed")) +
  scale_colour_manual(name = NULL,
                      values = c("Smoothed trend (95% CI)" = trend_colour)) +
  scale_fill_manual(name = NULL,
                    values = c("Smoothed trend (95% CI)" = trend_colour)) +
  scale_shape_manual(name = NULL,
                     values = c("Largest gaps (20+ advised pupils)" = 21)) +
  labs(
    title = paste(
      "Do schools advise higher or lower",
      "than their test results predict?"
    ),
    subtitle = paste(
      "Advice gap = actual average advice minus advice expected",
      "from test results"
    ),
    x = "Schoolweging (higher = more disadvantaged)",
    y = "Advice gap (tracks above/below prediction)",
    caption = "Advice gap above 0: advises higher than predicted. Below 0: lower."
  ) +
  theme_explore +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(
      colour = "grey35",
      size = 11,
      margin = margin(b = 12)
    ),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x       = element_text(margin = margin(t = 10)),
    axis.title.y       = element_text(margin = margin(r = 10)),
    plot.caption       = element_text(
      hjust = 0,
      colour = "grey40",
      size = 9,
      lineheight = 1.15,
      margin = margin(t = 14)
    ),
    plot.margin        = margin(14, 18, 14, 14),
    # legend styling
    legend.position      = "top",
    legend.justification = "left",
    legend.text          = element_text(size = 8),
    legend.key.width     = unit(0.5, "cm")
  )

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


subject_colours <- c(
  "Maths (1S)"    = "#E69F00",
  "Reading (2F)" = "#0072B2",
  "Language (2F)" = "#009E73"
)

plot_2 <- school_rq3_long |>
  filter(!is.na(schoolweging), !is.na(test_score)) |>
  ggplot(aes(x = schoolweging, y = test_score, colour = subject)) +
  # Background observations
  geom_point(alpha = 0.06, size = 0.7) +
  
  # Highlight schools with extreme percentages
  geom_point(
    data = extreme_scores |>
      filter(!is.na(schoolweging), !is.na(test_score)),
    shape = 21,
    fill = NA,
    colour = "grey25",
    stroke = 0.35,
    size = 1.6,
    alpha = 0.4,
    show.legend = FALSE
  ) +
  # Emphasise the trends
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1.3
  ) +
  scale_colour_manual(values = subject_colours) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x)
      paste0(x, "%")
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Attainment and school disadvantage",
    subtitle = "Higher schoolweging indicates a more disadvantaged student population",
    x = "Schoolweging",
    y = "% of students reaching target level",
    colour = NULL,
    caption = paste(
      "Lines: LOESS trends | Each point represents a school–subject observation.",
      "Black circles: 0% or 100% attainment.",
      sep = "\n"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(colour = "grey35", margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    axis.title = element_text(colour = "grey25"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(
      hjust = 0,
      colour = "grey40",
      size = 9,
      margin = margin(t = 12)
    ),
    plot.margin = margin(12, 16, 12, 12)
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

plot_2

# =============================================================================
# PLOT 3 - <TOPIC> (Sander) ----------------------------------------------------
# =============================================================================
#
# <one or two lines: what does this plot look at, and why?>
# Plot 3 plots providers measured by schoolweging in a boxplot. It orders the
# test providers to be arranged from lowest median schoolweging to highest
# median schoolweging. A relative higher value for schoolweging means a relative
# higher school disadvantage level. 

provider_colours <- c(
  "ROUTE8" = "#E69F00",  # orange
  "LIB"    = "#56B4E9",  # sky blue
  "IEP"    = "#CC79A7",  # reddish purple
  "AMN"    = "#D55E00",  # vermillion
  "DIA"    = "#0072B2",  # blue
  "DOE"    = "#009E73"   # bluish green
)



# national median reference
nat_median <- median(school_rq3$schoolweging, na.rm = TRUE)

# n labels per provider for x-axis
provider_n <- school_rq3 |>
  count(provider) |>
  mutate(label = paste0(provider, "\n(n=", n, ")"))

school_rq3 <- school_rq3 |>
  left_join(provider_n, by = "provider")

plot_3 <- ggplot(school_rq3,
                 aes(
                   x    = reorder(provider, schoolweging, FUN = median),
                   y    = schoolweging,
                   fill = provider,
                   colour = provider
                 )) +
  geom_hline(
    aes(yintercept = nat_median, linetype = "National median"),
    colour = "grey40",
    linewidth = 0.5
  ) +
  geom_violin(alpha = 0.3, linewidth = 0) + # shows full distribution of each provider
  geom_boxplot(
    aes(linetype = "Provider median"),
    alpha = 0.8,
    width = 0.3,
    outlier.shape = NA,
    colour = "grey20",
    linewidth = 0.6,
    key_glyph = "path"
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c("National median" = "dashed", "Provider median" = "solid"),
    guide  = guide_legend(override.aes = list(
      colour = "grey40", linewidth = 0.5
    ))
  )  +
  scale_fill_manual(values = provider_colours) +
  scale_colour_manual(values = provider_colours) +
  scale_x_discrete(labels = setNames(provider_n$label, provider_n$provider)) +
  # add sample size to variables
  guides(fill = "none", colour = "none") +
  labs(
    title    = "School disadvantage by primary test provider",
    subtitle = "Higher schoolweging indicates a more disadvantaged
    student population",
    x        = NULL,
    y        = "Schoolweging"
  ) +
  theme_explore +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text     = element_text(size = 11),
    axis.title.y  = element_text(size = 12)
  )

plot_3


# =============================================================================
# COMBINE ALL PLOTS -----------------------------------------------------------
# =============================================================================

top_row <- plot_grid(
  plot_1,
  plot_2,
  labels = c("A", "B"),
  label_size = 14,
  ncol = 2,
  align = "h"
)

explore_figure <- plot_grid(
  top_row,
  plot_3,
  labels = c("", "C"),
  label_size = 14,
  ncol = 1,
  rel_heights = c(1, 1.1)
)

explore_figure


