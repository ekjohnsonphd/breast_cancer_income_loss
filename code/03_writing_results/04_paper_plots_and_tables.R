# This script generates publication-ready plots for ATT, CATE, and DiD results
# for the income loss analysis following breast cancer diagnosis.

# Load libraries and custom theme -----------------------------------------
library(data.table)
library(tidyverse)
library(ggthemes)
library(cowplot)

source("ggplot_theme_Publication/ggplot_theme_Publication-2.R")

# Load and preprocess CATE data ---------------------------------------------
cate <- fread("data/heterogeneous_effects.csv")
cate[, signif := "Not significant"]
cate[p_val < 0.05, signif := "Significant"]

cate[, inc_formal := "Personal disposable income"]
cate[
  income == "household",
  inc_formal := "Household equivalized disposable income"
]

cate[
  model == "deaths_cfactual",
  inc_formal := "Personal disposable income, mortality adjusted"
]

# Format Table 1 summary stats ---------------------------------------------
t1 <- fread("data/table 1.csv")
colnames(t1) <- c(
  "Variable",
  "dbcg_mean",
  "dbcg_median",
  "dbcg_sd",
  "o_mean",
  "o_median",
  "o_sd"
)
t1[Variable %in% c("alder", "per_ind", "fam_ind"), `:=`(
  dbcg = paste0("Mean: ", dbcg_mean, "\nMedian: ", dbcg_median, "\nSD:", dbcg_sd),
  overall = paste0("Mean: ", o_mean, "\nMedian: ", o_median, "\nSD:", o_sd)
)]
t1[is.na(dbcg), dbcg := paste0(dbcg_mean, " (", dbcg_median, ")")]
t1[is.na(overall), overall := paste0(o_mean, " (", o_median, ")")]
fwrite(
  t1,
  "data/table 1 formatted.csv"
)

# CATE plots - Main model (personal and household income) ----------------------
# Plot CATE by year (main models)
g1 <- ggplot(cate[model == "final" &
  stratifier == "index" & retired == 0]) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(x = stratification,
      y = att,
      ymin = lower,
      ymax = upper,
      shape = signif,
      color = inc_formal
    )
  ) +
  scale_colour_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_continuous(labels = scales::label_currency(prefix = "€")) +
  labs(
    x = "Years since diagnosis",
    y = "Absolute personal income loss, \n 2023 EUR",
    shape = bquote("Significance at " ~ alpha == 0.05),
    color = "Outcome"
  ) +
  theme_Publication() +
  theme(legend.position = "bottom")

g2 <- ggplot(cate[model == "final" &
  stratifier == "index" & retired == 0]) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(
      x = stratification,
      y = as.numeric(str_remove(pct, "%")) / 100,
      ymin = as.numeric(str_remove(pct_lower, "%")) / 100,
      ymax = as.numeric(str_remove(pct_upper, "%")) / 100,
      shape = signif,
      color = inc_formal
    )
  ) +
  scale_colour_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "Years since diagnosis",
    y = "Relative personal income loss,  \n percent of control income",
    shape = "Statistical significance (0.05)",
    color = "Outcome"
  ) +
  theme_Publication()

leg <- cowplot::get_plot_component(g1, "guide-box", return_all = TRUE)[[3]]
plts <- plot_grid(
  g1 + theme(legend.position = "None"),
  g2 + theme(legend.position = "None")
)
plot_grid(plts, leg, ncol = 1, rel_heights = c(1, 0.25))

# CATE plots - stratification by education, age, etc. --------------------------
cate[is.na(retired) &
  income == "personal" &
  stratifier == "retired" & stratification == 0]
cate[stratification == "Higher education", stratification := "Postgraduate"]

cate_plt <- cate[retired == 0 &
  model %in% c("final", "deaths_cfactual") &
  income %in% c("personal", "household") &
  stratifier %in% c("edu", "match_age", "per_inc_qt", "cci", "util")]

cate_plt[, stratifier := factor(
  stratifier,
  levels = c("edu", "per_inc_qt", "match_age", "cci", "util"),
  labels = c(
    "Education",
    "Personal income quartile",
    "Age",
    "CCI",
    "Hospital utilization"
  )
)]
cate_plt[
  !(stratification %in% c("3 to 5", "6 to 10")),
  stratification := str_replace(stratification, "to", " to ")
]
cate_plt[, stratification :=
           str_to_sentence(str_replace(stratification, "_", " "))]
cate_plt <- cate_plt[!(stratification %in% c(
  "-1", "65 to 69", "70 to 74",
  "75 to 79", "80 to 84", "85 to 89"
))]
cate_plt[, n_treat := as.numeric(n_treat)]
cate_plt[, stratification := fct_relevel(
  stratification,
  "Basic",
  "Upper secondary",
  "Vocational training",
  "Bachelor",
  "Postgraduate",
  "0",
  "1",
  "2",
  "3",
  "4+",
  "3 to 5",
  "6 to 10",
  "11+"
)]
cate_plt[, pct_numeric := as.numeric(str_remove(pct, "%"))]
cate_plt[, pct_cat := cut(
  pct_numeric,
  breaks = c(-5, -1, 0, 1, 2, 3, 4, 5, 6, 10, 15, 20),
  labels = c(
    "-5% to -1%",
    "-1% to 0%",
    "0% to 1%",
    "1% to 2%",
    "2% to 3%",
    "3% to 4%",
    "4% to 5%",
    "5% to 6%",
    "6% to 10%",
    "10% to 15%",
    "15% to 20%"
  )
)]

ggplot(cate_plt[model == "final" & income == "personal"]) +
  geom_vline(aes(xintercept = 0)) +
  geom_pointrange(aes(
    y = stratification,
    x = att,
    xmin = lower,
    xmax = upper,
    color = pct_cat,
    shape = signif
  )) +
  facet_wrap(. ~ stratifier, scales = "free_y") +
  theme_Publication() +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(labels = scales::label_currency(prefix = "€")) +
  labs(
    x = "Personal income loss, 2023 EUR",
    y = NULL,
    color = "Percentage of control income",
    shape = bquote("Significance at " ~ alpha == 0.05)
  )


# Sensitivity analysis - DID --------------------------------------------------
did <- fread("data/did_coefficients.csv")
did[, estimate := -estimate]
did[, significant := ifelse(p.value > 0.05, "Not significant", "Significant")]
did_years <- did[str_detect(term, "index::.?\\d")]
did_years[, year_index := as.integer(str_remove(term, "index::"))]

ggplot(did_years) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(
      x = year_index,
      y = -estimate,
      ymin = -estimate - 1.96 * std.error,
      ymax = -estimate + 1.96 * std.error,
      color = significant
    )
  ) +
  scale_y_continuous(labels = scales::label_currency(prefix = "€")) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Years to diagnosis", y = "Income difference (2023 Euros)", 
       color = NULL) +
  theme_Publication()

ggplot(did[str_detect(term, "treat") &
  !str_detect(term, "index::.?\\d:treat")]) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(
      x = term,
      y = estimate,
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    )
  ) +
  scale_y_continuous(labels = scales::label_currency(prefix = NULL)) +
  labs(x = "Years since event", y = "Income change coefficient (2023 dkk)") +
  theme_Publication() +
  coord_flip()

# Sensitivity analysis plot - different match models -----------------------
ggplot(cate[income == "personal" &
  stratifier == "index" & retired == 0 &
  model != "deaths_cfactual" &
  model != "3y_lookback"]) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(
      x = stratification,
      y = att,
      ymin = lower,
      ymax = upper,
      color = model
    ),
    alpha = 0.7
  ) +
  labs(x = "Years since diagnosis", 
       y = "Personal disposable income loss, 2023€",
       color = "Match model") +
  scale_y_continuous(labels = scales::label_currency(prefix = "€")) +
  scale_colour_Publication(
    labels = c(
      "5 year lookback",
      "Age only",
      "Age + Education",
      "Primary model",
      "Non-health variables"
    )
  ) +
  theme_Publication()

# CATE plots - Mortality adjusted model -------------------------------------
# Provided in the supplementary materials
g1a <- ggplot(cate[model == "deaths_cfactual" &
  stratifier == "index" & retired == 0]) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(
      x = stratification,
      y = att,
      ymin = lower,
      ymax = upper
    ),
    color = "navy"
  ) +
  scale_y_continuous(labels = scales::label_currency(prefix = "€")) +
  labs(
    x = "Years since diagnosis",
    y = "Absolute effect, 2023 EUR",
    shape = "Statistical significance (0.05)",
    color = "Outcome"
  ) +
  theme_Publication() +
  theme(legend.position = "bottom")

g2a <- ggplot(cate[model == "deaths_cfactual" &
  stratifier == "index" & retired == 0]) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(
    aes(
      x = stratification,
      y = as.numeric(str_remove(pct, "%")) / 100,
      ymin = as.numeric(str_remove(pct_lower, "%")) / 100,
      ymax = as.numeric(str_remove(pct_upper, "%")) / 100
    ),
    color = "navy"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "Years since diagnosis",
    y = "Relative effect",
    shape = "Statistical significance (0.05)",
    color = "Outcome"
  ) +
  theme_Publication()

plot_grid(g1a, g2a, ncol = 1)