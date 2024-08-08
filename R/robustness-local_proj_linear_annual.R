# The Dynamic Effects of Weather Shocks on Agricultural Production
# Linear Local Projections with annual data
library(tidyverse)
library(fastDummies)
library(lmtest)
library(sandwich)

## 1. Load Data and functions----

# The baseline dataset
load("../data/output/df_lp_annual.rda")

# Functions useful to shape the data for local projections
source("../weatherperu/R/format_data-annual.R")

# Load function in utils
source("../weatherperu/R/utils.R")

# Load detrending functions
source("../weatherperu/R/detrending-annual.R")

# Estimation functions
source("../weatherperu/R/estimations-annual.R")

## 2. Estimation----

crops <- df$product_eng |> unique()
weather_variables <- c("temp_max_dev", "precip_piscop_sum_dev")
control_variables <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "int_price_inf")

# df = df
# horizons = 2
# y_name = "y_new_normalized"
# group_name = "region_id"
# detrend = TRUE
# add_year_fe = FALSE
# add_intercept = FALSE
# crop_name = "Rice"
# control_names = control_variables
# weather_names = weather_variables
# std = "Cluster"
# other_var_to_keep = "y_new"
# share_geo = NULL
# transition_name = NULL
# transition_method = NULL


resul_lp_year <- map(
  crops, ~ estimate_linear_lp(
    df = df,
    horizons = 4,
    y_name = "y_new_normalized",
    group_name = "region_id",
    detrend = TRUE,
    add_year_fe = FALSE,
    add_intercept = FALSE,
    crop_name = .x,
    control_names = control_variables,
    weather_names = weather_variables,
    std = "Cluster",
    other_var_to_keep = "y_new"
  )
)


save(resul_lp_year, file = "output/resul_lp_year.rda")

## 3. Plots ----

# Plotting the IRFs

df_irfs_lp_year <- map(resul_lp_year, "coefs") |> 
  list_rbind() |> 
  filter(name %in% weather_variables) |> 
  mutate(
    shock_1_sd = value * std_shock,
    lower_95 = (value - qnorm(0.975) * std) * std_shock,
    upper_95 = (value + qnorm(0.975) * std) * std_shock,
    lower_68 = (value - qnorm(0.84)  * std) * std_shock,
    upper_68 = (value + qnorm(0.84)  * std) * std_shock
  ) |> 
  mutate(
    crop = factor(
      crop, 
      levels = c("Rice", "Dent corn", "Potato", "Cassava"),
      labels = c("Rice", "Maize", "Potato", "Cassava"))
  ) |> 
  mutate(
    name = factor(
      name,
      levels = c(
        "temp_max_dev",
        "precip_piscop_sum_dev"
      ),
      labels = c(
        "Temp. anomalies", 
        "Precip. anomalies"
      )
    )
  )


df_irfs_lp_ci_year <- 
  df_irfs_lp_year |> 
  select(horizon, crop, name, matches("^(lower)|^(upper)", perl = TRUE)) |> 
  pivot_longer(
    cols = matches("^(lower)|^(upper)", perl = TRUE),
    names_pattern = "(.*)_(95|68)$",
    names_to = c(".value", "level")
  ) |> 
  mutate(level = str_c(level, "%"))

p_lp_lin_year <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci_year |> filter(horizon <= 8),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = level),
    alpha = .2
  ) +
  geom_line(
    data = df_irfs_lp_year |> filter(horizon <= 8),
    mapping = aes(x = horizon, y = shock_1_sd),
    colour = "#0072B2") +
  geom_hline(yintercept = 0, colour = "#D55E00") +
  ggh4x::facet_grid2(
    name~crop, scales = "free_y", 
    independent = "y", switch = "y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    "C.I. level", 
    values = c("68%" = "gray10", "95%" = "gray60")
  ) +
  theme_paper() +
  theme(strip.placement = "outside")

p_lp_lin_year



save(
  df_irfs_lp_year, df_irfs_lp_ci_year, 
     file = "output/df_irfs_lp_year.rda"
)



if (1 == 0) {
  # Save the plots in PDF using pdflatex
  p_lp_lin_year <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_lp_ci_year |> 
        mutate(
          level = str_replace(level, pattern = "%", replacement = "\\\\%")
        ) |> 
        filter(horizon <= 8),
      mapping = aes(
        x = horizon,
        ymin = lower, ymax = upper, fill = level),
      alpha = .2
    ) +
    geom_line(
      data = df_irfs_lp_year |> filter(horizon <= 8),
      mapping = aes(x = horizon, y = shock_1_sd),
      colour = "#0072B2",
      linewidth = 1) +
    geom_hline(yintercept = 0, colour = "gray40") +
    ggh4x::facet_grid2(
      name~crop, scales = "free_y", 
      independent = "y", switch = "y") +
    scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
    labs(x = "Horizon", y = NULL) +
    scale_fill_manual(
      "C.I. level", 
      values = c("68\\%" = "gray10", "95\\%" = "gray60")
    ) +
    theme_paper() +
    theme(strip.placement = "outside")
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin_year, 
    path = "../../figs/", 
    filename = "fig_lp_lin_year",
    width = 7,
    height = 4.5)
}