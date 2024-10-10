# The Dynamic Effects of Weather Shocks on Agricultural Production
# Linear Local Projections
# Robustness without last year (2015 data removed from sample)
library(tidyverse)
library(fastDummies)
library(lmtest)
library(sandwich)

## 1. Load Data and functions----

# The baseline dataset
load("../data/output/df_lp.rda")

# Functions useful to shape the data for local projections
source("../weatherperu/R/format_data.R")

# Load function in utils
source("../weatherperu/R/utils.R")

# Load detrending functions
source("../weatherperu/R/detrending.R")

# Estimation functions
source("../weatherperu/R/estimations.R")


# Remove last year from the data
df <- df |> filter(year < 2015)


## 2. Estimation----

crops <- df$product_eng |> unique()
weather_variables <- c("temp_max_dev", "precip_piscop_sum_dev")
control_variables <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "price_int_inf")

nb_h <- 14

resul_lp <- vector(mode = "list", length = length(crops))
for (i_crop in 1:length(crops)) {
  print(crops[i_crop])
  resul_lp[[i_crop]] <- estimate_linear_lp(
    df,
    horizons = nb_h,
    y_name = "y_new_normalized",
    group_name = "region_id",
    detrend = TRUE,
    add_month_fe = FALSE,
    add_intercept = FALSE,
    crop_name = crops[i_crop],
    control_names = control_variables,
    weather_names = weather_variables,
    std = "Cluster",
    # std = "nw",
    other_var_to_keep = "y_new"
  )
}

save(resul_lp, file = "output/resul_lp_piscop_until2014.rda")

# load("output/resul_lp_piscop_until2014.rda")

## 3. Plots ----

# Plotting the IRFs

df_irfs_lp <- map(resul_lp, "coefs") |> 
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


df_irfs_lp_ci <- 
  df_irfs_lp |> 
  select(horizon, crop, name, matches("^(lower)|^(upper)", perl = TRUE)) |> 
  pivot_longer(
    cols = matches("^(lower)|^(upper)", perl = TRUE),
    names_pattern = "(.*)_(95|68)$",
    names_to = c(".value", "level")
  ) |> 
  mutate(level = str_c(level, "%"))

# Duration of the growing season
gs_duration_df <- tribble(
  ~crop, ~tc,
  "Rice", 4,
  "Maize", 5,
  "Potato", 6,
  "Cassava", 9
)

p_lp_lin <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci |> filter(horizon <= !!nb_h),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = level),
    alpha = .2
  ) +
  geom_line(
    data = df_irfs_lp |> filter(horizon <= !!nb_h),
    mapping = aes(x = horizon, y = shock_1_sd),
    colour = "#0072B2") +
  geom_hline(yintercept = 0, colour = "gray40") +
  geom_vline(
    data = gs_duration_df, 
    mapping = aes(xintercept = tc),
    colour = "#D55E00", linetype = "dashed") +
  ggh4x::facet_grid2(
    name~crop, 
    # scales = "free_y", 
    axes = "all",
    # independent = "y", 
    switch = "y") +
  scale_x_continuous(breaks = seq(0, nb_h, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    "C.I. level", 
    values = c("68%" = "gray10", "95%" = "gray60")
  ) +
  theme_paper() +
  theme(strip.placement = "outside")

p_lp_lin

save(df_irfs_lp, df_irfs_lp_ci, file = "output/df_irfs_lp_piscop_until2014.rda")


df_irfs_lp_2014 <- df_irfs_lp
df_irfs_lp_ci_2014 <- df_irfs_lp_ci

# 4. Comparison----

# Let us compare with LPs obtained using observations until 2015
load("output/df_irfs_lp_piscop.rda")

df_irfs_lp_comparison <- df_irfs_lp |> 
  mutate(data_type = "Full sample") |> 
  bind_rows(
    df_irfs_lp_2014 |> 
      mutate(data_type = "Without 2015 data")
  ) |> 
  mutate(
    data_type = factor(
      data_type,
      levels = c("Full sample", "Without 2015 data")
    )
  )


df_irfs_lp_ci_comparison <- df_irfs_lp_ci |> 
  mutate(data_type = "Full sample") |> 
  bind_rows(
    df_irfs_lp_ci_2014 |> 
      mutate(data_type = "Without 2015 data")
  ) |> 
  mutate(
    data_type = factor(
      data_type,
      levels = c("Full sample", "Without 2015 data")
    )
  )


p_lp_lin_comparison_2014 <- ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci_comparison |> filter(level == "68%", horizon <= !!nb_h),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = data_type, colour = data_type, 
      linetype = data_type),
    alpha = .2
  ) +
  geom_line(
    data = df_irfs_lp_comparison |> filter(horizon <= !!nb_h),
    mapping = aes(x = horizon, y = shock_1_sd, colour = data_type, 
                  linetype = data_type),
    linewidth = 1.1
  ) +
  scale_colour_manual(
    NULL, 
    values = c("Full sample" = "#56B4E9", "Without 2015 data" = "#CC79A7")
  ) +
  geom_hline(yintercept = 0, colour = "gray40") +
  geom_vline(
    data = gs_duration_df, 
    mapping = aes(xintercept = tc),
    colour = "#D55E00", linetype = "dashed") +
  ggh4x::facet_grid2(
    name~crop,
    # scales = "free_y", 
    axes = "all",
    # independent = "y",
    switch = "y") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, nb_h, by = 2)) +
  labs(x = "Horizon (in months)", y = NULL) +
  scale_fill_manual(
    NULL,
    values = c("Full sample" = "#56B4E9", "Without 2015 data" = "#CC79A7")
  ) +
  scale_linetype_discrete(NULL) +
  theme_paper() +
  theme(strip.placement = "outside")

p_lp_lin_comparison_2014

if (1 == 0) {
  # Save the plots in PDF using pdflatex
  p_lp_lin_comparison_2014 <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_lp_ci_comparison |> 
        filter(level == "68%", horizon <= !!nb_h),
      mapping = aes(
        x = horizon,
        ymin = lower, ymax = upper, fill = data_type, colour = data_type,
        linetype = data_type),
      alpha = .2, linetype = "dashed"
    ) +
    geom_line(
      data = df_irfs_lp_comparison |> filter(horizon <= !!nb_h),
      mapping = aes(
        x = horizon, y = shock_1_sd, colour = data_type,
        linetype = data_type
      ),
      linewidth = 1.1
    ) +
    geom_hline(yintercept = 0, colour = "gray40") +
    geom_vline(
      data = gs_duration_df, 
      mapping = aes(xintercept = tc),
      colour = "#D55E00", linetype = "dashed") +
    ggh4x::facet_grid2(
      name~crop, 
      # scales = "free_y", 
      axes = "all",
      # independent = "y",
      switch = "y") +
    scale_x_continuous(breaks = seq(0, nb_h, by = 2)) +
    scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
    labs(x = "Horizon", y = NULL) +
    scale_colour_manual(
      NULL, 
      values = c("Full sample" = "#56B4E9", "Without 2015 data" = "#CC79A7")
    ) +
    scale_fill_manual(
      NULL,
      values = c("Full sample" = "#56B4E9", "Without 2015 data" = "#CC79A7")
    ) +
    scale_linetype_discrete(NULL) +
    theme_paper() +
    theme(strip.placement = "outside")
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin_comparison_2014, 
    path = "../../../figs/", 
    filename = "fig_lp_lin_until_2014",
    width = 7,
    height = 4.5
  )
}