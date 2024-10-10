# The Dynamic Effects of Weather Shocks on Agricultural Production
# Linear Local Projections with quadratic effects
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

## 2. Estimation----

crops <- df$product_eng |> unique()

df <- 
  df |> 
  mutate(
    temp_max_dev_sq = temp_max_dev^2,
    precip_piscop_sum_dev_sq = precip_piscop_sum_dev^2
  )


weather_variables <- c(
  "temp_max_dev", "precip_piscop_sum_dev",
  "temp_max_dev_sq", "precip_piscop_sum_dev_sq"
)
control_variables <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "price_int_inf")

# Number of horizons
nb_h <- 14

resul_lp_quad <- map(
  crops, ~ estimate_linear_lp(
    df = df,
    horizons = nb_h,
    y_name = "y_new_normalized",
    group_name = "region_id",
    detrend = TRUE,
    add_month_fe = FALSE,
    add_intercept = FALSE,
    crop_name = .,
    control_names = control_variables,
    weather_names = weather_variables,
    std = "Cluster",
    other_var_to_keep = "y_new"
  )
)

save(resul_lp_quad, file = "output/resul_lp_quad.rda")

# load("output/resul_lp_quad.rda")

## 3. Plots ----

# Plotting the IRFs

df_irfs_lp_quad <- 
  map(resul_lp_quad, "coefs") |> 
  list_rbind() |> 
  filter(name %in% weather_variables) |> 
  mutate(
    order = ifelse(str_detect(name, "_sq$"), 2, 1),
    name = str_remove(name, "_sq$")
  ) |> 
  pivot_wider(
    names_from = order, 
    values_from = c(value, std, std_shock, median_shock, q05_shock, q95_shock), 
    names_prefix = 'order_') |> 
  rename(d = std_shock_order_1) |> 
  mutate(
    ir = value_order_1 * d + value_order_2 * d^2,
    # IC 95%
    ir_lower_95 = (value_order_1 - qnorm(0.975) * std_order_1) * d + 
      (value_order_2 - qnorm(0.975) * std_order_2) * d^2,
    ir_upper_95 = (value_order_1 + qnorm(0.975) * std_order_1) * d +
      (value_order_2 + qnorm(0.975) * std_order_2) * d^2,
    # IC 68%
    ir_lower_68 = (value_order_1 - qnorm(0.84) * std_order_1) * d + 
      (value_order_2 - qnorm(0.84) * std_order_2) * d^2,
    ir_upper_68 = (value_order_1 + qnorm(0.84) * std_order_1) * d +
      (value_order_2 + qnorm(0.84) * std_order_2) * d^2
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


df_irfs_lp_quad_ci <- 
  df_irfs_lp_quad |> 
  select(horizon, crop, name, matches("^(ir_lower)|^(ir_upper)", perl = TRUE)) |> 
  pivot_longer(
    cols = matches("^(ir_lower)|^(ir_upper)", perl = TRUE),
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

p_lp_lin_quad <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_quad_ci,
    mapping = aes(
      x = horizon,
      ymin = ir_lower, ymax = ir_upper, fill = level),
    alpha = .2
  ) +
  geom_line(
    data = df_irfs_lp_quad,
    mapping = aes(x = horizon, y = ir),
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

p_lp_lin_quad

if (1 == 0) {
  
  # To save plots in PDF using pdflatex
  p_lp_lin_quad <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_lp_quad_ci |> 
        mutate(
          level = str_replace(level, pattern = "%", replacement = "\\\\%")
        ),
      mapping = aes(
        x = horizon,
        ymin = ir_lower, ymax = ir_upper, fill = level),
      alpha = .2
    ) +
    geom_line(
      data = df_irfs_lp_quad,
      mapping = aes(x = horizon, y = ir),
      colour = "#0072B2",
      linewidth = 1) +
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
    scale_fill_manual(
      "C.I. level", 
      values = c("68\\%" = "gray10", "95\\%" = "gray60")
    ) +
    theme_paper() +
    theme(strip.placement = "outside")
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin_quad, 
    path = "../../../figs/", 
    filename = "fig_lp_quad",
    width = 7,
    height = 4.5
  )
}

# Positive vs. Negative shocks----

#' @param scale_shock number of SD to use
#' @param sign 1 for positive shock (default), -1 for negative shock
get_quad_response <- function(scale_shock, sign = 1) {
  df_irfs_lp_quad <- 
    map(resul_lp_quad, "coefs") |> 
    list_rbind() |> 
    filter(name %in% weather_variables) |> 
    mutate(
      order = ifelse(str_detect(name, "_sq$"), 2, 1),
      name = str_remove(name, "_sq$")
    ) |> 
    pivot_wider(
      names_from = order, 
      values_from = c(value, std, std_shock, median_shock, q05_shock, q95_shock), 
      names_prefix = 'order_') |> 
    rename(d = std_shock_order_1) |> 
    mutate(d = !!sign * !!scale_shock * d) |> 
    mutate(
      ir = value_order_1 * d + value_order_2 * d^2,
      # IC 95%
      ir_lower_95 = (value_order_1 - qnorm(0.975) * std_order_1) * d + 
        (value_order_2 - qnorm(0.975) * std_order_2) * d^2,
      ir_upper_95 = (value_order_1 + qnorm(0.975) * std_order_1) * d +
        (value_order_2 + qnorm(0.975) * std_order_2) * d^2,
      # IC 68%
      ir_lower_68 = (value_order_1 - qnorm(0.84) * std_order_1) * d + 
        (value_order_2 - qnorm(0.84) * std_order_2) * d^2,
      ir_upper_68 = (value_order_1 + qnorm(0.84) * std_order_1) * d +
        (value_order_2 + qnorm(0.84) * std_order_2) * d^2
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
  
  df_irfs_lp_quad_ci <- 
    df_irfs_lp_quad |> 
    select(horizon, crop, name, matches("^(ir_lower)|^(ir_upper)", perl = TRUE)) |> 
    pivot_longer(
      cols = matches("^(ir_lower)|^(ir_upper)", perl = TRUE),
      names_pattern = "(.*)_(95|68)$",
      names_to = c(".value", "level")
    ) |> 
    mutate(level = str_c(level, "%"))
  
  list(df_irfs_lp_quad = df_irfs_lp_quad, df_irfs_lp_quad_ci = df_irfs_lp_quad_ci)
}


tbl_irfs_pos <- get_quad_response(scale_shock = 1, sign = 1)
tbl_irfs_neg <- get_quad_response(scale_shock = 1, sign = -1)

tbl_irfs <- 
  tbl_irfs_pos$df_irfs_lp_quad |> mutate(sign = "positive") |> 
  bind_rows(
    tbl_irfs_neg$df_irfs_lp_quad |> mutate(sign = "negative")
  )
tbl_irfs_ci <- 
  tbl_irfs_pos$df_irfs_lp_quad_ci |> mutate(sign = "positive") |> 
  bind_rows(
    tbl_irfs_neg$df_irfs_lp_quad_ci |> mutate(sign = "negative")
  )

p_lp_lin_quad_pos_neg <- 
  ggplot() +
  geom_ribbon(
    data = tbl_irfs_ci |> filter(sign == "positive"),
    mapping = aes(
      x = horizon,
      ymin = ir_lower, ymax = ir_upper, fill = level),
    alpha = .2
  ) +
  geom_ribbon(
    data = tbl_irfs_ci |> filter(sign == "negative"),
    mapping = aes(
      x = horizon,
      ymin = ir_lower, ymax = ir_upper, fill = level),
    alpha = .2
  ) +
  geom_line(
    data = tbl_irfs,
    mapping = aes(x = horizon, y = ir, colour = sign, linetype = sign),
    linewidth = 1.2
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
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    "C.I. level", 
    values = c("68%" = "gray10", "95%" = "gray60")
  ) +
  scale_colour_manual(
    "Shock", 
    values = c("negative" = "#56B4E9", "positive" = "#009E73"),
    labels = c("negative" = "Lower than usual", "positive" = "Higher than usual")) +
  scale_linetype_discrete(
    "Shock",
    labels = c("negative" = "Lower than usual", "positive" = "Higher than usual")
  ) +
  theme_paper() +
  theme(
    strip.placement = "outside", 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal"
  )

p_lp_lin_quad_pos_neg

if (1 == 0) {
  
  p_lp_lin_quad_pos_neg <- 
    ggplot() +
    geom_ribbon(
      data = tbl_irfs_ci |> 
        filter(sign == "positive") |> 
        mutate(
          level = str_replace(level, pattern = "%", replacement = "\\\\%")
        ),
      mapping = aes(
        x = horizon,
        ymin = ir_lower, ymax = ir_upper, fill = level),
      alpha = .2
    ) +
    geom_ribbon(
      data = tbl_irfs_ci |> filter(sign == "negative"),
      mapping = aes(
        x = horizon,
        ymin = ir_lower, ymax = ir_upper, fill = level),
      alpha = .2
    ) +
    geom_line(
      data = tbl_irfs,
      mapping = aes(x = horizon, y = ir, colour = sign, linetype = sign),
      linewidth = 1.2
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
    scale_fill_manual(
      "C.I. level", 
      values = c("68\\%" = "gray10", "95\\%" = "gray60")
    ) +
    scale_colour_manual(
      "Shock", 
      values = c("negative" = "#56B4E9", "positive" = "#009E73"),
      labels = c("negative" = "Lower than usual", "positive" = "Higher than usual")) +
    scale_linetype_discrete(
      "Shock",
      labels = c("negative" = "Lower than usual", "positive" = "Higher than usual")
    ) +
    theme_paper() +
    theme(
      strip.placement = "outside", 
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin_quad_pos_neg, 
    path = "../../../figs/", 
    filename = "fig_lp_quad_pos_neg",
    width = 7,
    height = 4.5
  )
}
