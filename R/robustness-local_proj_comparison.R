library(tidyverse)

source("../weatherperu/R/utils.R")

load("output/df_irfs_lp_piscop.rda") # monthly
load("output/df_irfs_lp_quarter.rda") # quarterly
load("output/df_irfs_lp_year.rda") # annual

df_irfs_lp_comparison <- df_irfs_lp |> 
  mutate(data_type = "monthly") |> 
  bind_rows(
    df_irfs_lp_quarter |> 
      mutate(data_type = "quarterly") |> 
      mutate(count = 3) |> 
      uncount(count) |> 
      group_by(crop, horizon, name) |> 
      mutate(
        horizon = ifelse(
          horizon != 0, 
          yes = (horizon-1)*3 + row_number(), no = 0
        )
      )
  ) |> 
  bind_rows(
    df_irfs_lp_year |> 
      mutate(data_type = "annual") |> 
      mutate(count = 12) |> 
      uncount(count) |> 
      group_by(crop, horizon, name) |> 
      mutate(
        horizon = ifelse(
          horizon != 0, 
          yes = (horizon-1)*12 + row_number(), no = 0
        )
      )
  ) |> 
  mutate(
    data_type = factor(
      data_type,
      levels = c("monthly", "quarterly", "annual"),
      labels = c("Monthly", "Quarterly", "Annual")
    )
  )

df_irfs_lp_ci_comparison <- df_irfs_lp_ci |> 
  mutate(data_type = "monthly") |> 
  bind_rows(
    df_irfs_lp_ci_quarter |> 
      mutate(data_type = "quarterly") |> 
      mutate(count = 3) |> 
      uncount(count) |> 
      group_by(crop, horizon, name, level) |> 
      mutate(
        horizon = ifelse(
          horizon != 0, 
          yes = (horizon-1)*3 + row_number(), no = 0
        )
      )
  ) |> 
  bind_rows(
    df_irfs_lp_ci_year |> 
      mutate(data_type = "annual") |> 
      mutate(count = 12) |> 
      uncount(count) |> 
      group_by(crop, horizon, name, level) |> 
      mutate(
        horizon = ifelse(
          horizon != 0, 
          yes = (horizon-1)*12 + row_number(), no = 0
        )
      )
  ) |> 
  mutate(
    data_type = factor(
      data_type,
      levels = c("monthly", "quarterly", "annual"),
      labels = c("Monthly", "Quarterly", "Annual")
    )
  )

nb_h <- 14

# Duration of the growing season
gs_duration_df <- tribble(
  ~crop, ~tc,
  "Rice", 4,
  "Maize", 5,
  "Potato", 6,
  "Cassava", 9
)

p_lp_lin_comparison <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci_comparison |> 
      filter(level == "68%", horizon <= !!nb_h),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = data_type, colour = data_type),
    alpha = .2, linetype = "dashed"
  ) +
  geom_line(
    data = df_irfs_lp_comparison |> filter(horizon <= !!nb_h),
    mapping = aes(x = horizon, y = shock_1_sd, colour = data_type)
  ) +
  scale_colour_manual(
    NULL, 
    values = c("Monthly" = "#56B4E9", "Quarterly" = "#E69F00", "Annual" = "#D55E00")
  ) +
  geom_hline(yintercept = 0, colour = "gray40") +
  ggh4x::facet_grid2(
    name~crop, scales = "free_y", axes = "all",
    independent = "y", switch = "y") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, nb_h, by = 2)) +
  labs(x = "Horizon (in months)", y = NULL) +
  scale_fill_manual(
    NULL,
    values = c("Monthly" = "#56B4E9", "Quarterly" = "#E69F00", "Annual" = "#D55E00")
  ) +
  theme_paper() +
  theme(strip.placement = "outside")


if (1 == 0) {
  # Save the plots in PDF using pdflatex
  p_lp_lin_comparison <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_lp_ci_comparison |> 
        filter(level == "68%", horizon <= !!nb_h),
      mapping = aes(
        x = horizon,
        ymin = lower, ymax = upper, fill = data_type, colour = data_type),
      alpha = .2, linetype = "dashed"
    ) +
    geom_line(
      data = df_irfs_lp_comparison |> filter(horizon <= !!nb_h),
      mapping = aes(x = horizon, y = shock_1_sd, colour = data_type)
    ) +
    geom_hline(yintercept = 0, colour = "gray40") +
    geom_vline(
      data = gs_duration_df, 
      mapping = aes(xintercept = tc),
      colour = "#D55E00", linetype = "dashed") +
    ggh4x::facet_grid2(
      name~crop, scales = "free_y", axes = "all",
      independent = "y", switch = "y") +
    scale_x_continuous(breaks = seq(0, nb_h, by = 2)) +
    scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
    labs(x = "Horizon", y = NULL) +
    scale_colour_manual(
      NULL, 
      values = c("Monthly" = "#56B4E9", "Quarterly" = "#E69F00", "Annual" = "#CC79A7")
    ) +
    scale_fill_manual(
      NULL,
      values = c("Monthly" = "#56B4E9", "Quarterly" = "#E69F00", "Annual" = "#CC79A7")
    ) +
    theme_paper() +
    theme(strip.placement = "outside")
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin_comparison, 
    path = "../../figs/", 
    filename = "fig_lp_lin_monthly_quarter_annual",
    width = 7,
    height = 4.5
  )
}
