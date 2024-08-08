library(tidyverse)
library(ggpattern)

# ggplot2 theme:
source("../weatherperu/R/utils.R")
# data used in the local projection estimations
load("../data/output/df_lp.rda")
# weather data
load("../data/output/weather/weather_regions_df.rda")

## Crops----

crops <- c("Rice", "Dent corn", "Potato", "Cassava")

### Table----

# Monthly agricultural production (in Tons), per region
df |>
  mutate(product_eng = factor(product_eng, levels = crops)) |> 
  group_by(product_eng) |> 
  summarise(
    prod_mean = mean(y_new),
    prod_median = median(y_new),
    prod_sd = sd(y_new),
    prod_min = min(y_new),
    prod_max = max(y_new),
    nb_regions = length(unique(region)),
    nb_obs = n()
  ) |> 
  knitr::kable("markdown", digits = 0, format.args = list(big.mark = ","))

### National production----

# National monthly crop production for selected cultures (in tons)
p_nat_prod <- 
  ggplot(
  data = df |> 
    group_by(product_eng, date) |> 
    summarise(
      nat_prod = sum(y_new),
      .groups = "drop"
    ) |> 
    mutate(
      product_eng = factor(
        product_eng, 
        levels = c("Cassava", "Dent corn", "Potato", "Rice"),
        labels = c("Cassava", "Maize", "Potato", "Rice")
      )
    )
) +
  geom_line(
    mapping = aes(x = date, y = nat_prod),
    colour = "#1f78b4",
    linewidth = 1
  ) +
  facet_wrap(~product_eng, scales = "free_y", ncol = 2) +
  labs(x = NULL, y=  "Aggregate Production (tons)") +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  theme_paper()

p_nat_prod

if (1 == 0) {
  # To save the graph in PDF, using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_nat_prod,
    path = "../../figs/", 
    filename = "fig_nat_prod",
    width = 6,
    height = 4)
}


### National production by month and geo----

# Crop production by months and natural regions (in tons)
p_regional_monthly_prod <- 
  ggplot(
  data = df |> 
    group_by(product_eng, year, month) |> 
    # Average each month at the national level
    summarise(
      prod_nat_costa = sum(y_new * share_costa),
      prod_nat_selva = sum(y_new * share_selva),
      prod_nat_sierra = sum(y_new * share_sierra),
      .groups = "drop"
    ) |> 
    pivot_longer(
      cols = c(prod_nat_costa, prod_nat_selva, prod_nat_sierra),
      names_to = "geo",
      values_to = "monthly_prod_geo"
    ) |> 
    # Average in each region type for each calendar month
    group_by(product_eng, month, geo) |> 
    summarise(
      monthly_prod_geo = mean(monthly_prod_geo),
      .groups = "drop"
    ) |> 
    mutate(
      product_eng = factor(
        product_eng, 
        levels = c("Cassava", "Dent corn", "Potato", "Rice"),
        labels = c("Cassava", "Maize", "Potato", "Rice")
      ),
      geo = factor(
        geo,
        levels = c("prod_nat_costa", "prod_nat_selva", "prod_nat_sierra"),
        labels = c("Coast", "Forest", "Highlands")
      )
    ),
  mapping = aes(
    x = month, y = monthly_prod_geo, 
    colour = geo, linetype = geo
  )
) +
  geom_line(
    linewidth = 1
  ) +
  facet_wrap(~product_eng, scales = "free") +
  labs(x = NULL, y = "Average Production (tons)") +
  scale_colour_manual(
    NULL,
    values = c(
      "Coast" = "#56B4E9", "Forest" = "#009E73", "Highlands" = "#E69F00"
    )
  ) +
  scale_linetype_discrete(NULL) +
  scale_x_continuous(breaks= 1:12, labels = month.abb) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  theme_paper()

p_regional_monthly_prod

if (1 == 0) {
  # To save the graph using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_regional_monthly_prod, 
    path = "../../figs/", 
    filename = "fig_regional_monthly_prod",
    width = 8,
    height = 4
  )
}

### Maps----


map_peru <- sf::st_read("../data/raw/shapefile_peru/departamentos/")
map_peru <-
  rmapshaper::ms_simplify(input = as(map_peru, 'Spatial')) |>
  sf::st_as_sf()

#### Grid----

map_peru_grid <- sf::st_read(
  str_c(
    "../data/raw/shapefile_peru/grid/",
    "cuadro de empalme oficial 100k ign peru geogpsperu/")
)


if (1 == 0) {
  # To save the graph using pdflatex
  library(tikzDevice)
  
  p_map_peru_grid <- 
    ggplot(data = map_peru_grid) + 
    geom_sf(fill = "#009E73", alpha = .3, colour = "white") +
    geom_sf(data = map_peru, fill = NA, colour = "#E69F00", linewidth = .8) +
    theme_map_paper_dark()
}


#### Share agri. land----
# Share of agricultural area in the cell, for each cell of the grid
load("../data/output/land/map_peru_grid_agri.rda")


p_share_agri_land <- 
  ggplot() +
  geom_sf(
    data = map_peru_grid_agri |> 
      mutate(share_cropland = percent_cropland / 100), 
    mapping = aes(fill = share_cropland),
    colour = "grey"
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    "Share of\nagricultural\nland", 
    low = "white", high = "#61C250",
    labels = scales::label_percent()
  ) +
  theme_map_paper()

p_share_agri_land

if (1 == 0) {
  # To save the graph using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_share_agri_land +
      scale_fill_gradient2(
        "Share of\nagricultural\nland", 
        low = "white", high = "#61C250",
        labels = scales::percent_format(suffix = "\\%")
      ),
    path = "./", 
    filename = "fig_share_agri_land",
    width = 6,
    height = 6)
  system("mv fig_share_agri_land.pdf ../../figs/")
}

# The relative share of each cell within each region

get_shares_i <- function(i, weather_variables) {
  map_region_i <- map_peru[i,]
  tmp <- 
    sf::st_intersection(map_peru_grid_agri, map_region_i) |> 
    # Get the area of the intersection between the polygon of the current
    # region and each grid cell that intersects it
    dplyr::mutate(area_cell_intersect = sf::st_area(geometry)) |> 
    dplyr::rename(grid_id = i) |> 
    # Get the weather data for the corresponding grid cells
    # Remove the unit in the area (squared metres)
    dplyr::mutate(
      area_cell_intersect = units::drop_units(area_cell_intersect)
    ) |> 
    # Compute the weights to attribute to each grid cell
    dplyr::mutate(
      w_cropland = cropland / sum(cropland),
      w_area     = area_cell_intersect / sum(area_cell_intersect),
      w          = w_cropland * w_area,
      w          = w / sum(w)
    )
}

cropland_regions_df <- 
  map(
    .x = seq_len(nrow(map_peru)),
    .f = ~get_shares_i(.x), 
    .progress = TRUE
  )

cropland_regions_df <- 
  cropland_regions_df |> bind_rows()

p_regional_share_agri_land <- 
  ggplot() +
  geom_sf(
    data = cropland_regions_df,
    mapping = aes(fill = w),
    colour = "grey"
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    "Regional\nshare of\nagricultural\nland", 
    low = "white", high = "#61C250",
    labels = scales::label_percent()
  ) +
  theme_map_paper()

p_regional_share_agri_land

#### Regions used----

# For each region of the map: is it included or not in the analysis?
df_plot_map_regions <- 
  map_peru |> 
  left_join(
    df |> 
      select(product_eng, region) |> unique() |> 
      mutate(used = TRUE) |> 
      pivot_wider(names_from = product_eng, values_from = used),
    by = c("DEPARTAMEN" = "region")
  )

# Included regions
ggplot(
  data = df_plot_map_regions |> 
    pivot_longer(cols = !!crops, values_to = "included") |> 
    mutate(
      included = replace_na(included, FALSE),
      included = factor(
        included, levels = c(TRUE, FALSE),
        labels = c("Yes", "No")
      )
    )
) +
  geom_sf(mapping = aes(fill = included)) +
  facet_wrap(~name) +
  theme_paper() +
  scale_fill_manual(
    NULL,
    values = c("Yes" = "#009E73", "No" = "#949698"),
    labels = c("Yes" = "Region included", "No" = "Region discarded")) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )


load("../data/output/dataset_2001_2015.rda")

# Let us compare the previous map with the annual production.
# Some regions are discarded because the production is 0 for consecutive
# months.
# This is especially the case for rice production, in regions like
# Lambayeque, La Libertad, or Arequipa.

prod_reg <- 
  data_total |> 
  group_by(region, product_eng) |> 
  summarise(
    total_production = sum(Value_prod, na.rm = T),
    .groups = "drop"
  ) |> 
  unique() |> 
  group_by(product_eng) |> 
  mutate(total_culture = sum(total_production)) |> 
  ungroup() |> 
  mutate(share = total_production / total_culture)


map_production_regions <- NULL
for (i in 1:length(crops)) {
  culture <- as.character(crops[i])
  map_peru_tmp <- map_peru |> 
    left_join(
      prod_reg |> 
        filter(product_eng == !!culture), 
      by = c("DEPARTAMEN" = "region")
    ) |> 
    mutate(product_eng = !!culture)
  map_production_regions <- bind_rows(map_production_regions, map_peru_tmp)
}

ggplot(
  data = map_production_regions
) +
  geom_sf(
    mapping = aes(fill = share, group = DEPARTAMEN)
  ) +
  scale_fill_gradient2(
    "Share", low = "#FFC107", mid = "white", high = "#009E73",
    labels = scales::percent_format()
  ) +
  facet_wrap(~product_eng) +
  theme_map_paper()

included_regions <- 
  df_plot_map_regions |> 
  pivot_longer(cols = !!crops, values_to = "included") |> 
  as_tibble() |> 
  select(IDDPTO, name, included) |> 
  mutate(included = replace_na(included, FALSE)) |> 
  rename(product_eng = name)

p_regions_use_production <- 
  ggplot(
    data = map_production_regions |> 
      left_join(included_regions, by = c("IDDPTO", "product_eng")) |> 
      mutate(
        included = factor(
          included, 
          levels = c(TRUE, FALSE), 
          labels = c("Yes", "No")
        )
      ) |> 
      mutate(
        product_eng = factor(
          product_eng,
          levels = c("Cassava", "Dent corn", "Potato", "Rice"),
          labels = c("Cassava", "Maize", "Potato", "Rice")
        )
      )
  ) +
  geom_sf_pattern(
    mapping = aes(
      pattern = included,
      pattern_type = included, 
      fill = share
    ),
    pattern_fill = "white",
    pattern_alpha = .8
  ) +
  # scale_pattern_type_discrete("Included", values = c("Yes" = "none", "No" = "stripe")) +
  scale_pattern_manual("Included", values = c("No" = "stripe", "Yes" = "none")) +
  scale_pattern_type_manual("Included", values=c(NA, NA)) +
  scale_fill_gradient2(
    "Share", low = "#FFC107", mid = "white", high = "#009E73",
    labels = scales::percent_format()
  ) +
  facet_wrap(~product_eng) +
  theme_map_paper()

p_regions_use_production

if (1 == 0) {
  # To save the graph in PDF using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_regions_use_production + 
      scale_fill_gradient2(
        "Share", low = "#FFC107", mid = "white", high = "#009E73",
        labels = scales::percent_format(suffix = "\\%")
      ),
    path = "./", 
    filename = "fig_regions_use_production",
    width = 7,
    height = 7)
  
  system("mv fig_regions_use_production.pdf ../../figs/")
}

## Natural regions----

# Map of the natural regions 
map_regiones_naturales <-  sf::st_read(
  str_c(
    "../data/raw/shapefile_peru/regiones_naturales/",
    "region natural_geogpsperu_JuanPabloSuyoPomalia.geojson"
  )
)

map_regiones_naturales <-
  rmapshaper::ms_simplify(input = as(map_regiones_naturales, 'Spatial')) |> 
  sf::st_as_sf() |> 
  mutate(
    Natural_region = case_when(
      Nm_RegNat == "Costa" ~ "Coast",
      Nm_RegNat == "Selva" ~ "Forest",
      Nm_RegNat == "Sierra" ~ "Highlands",
    )
  )

# Visual representation 
cols <- c("Coast" = "#56B4E9", "Forest" = "#009E73", "Highlands" = "#E69F00")

# Natural regions in Peru
p_nat_reg_admin_gridded <- 
  ggplot(data = map_regiones_naturales) +
  geom_sf(mapping = aes(fill = Natural_region), lwd = 0) +
  scale_fill_manual(values = cols, name = "Natural region") +
  geom_sf(data = map_peru, fill = NA) +
  geom_sf(data = map_peru_grid_agri, fill = NA, lwd = 0.25) +
  theme_map_paper()


if (1 == 0) {
  # To save the graph in PDF using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_nat_reg_admin_gridded,
    path = "../../figs/", 
    filename = "fig_nat_reg_admin_gridded",
    width = 7,
    height = 7)
}

## Correlations----
name_weather_variables <- 
  weather_regions_df |> 
  select(where(is.numeric)) |> 
  select(-year, -month) |> 
  colnames()

# Correlations between the weather variables and the production variable
df |>
  select(product_eng, y, !!!syms(name_weather_variables)) |>
  # mutate(y_lead = dplyr::lead(y, n=1)) |> 
  na.omit() |>
  nest(.by = product_eng) |> 
  mutate(
    correlation = map(
      data, ~cor(.x) |> 
        data.frame() |> 
        as_tibble(rownames = "var")
    )
  ) |> 
  select(-data) |> 
  unnest(correlation) |> 
  select(product_eng, var, y) |> 
  pivot_wider(names_from = product_eng, values_from = y) |> 
  mutate(across(where(is.numeric), ~round(.x, 2))) |> 
  print(n = 35)

## Local Projections data----

# Number of observation in each region and crops left
df |> 
  count(product_eng, region_id) |> 
  arrange(n)

# Number of regions for each crop
df |> 
  group_by(product_eng) |> 
  summarise(nb_regions = length(unique(region_id)))

df |> 
  group_by(product_eng, region_id) |> 
  summarise()

df$y_new |> head()


for (i_crop in 1:length(crops)) {
  current_crop <- crops[i_crop]
  # The series in each region for the current crop
  p_crop_lp <- 
    ggplot(
    data = df |> filter(product_eng == !!current_crop),
    mapping = aes(x = date, y = y)
  ) +
    geom_line() +
    facet_wrap(~region, scales = "free_y") +
    labs(
      title = current_crop, x = NULL,
      y = "Percent deviation from monthly trend") +
    theme_paper()
  print(p_crop_lp)
}


library(gtsummary)

df |>
  tbl_summary(
    include = c(
      "product_eng",
      # Production
      "y_new", "y",
      # Weather
      "temp_max_dev", "perc_gamma_precip",
      # Control
      "rer", "r", "pi", "ind_prod",
      # Type of region
      "share_costa", "share_selva", "share_sierra"
    ),
    by = product_eng,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"),
      all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ 0
    )
  ) |> 
  add_overall(col_label = "Whole sample") |> 
  modify_header(label ~ "**Variable**") |> 
  modify_spanning_header(
    c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Crop**"
  ) |> 
  add_stat_label(
    label = list(
      all_continuous() ~ c("Mean (SD)", "Median (IQR)"),
      all_categorical() ~ "n (%)"
    )
  )


# Descriptive statistics for the production (in tons, detrended, SA)
df |> 
  group_by(product_eng) |> 
  summarise(
    Mean = mean(y_new, na.rm = TRUE),
    Median = median(y_new, na.rm = TRUE),
    `Standard Deviation` = sd(y_new, na.rm = TRUE),
    Min = min(y_new, na.rm = TRUE),
    Max = max(y_new, na.rm = TRUE),
    `No. regions` = length(unique(region)),
    `No. obs.` = n()
  ) |> 
  knitr::kable(
    booktabs = TRUE, 
    format = "latex",
    format.args = list(big.mark = ","), 
    caption = "Descriptive statistics for monthly production (in tons) per type of crop"
  )

## Growing Season Duration

library(tidyverse)
library(readxl)
library(dplyr)
library(xtable)


load("../data/output/minagri/data_S_TOTAL.rda")
load("../data/output/minagri/data_SR_TOTAL.rda")
load("../data/output/df_lp.rda")

# Selection of the regions (by culture) from the final dataset 

list_of_regions <- df |> select(product, region) |> unique() |> mutate(keep_data =1)

# Extraction of information from the calandars 

N <- list.files(path = "../data/raw/Calendario agricola/", pattern = "xls$", full.names = TRUE)

#x <- N[12]

import_calendar <- function(x){
  region <- str_extract(x, "//cal_(.*)\\.xls") %>% 
    str_remove("//cal_") %>% 
    str_remove("\\.xls")
  
  df_cal_1 <- read_excel(x)
  
  row_prod_mes <- str_which(df_cal_1$`CALENDARIO AGRICOLA NACIONAL`, "Producto/Mes")
  ind_first <- first(row_prod_mes)
  ind_last <- last(row_prod_mes)
  
  df_cal_planting <- read_excel(x, skip = ind_first, n_max = ind_last-ind_first-3)
  df_cal_planting <- 
    df_cal_planting %>% 
    pivot_longer(cols = -`Producto/Mes`, names_to = "month_spanish", values_to = "pct") %>% 
    mutate(region = region, period = "planting")
  
  df_cal_harvest <- read_excel(x, skip = ind_last)
  df_cal_harvest <- 
    df_cal_harvest %>% 
    filter(!is.na(`Producto/Mes`)) %>% 
    mutate(across(-`Producto/Mes`, ~as.numeric(.))) %>% 
    pivot_longer(cols = -`Producto/Mes`, names_to = "month_spanish", values_to = "pct") %>% 
    mutate(region = region, period = "harvest")
  
  df_cal_planting %>% 
    bind_rows(df_cal_harvest)
  
}

calendar_brut <- map_df(N, import_calendar)
calendar <- calendar_brut
calendar <- 
  calendar %>% 
  filter(month_spanish != "...14")


calendar <-
  calendar %>%
  mutate(month = case_when(
    month_spanish == "Dic" ~ 12,
    month_spanish == "Nov" ~ 11,
    month_spanish == "Oct" ~ 10,
    month_spanish == "Set" ~ 9,
    month_spanish == "Ago" ~ 8,
    month_spanish == "Jul" ~ 7,
    month_spanish == "Jun" ~ 6,
    month_spanish == "May" ~ 5,
    month_spanish == "Abr" ~ 4,
    month_spanish == "Mar" ~ 3,
    month_spanish == "Feb" ~ 2,
    month_spanish == "Ene" ~ 1
  )) |> 
  select(-month_spanish)

calendar <- calendar %>% 
  mutate(region = ifelse(region == "lalibertad", "la libertad", region),
         region = ifelse(region == "madrededios", "madre de dios", region),
         region = ifelse(region == "sanmartin", "san martin", region)
  ) %>% 
  mutate(region = str_to_upper(region))

calendar <- 
  calendar %>% 
  rename(product = `Producto/Mes`) %>% 
  mutate(pct = ifelse(is.na(pct), 0, pct))

calendar1 <- calendar %>% 
  filter(period == "planting") %>% 
  group_by(region, product) %>% 
  mutate(val_max = max(pct)) %>% 
  ungroup() %>% 
  slice(., which(pct == val_max)) %>% 
  select(-val_max) 

calendar2 <- calendar %>% 
  filter(period == "harvest") %>% 
  group_by(region, product) %>% 
  mutate(val_max = max(pct)) %>% 
  ungroup() %>% 
  slice(., which(pct == val_max)) %>% 
  slice(.,-2) %>% 
  slice(.,-163) %>% 
  select(-val_max) %>% 
  full_join(calendar1, by = c("region","product")) %>% 
  mutate(growth_duration = month.x - month.y) %>% 
  mutate(growth_duration = ifelse(growth_duration < 0, month.x + 13 - month.y, growth_duration)) %>% 
  mutate(growth_duration = ifelse(growth_duration == 0,12, growth_duration)) %>% 
  slice(., - which(is.na(growth_duration))) %>% 
  mutate(region = toupper(iconv(region, to = "ASCII//TRANSLIT")),
         product = toupper(product))


calendar3 <- calendar %>% 
  filter(period == "harvest") %>% 
  mutate(month =   str_c("month", month)) %>% 
  pivot_wider(names_from = month, values_from = pct) %>% 
  mutate(cum_sum1  = month1, 
         cum_sum2  = month2 + cum_sum1, 
         cum_sum3  = month3 + cum_sum2, 
         cum_sum4  = month4 + cum_sum3, 
         cum_sum5  = month5 + cum_sum4, 
         cum_sum6  = month6 + cum_sum5, 
         cum_sum7  = month7 + cum_sum6, 
         cum_sum8  = month8 + cum_sum7, 
         cum_sum9  = month9 + cum_sum8, 
         cum_sum10 = month10 + cum_sum9, 
         cum_sum11 = month11 + cum_sum10, 
         cum_sum12 = month12 + cum_sum11) %>% 
  select(product, region, cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6, cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12) %>% 
  pivot_longer(cols = c(cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6, cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12), names_to = "month") %>% 
  rename(perc_cum_harv = value) %>% 
  mutate(month = case_when(
    month == "cum_sum1"  ~ 1, 
    month == "cum_sum2"  ~ 2, 
    month == "cum_sum3"  ~ 3, 
    month == "cum_sum4"  ~ 4, 
    month == "cum_sum5"  ~ 5, 
    month == "cum_sum6"  ~ 6, 
    month == "cum_sum7"  ~ 7, 
    month == "cum_sum8"  ~ 8, 
    month == "cum_sum9"  ~ 9, 
    month == "cum_sum10" ~ 10, 
    month == "cum_sum11" ~ 11, 
    month == "cum_sum12" ~ 12, 
  ))

calendar4 <- calendar %>% 
  filter(period == "planting") %>% 
  mutate(month =   str_c("month", month)) %>% 
  pivot_wider(names_from = month, values_from = pct) %>% 
  mutate(cum_sum1  = month8, 
         cum_sum2  = month9 + cum_sum1, 
         cum_sum3  = month10 + cum_sum2, 
         cum_sum4  = month11 + cum_sum3, 
         cum_sum5  = month12 + cum_sum4, 
         cum_sum6  = month1 + cum_sum5, 
         cum_sum7  = month2 + cum_sum6, 
         cum_sum8  = month3 + cum_sum7, 
         cum_sum9  = month4 + cum_sum8, 
         cum_sum10 = month5 + cum_sum9, 
         cum_sum11 = month6 + cum_sum10, 
         cum_sum12 = month7 + cum_sum11) %>% 
  select(product, region, cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6, cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12) %>% 
  pivot_longer(cols = c(cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6, cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12), names_to = "month") %>% 
  rename(perc_cum_plan = value) %>% 
  mutate(month = case_when(
    month == "cum_sum1"  ~ 8, 
    month == "cum_sum2"  ~ 9, 
    month == "cum_sum3"  ~ 10, 
    month == "cum_sum4"  ~ 11, 
    month == "cum_sum5"  ~ 12, 
    month == "cum_sum6"  ~ 1, 
    month == "cum_sum7"  ~ 2, 
    month == "cum_sum8"  ~ 3, 
    month == "cum_sum9"  ~ 4, 
    month == "cum_sum10" ~ 5, 
    month == "cum_sum11" ~ 6, 
    month == "cum_sum12" ~ 7, 
  ))



# save(calendar, file = "Calendario agricola/calendar.rda")
# save(calendar2, file = "Calendario agricola/calendar2.rda")
# save(calendar3, file = "Calendario agricola/calendar3.rda")
# save(calendar4, file = "Calendario agricola/calendar4.rda")
# Computing the planting and harvesting cumulative sums from the data 
#--------------------------------------------------------------------

data_S_TOTAL <-data_S_TOTAL %>%
  mutate( region = str_replace_all(region, "á", "a"),
          region = str_replace_all(region, "í", "i"),
          region = str_replace_all(region, "é", "e"),
          region = str_replace_all(region, "ó", "o"),
          region = str_replace_all(region, "ú", "u"),
          region = str_replace_all(region, "ñ", "n"))


data_SR_TOTAL <-data_SR_TOTAL %>%
  mutate( region = str_replace_all(region, "á", "a"),
          region = str_replace_all(region, "í", "i"),
          region = str_replace_all(region, "é", "e"),
          region = str_replace_all(region, "ó", "o"),
          region = str_replace_all(region, "ú", "u"),
          region = str_replace_all(region, "ñ", "n"))


harv_cum <- data_SR_TOTAL %>%
  #filter(product  %in% c("PAPA", "CEBADA GRANO", "MAÍZ AMARILLO DURO","MAÍZ AMILÁCEO", "ARROZ CÁSCARA", "SORGO GRANO", "YUCA", "TRIGO")) %>%
  #filter(! region == "TOTAL NACIONAL") %>% 
  #filter(product  == "PAPA") %>%
  #filter(region  == "AMAZONAS") %>% 
  select(-date) %>% 
  mutate(month =   str_c("month", month)) %>% 
  pivot_wider(names_from = month, values_from = Value_surfR) %>% 
  mutate(cum_sum1  = month1, 
         cum_sum2  = month2 + cum_sum1, 
         cum_sum3  = month3 + cum_sum2, 
         cum_sum4  = month4 + cum_sum3, 
         cum_sum5  = month5 + cum_sum4, 
         cum_sum6  = month6 + cum_sum5, 
         cum_sum7  = month7 + cum_sum6, 
         cum_sum8  = month8 + cum_sum7, 
         cum_sum9  = month9 + cum_sum8, 
         cum_sum10 = month10 + cum_sum9, 
         cum_sum11 = month11 + cum_sum10, 
         cum_sum12 = month12 + cum_sum11) %>%
  mutate(perc_cum1   = cum_sum1/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum2   = cum_sum2/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum3   = cum_sum3/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum4   = cum_sum4/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum5   = cum_sum5/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum6   = cum_sum6/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum7   = cum_sum7/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum8   = cum_sum8/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum9   = cum_sum9/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum10  = cum_sum10/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum11  = cum_sum11/ifelse(cum_sum12 == 0, 1, cum_sum12),
         perc_cum12  = cum_sum12/ifelse(cum_sum12 == 0, 1, cum_sum12))%>% 
  select(product, region, year, perc_cum1, perc_cum2, perc_cum3, perc_cum4, perc_cum5, perc_cum6, perc_cum7, perc_cum8, perc_cum9, perc_cum10, perc_cum11, perc_cum12) %>% 
  pivot_longer(cols = c(perc_cum1, perc_cum2, perc_cum3, perc_cum4, perc_cum5, perc_cum6, perc_cum7, perc_cum8, perc_cum9, perc_cum10, perc_cum11, perc_cum12), names_to = "month") %>% 
  rename(perc_cum_harv_obs = value) %>% 
  mutate(perc_cum_harv_obs = perc_cum_harv_obs*100) %>%
  mutate(month = case_when(
    month == "perc_cum1"  ~ 1, 
    month == "perc_cum2"  ~ 2, 
    month == "perc_cum3"  ~ 3, 
    month == "perc_cum4"  ~ 4, 
    month == "perc_cum5"  ~ 5, 
    month == "perc_cum6"  ~ 6, 
    month == "perc_cum7"  ~ 7, 
    month == "perc_cum8"  ~ 8, 
    month == "perc_cum9"  ~ 9, 
    month == "perc_cum10" ~ 10, 
    month == "perc_cum11" ~ 11, 
    month == "perc_cum12" ~ 12, 
  )) %>% 
  #filter(perc_cum_harv_obs == 100) %>%
  #group_by(region, product, year) %>% 
  mutate(first = ifelse(perc_cum_harv_obs == 100, 1, 0)) %>% 
  mutate(first = ifelse(first == 1 & lag(first) == 1, -1, first)) %>% 
  mutate(perc_cum_harv_obs_first100 = ifelse(first == -1,0,perc_cum_harv_obs)) %>% 
  select(-first) 




# Selecting the total value (December) as an additional month 
temp <-   data_S_TOTAL %>% 
  select(region, product, campaign_plain, month_campaign, Value_surf , campaign) %>% 
  filter(month_campaign == 12) %>% 
  mutate(month_campaign = 13) %>%
  rename(surf_m = Value_surf)


plan_perc  <- data_S_TOTAL %>%
  select(region, product, campaign_plain, month_campaign, surf_m , campaign) %>%
  rbind(temp) %>%
  .[with(., order(region, product, campaign, month_campaign) ), ] %>%
  select(-campaign) %>%
  #filter( ! region == "JUNIN") %>%
  mutate(month_campaign =   str_c("cum_sum", month_campaign)) %>%
  unique() %>%   
  filter(! product == "TOTAL") %>% 
  pivot_wider(names_from = month_campaign, values_from = surf_m) %>%
  relocate(region, product, campaign_plain, cum_sum1, cum_sum2, cum_sum3,
           cum_sum4, cum_sum5, cum_sum6, cum_sum7, cum_sum8,
           cum_sum9, cum_sum10, cum_sum11, cum_sum12, cum_sum13) %>%
  rename(total = cum_sum13) %>%
  mutate(perc_cum1   = cum_sum1/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum2   = cum_sum2/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum3   = cum_sum3/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum4   = cum_sum4/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum5   = cum_sum5/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum6   = cum_sum6/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum7   = cum_sum7/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum8   = cum_sum8/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum9   = cum_sum9/total*100,       #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum10  = cum_sum10/total*100,      #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum11  = cum_sum11/total*100,      #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12),
         perc_cum12  = cum_sum12/total*100) %>%  #ifelse(is.na(cum_sum12), cum_sum5,cum_sum12)) %>%
  select(product, region, campaign_plain, perc_cum1, perc_cum2, perc_cum3, perc_cum4, perc_cum5, perc_cum6, perc_cum7, perc_cum8, perc_cum9, perc_cum10, perc_cum11, perc_cum12) %>%
  pivot_longer(cols = c(perc_cum1, perc_cum2, perc_cum3, perc_cum4, perc_cum5, perc_cum6, perc_cum7, perc_cum8, perc_cum9, perc_cum10, perc_cum11, perc_cum12), names_to = "month_campaign") %>%
  rename(perc_plan_obs = value) %>%
  mutate(perc_plan_obs = perc_plan_obs) %>%
  mutate(month_campaign = case_when(
    month_campaign == "perc_cum1"  ~ 1,
    month_campaign == "perc_cum2"  ~ 2,
    month_campaign == "perc_cum3"  ~ 3,
    month_campaign == "perc_cum4"  ~ 4,
    month_campaign == "perc_cum5"  ~ 5,
    month_campaign == "perc_cum6"  ~ 6,
    month_campaign == "perc_cum7"  ~ 7,
    month_campaign == "perc_cum8"  ~ 8,
    month_campaign == "perc_cum9"  ~ 9,
    month_campaign == "perc_cum10" ~ 10,
    month_campaign == "perc_cum11" ~ 11,
    month_campaign == "perc_cum12" ~ 12,
  ))


plan_perc_cum <- data_S_TOTAL %>% 
  select(region, product, campaign_plain, month_campaign, Value_surf , campaign) %>%
  .[with(., order(region, product, campaign, month_campaign) ), ] %>% 
  select(-campaign) %>% 
  #filter( ! region == "JUNIN") %>% 
  mutate(month_campaign = str_c("cum_sum", month_campaign)) %>% 
  unique() %>% 
  filter(! product == "TOTAL") %>% 
  pivot_wider(names_from = month_campaign, values_from = Value_surf) %>% 
  relocate(region, product, campaign_plain, cum_sum1, cum_sum2, cum_sum3, 
           cum_sum4, cum_sum5, cum_sum6, cum_sum7, cum_sum8, 
           cum_sum9, cum_sum10, cum_sum11, cum_sum12) %>% 
  mutate(perc_cum1   = cum_sum1/cum_sum12, 
         perc_cum2   = cum_sum2/cum_sum12, 
         perc_cum3   = cum_sum3/cum_sum12, 
         perc_cum4   = cum_sum4/cum_sum12,
         perc_cum5   = cum_sum5/cum_sum12, 
         perc_cum6   = cum_sum6/cum_sum12, 
         perc_cum7   = cum_sum7/cum_sum12, 
         perc_cum8   = cum_sum8/cum_sum12, 
         perc_cum9   = cum_sum9/cum_sum12, 
         perc_cum10  = cum_sum10/cum_sum12, 
         perc_cum11  = cum_sum11/cum_sum12, 
         perc_cum12  = cum_sum12/cum_sum12) %>%  
  select(product, region, campaign_plain, perc_cum1, perc_cum2, perc_cum3, perc_cum4, perc_cum5, perc_cum6, perc_cum7, perc_cum8, perc_cum9, perc_cum10, perc_cum11, perc_cum12) %>% 
  pivot_longer(cols = c(perc_cum1, perc_cum2, perc_cum3, perc_cum4, perc_cum5, perc_cum6, perc_cum7, perc_cum8, perc_cum9, perc_cum10, perc_cum11, perc_cum12), names_to = "month_campaign") %>% 
  rename(perc_cum_plan_obs = value) %>% 
  mutate(perc_cum_plan_obs = perc_cum_plan_obs*100) %>%
  mutate(month_campaign = case_when(
    month_campaign == "perc_cum1"  ~ 1, 
    month_campaign == "perc_cum2"  ~ 2, 
    month_campaign == "perc_cum3"  ~ 3, 
    month_campaign == "perc_cum4"  ~ 4, 
    month_campaign == "perc_cum5"  ~ 5, 
    month_campaign == "perc_cum6"  ~ 6, 
    month_campaign == "perc_cum7"  ~ 7, 
    month_campaign == "perc_cum8"  ~ 8, 
    month_campaign == "perc_cum9"  ~ 9, 
    month_campaign == "perc_cum10" ~ 10, 
    month_campaign == "perc_cum11" ~ 11, 
    month_campaign == "perc_cum12" ~ 12, 
  ))




#load("Calendario agricola/calendar3.rda") 
calendar3 <- calendar3 %>% 
  mutate( region = str_replace_all(region, "á", "a"),
          region = str_replace_all(region, "í", "i"),
          region = str_replace_all(region, "é", "e"),
          region = str_replace_all(region, "ó", "o"),
          region = str_replace_all(region, "ú", "u"),
          region = str_replace_all(region, "ñ", "n")) %>% 
  mutate(region = toupper(region), 
         product = toupper(product)) %>% 
  rename("perc_cum_harv_calend" = "perc_cum_harv")

#load("Calendario agricola/calendar4.rda") 
calendar4 <- calendar4 %>% 
  mutate( region = str_replace_all(region, "á", "a"),
          region = str_replace_all(region, "í", "i"),
          region = str_replace_all(region, "é", "e"),
          region = str_replace_all(region, "ó", "o"),
          region = str_replace_all(region, "ú", "u"),
          region = str_replace_all(region, "ñ", "n")) %>% 
  mutate(region = toupper(region), 
         product = toupper(product)) %>% 
  rename("perc_cum_plan_calend" = "perc_cum_plan")

rm(temp)

# Determining the optimal growth duration from the data 
#------------------------------------------------------

optimal_growth_duration <- data_SR_TOTAL |> 
  mutate(region = toupper(region), 
         product = toupper(product)) |>
  filter(product  %in% c("PAPA","MAÍZ AMARILLO DURO", "ARROZ CÁSCARA",  "YUCA")) |> 
  full_join(list_of_regions) |> 
  filter(keep_data == 1) |> 
  select(- keep_data)

for(ii in 1:12){
  optimal_growth_duration <- optimal_growth_duration %>% 
    left_join(data_S_TOTAL %>% 
                group_by(region, product) %>% 
                select(region,product, date, surf_m) %>% 
                mutate(!!as.name(paste("surf_lag", ii, sep="")) := lag(surf_m, ii)) %>%
                select(-surf_m) , 
              by = c("region", "product", "date")) %>% 
    mutate(!!as.name(paste("diff", ii, sep="")) := !!as.name(paste("surf_lag", ii, sep="")) - Value_surfR)
  
}

rm(ii)

optimal_growth_duration_results <- optimal_growth_duration %>% 
  unique() %>% 
  group_by(region, product) %>%
  mutate(sum_totale = sum(Value_surfR, na.rm = T )) %>% 
  ungroup() %>% 
  filter(! sum_totale == 0) %>%
  select(- sum_totale) %>% 
  #  filter(! year == 2005) %>% 
  filter(! year == 2001) %>% 
  group_by(region, product) %>% 
  transmute(nb_neg1 = sum(diff1 <0, na.rm = T),
            nb_neg2 = sum(diff2 <0, na.rm = T),
            nb_neg3 = sum(diff3 <0, na.rm = T),
            nb_neg4 = sum(diff4 <0, na.rm = T),
            nb_neg5 = sum(diff5 <0, na.rm = T),
            nb_neg6 = sum(diff6 <0, na.rm = T),
            nb_neg7 = sum(diff7 <0, na.rm = T),
            nb_neg8 = sum(diff8 <0, na.rm = T),
            diff1 = mean(abs(diff1), na.rm = T), 
            diff2 = mean(abs(diff2), na.rm = T), 
            diff3 = mean(abs(diff3), na.rm = T), 
            diff4 = mean(abs(diff4), na.rm = T), 
            diff5 = mean(abs(diff5), na.rm = T), 
            diff6 = mean(abs(diff6), na.rm = T), 
            diff7 = mean(abs(diff7), na.rm = T), 
            diff8 = mean(abs(diff8), na.rm = T), 
            diff9 = mean(abs(diff9), na.rm = T), 
            diff10 = mean(abs(diff10), na.rm = T), 
            diff11 = mean(abs(diff11), na.rm = T), 
            diff12 = mean(abs(diff12), na.rm = T)) %>% 
  unique() %>% 
  mutate(min_lag = min(diff1,diff2,diff3, diff4, diff5, diff6, diff7, diff8, diff9, diff10, diff11, diff12), 
         max_neg_values = max(nb_neg1, nb_neg2, nb_neg3, nb_neg4, nb_neg5, nb_neg6, nb_neg7, nb_neg8)) %>% 
  ungroup() %>% 
  filter(! is.nan(min_lag)) %>% 
  mutate(best_lag = case_when(
    diff1 == min_lag ~ as.numeric(13), 
    diff2 == min_lag ~ as.numeric(14), 
    diff3 == min_lag ~ as.numeric(3), 
    diff4 == min_lag ~ as.numeric(4), 
    diff5 == min_lag ~ as.numeric(5), 
    diff6 == min_lag ~ as.numeric(6), 
    diff7 == min_lag ~ as.numeric(7), 
    diff8 == min_lag ~ as.numeric(8), 
    diff9 == min_lag ~ as.numeric(9), 
    diff10 == min_lag ~ as.numeric(10), 
    diff11 == min_lag ~ as.numeric(11), 
    diff12 == min_lag ~ as.numeric(12), 
  ),
  worse_neg_values = case_when(
    nb_neg1 == max_neg_values ~ as.numeric(1),
    nb_neg2 == max_neg_values ~ as.numeric(2), 
    nb_neg3 == max_neg_values ~ as.numeric(3),
    nb_neg4 == max_neg_values ~ as.numeric(4),
    nb_neg5 == max_neg_values ~ as.numeric(5), 
    nb_neg6 == max_neg_values ~ as.numeric(6), 
    nb_neg7 == max_neg_values ~ as.numeric(7), 
    nb_neg8 == max_neg_values ~ as.numeric(8)), 
  bad_result = ifelse(best_lag == worse_neg_values, 1,0)) 


summary_opt_growth_duration <- optimal_growth_duration_results %>% 
  filter(! best_lag == 1) %>% 
  select(region, product, max_neg_values, best_lag) %>% 
  group_by(product) %>% 
  transmute(mean_best_lag = mean(best_lag),
            med_best_lag  = median(best_lag), 
            sd_best_lag   = sd(best_lag), 
            min_best_lag  = min(best_lag), 
            max_best_lag  = max(best_lag), 
            nb_regions     = n()) %>% 
  ungroup() |> 
  mutate(product_eng = case_when(
    product == "ARROZ CÁSCARA"      ~ "Rice",
    product == "MAÍZ AMARILLO DURO" ~ "Maize",
    product == "PAPA"               ~ "Potato", 
    product == "YUCA"               ~ "Cassava", 
    TRUE ~ "delete")) |> 
  #filter(!product_eng =="delete")
  unique() |> 
  select(- product) |> 
  relocate(product_eng) |> 
  arrange(product_eng)


xtable(summary_opt_growth_duration, type = "latex", file = "data/duration.tex")

