library(tidyverse)
library(cli)
library(mFilter)
# Load Intermediate Data----

## Weather----
load("../data/output/weather/weather_quarter_regions_df.rda")
## Agriculture----
load("../data/output/minagri/dataset_agri_2001_2015.rda")
## Macro----
load("../data/output/macro/df_macro.rda")
## Commodity prices----
load("../data/output/macro/df_int_prices.rda")
## Natural Regions----
load("../data/output/natural_region_dep.rda")
## ENSO----
load("../data/output/weather/ONI_temp.rda")

# Merge----
# Compute ENSO-dependent weather anomalies 

Weather <- weather_quarter_regions_df |> 
  # Add ENSO data
  left_join(
    ONI_temp |> 
      mutate(quarter = quarter(date)) |> 
      mutate(
        year = as.numeric(Year), 
        quarter = as.numeric(quarter)) |> 
      group_by(year, quarter) |> 
      summarise(ONI = mean(ONI)), 
    by = c(
      "year" = "year",
      "quarter" = "quarter"
    )
  ) |>
  group_by(IDDPTO, quarter) |> 
  mutate( 
    temp_min_dev_ENSO   = temp_min - mean(temp_min),
    temp_max_dev_ENSO   = temp_max - mean(temp_max),
    temp_mean_dev_ENSO  = temp_mean - mean(temp_mean),
    precip_sum_dev_ENSO = precip_sum - mean(precip_sum))|> 
  ungroup() |> 
  labelled::set_variable_labels(
    temp_min_dev_ENSO   = "Deviation of Min. Temperature from ENSO Normals",
    temp_max_dev_ENSO   = "Deviation of Max. Temperature from ENSO Normals",
    temp_mean_dev_ENSO  = "Deviation of Mean Temperature from ENSO Normals",
    precip_sum_dev_ENSO = "Deviation of Total Rainfall from ENSO Normals",
  )


library(readxl)
int_prices <- read_excel(
  path = "../data/raw/Macro/IMF_DATA.xls",
  sheet = "SELECTED",
  col_types = "text") |> 
  mutate(date = lubridate::ymd(str_c(YEAR,  "-", MONTH, "-01"))) |> 
  rename(
    "FPI"           = "FPI_PFOOD", 
    "FERTILIZER"    = "FERTILIZER_PFERT",   
    "IndexOIL"      = "IndexOIL_POILAPSP",
    "PriceOIL"      = "PriceOIL_POILAPSP", 
    "CORN"          = "CORN_PMAIZMT", 
    "ARROZ CÁSCARA" = "RICE_PRICENPQ", 
    "TRIGO"         = "WHEAT_PWHEAMT"
  ) |> 
  mutate(CORN2 = CORN, 
         FPI2 = FPI) |> 
  select(-c(MONTH, YEAR, CPI_Peru_IMF, CPI_US_IMF, IndexOIL, PriceOIL)) |> 
  pivot_longer(cols = !date, names_to = "product", values_to = "price_int") |> 
  mutate(
    product = case_when(
      product == "FPI"  ~ "PAPA", 
      product == "FPI2" ~ "YUCA", 
      product == "CORN" ~ "MAÍZ AMILÁCEO", 
      product == "CORN2"~ "MAÍZ AMARILLO DURO", 
      TRUE ~ product
    ), 
    product_eng = case_when(
      product == "PAPA" ~ "Potato",
      product == "YUCA" ~ "Cassava",
      product == "ARROZ CÁSCARA" ~ "Rice",
      product == "MAÍZ AMARILLO DURO" ~ "Dent corn"
    ),
    price_int = as.numeric(price_int)
  ) |> 
  arrange(product, date) |> 
  mutate(
    quarter = quarter(date), 
    year    = year(date)
  ) |>  
  group_by(product, quarter, year) |> 
  mutate(price_int = mean(price_int)) |>  
  ungroup() |> 
  select(-date) |> 
  unique() |> 
  group_by(product, quarter) |> 
  arrange(year, quarter) |> 
  mutate(int_price_inf = (price_int/lag(price_int) - 1) * 100) |> 
  ungroup() |> 
  arrange(product, year, quarter) |> 
  filter(! year == 2000)
  
  

data_total <- 
  data_total |> 
  # Add macroeconomic data
  left_join(
    df_macro |> rename(gdp = y),
    by = "date"
  ) |> 
  mutate(quarter = quarter(date)) |> 
  dplyr::select(
    product_eng, region,region_id, product, quarter, year, Value_prod,
    rer_hp, r_hp, pi, ind_prod) |> 
  group_by(region, product, quarter, year) |> 
  mutate(
    Value_prod     = sum(Value_prod),   
    rer_hp         = mean(rer_hp),
    r_hp           = mean(r_hp),
    pi             = mean(pi),
    ind_prod       = mean(ind_prod)
  ) |>  
  ungroup() |> 
  unique() |> 
  # Add commodity prices data
  left_join(
    int_prices,
    by =  c(
      "product", "product_eng", "year", "quarter")
  ) |> 
  group_by(product, region) |> 
  mutate(
    int_price_inf = mFilter::hpfilter(
      int_price_inf, freq = 1600, type = "lambda")[["cycle"]]
  ) |> 
  # Add weather data and ENSO 
  left_join(
    Weather |> 
      dplyr::select(-IDDPTO),
    by = c(
      "year" = "year",
      "quarter" = "quarter",
      "region" = "DEPARTAMEN"
    )
  )

save(data_total, file = "../data/output/dataset_2001_2015-quarter.rda")
# write_csv(data_total, file = "../data/output/dataset_2001_2015.csv")

# Dataset for the Local Projections----

# Recode region as a factor
data_total <- 
  data_total |> 
  mutate(region_id = factor(region_id))
  

# The crops we focus on
crops <- c("Rice", "Dent corn", "Potato", "Cassava")

# Some desc. stat checks:
# Number of observation in each region, for each crop
data_total |> 
  group_by(product_eng, region_id) |> 
  summarise(n = sum(Value_prod <= 0)) |> 
  arrange(desc(n))

# Corresponding names of crops between Spanish and English
data_total |> 
  dplyr::select(product, product_eng) |> 
  unique()

# Id associated to each region
data_total |> 
  dplyr::select(region, region_id) |> 
  unique()

# Load function in utils
source("../weatherperu/R/utils.R")

# Load detrending functions
source("../weatherperu/R/detrending-quarter.R")

## Percentage deviation from regional quarterly production----

# Detrending the production data for each crop in each region:
# First, table with all combinations of crop x region
product_and_regions <- 
  data_total |> 
  filter(product_eng %in% crops) |> 
  dplyr::select(product_eng, region_id, region) |> 
  unique()

df_pct_pred_production <- vector(mode = "list", length = nrow(product_and_regions))
cli_progress_bar(total = nrow(product_and_regions))
for(i in 1:nrow(product_and_regions)){
  df_pct_pred_production[[i]] <- pct_prod_production(
    df = data_total, 
    crop_name = product_and_regions$product_eng[i], 
    region_id = product_and_regions$region_id[i]
  )
  cli_progress_update(set = i)
}
df_pct_pred_production <- bind_rows(df_pct_pred_production)


# The number of quarters with 0 values for ag. production
df_pct_pred_production |> 
  group_by(product_eng, region_id) |> 
  summarise(nb_0 = sum(y_new == 0)) |> 
  arrange(desc(nb_0))


# Add the other characteristics to the production data
df <- df_pct_pred_production |> 
  left_join(
    data_total,
    join_by(product_eng, region_id, year, quarter)
  )

## Missing values for the weather variables----

# Let us also impute missing values for the weather variables
weather_variables <- 
  weather_quarter_regions_df |> 
  select(where(is.numeric)) |> 
  select(-year, -quarter) |> 
  colnames()

df <- 
  df |> 
  mutate(
    across(
      .cols = !!weather_variables,
      .fns = ~ imputeTS::na_interpolation(.x, maxgap = 3)
    )
  )

# How many NAs left for those weather variables?
df |> 
  summarise(
    across(
      .cols = !!weather_variables,
      .fns = ~ sum(is.na(.x)),
      .names = "{.col}_nb_na"
    )
  ) |> 
  unlist()


# Add labels to the new columns
df <- 
  df |> 
  labelled::set_variable_labels(
    y_new = "Quarterly Agricultural Production (tons)",
    y_dev_pct = "Quarterly Agricultural Production (pct. deviation from regional quarterly mean)"
  )


save(df, file = "../data/output/df_lp_quarter.rda")
