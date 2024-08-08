#' Applies HP filter for annual data and returns the trend component
#'
#' @param x vector of annual observations
hp_filter_trend <- function(x, freq = 6.25) {
  res_hp_filter <- mFilter::hpfilter(
    x,
    freq = freq,
    type = "lambda",
    drift = FALSE
  )
  as.vector(res_hp_filter$trend)
}



#' Computes the percentage deviation of production from annual regional average
#'
#' @param df data
#' @param crop_name name of the crop
#' @param region_id id of the region
#'
#' @returns data frame with the product, the region id, the date, the production
#' with imputed missing values (`y_new`), the production demeaned (`y_new_normalized`),
#' the percentage deviation from mean production (`y_dev_pct`), the percentage
#' deviation from mean production minus an estimated quadratic trend (estimated
#'  by OLS) (`y`), and, a trend (`t`)
#' @export
#' @importFrom dplyr filter arrange mutate select row_number group_by
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom imputeTS na_interpolation
#' @importFrom stats lm predict residuals
pct_prod_production <- function(df,
                                crop_name,
                                region_id) {
  # The current data
  df_current <-
    df |>
    filter(
      product_eng == !!crop_name,
      region_id == !!region_id
    ) |>
    arrange(year)

  ## Dealing with missing values ----
  # Look for negative production values
  df_current <-
    df_current |>
    mutate(
      y_new = ifelse(Value_prod < 0, NA, Value_prod)
    )

  if (any(is.na(df_current$y_new))) {

    # Replacing NAs by interpolation
    # If there are more than two contiguous NAs, they are not replaced
    df_current <-
      df_current |>
      mutate(
        y_new = imputeTS::na_interpolation(y_new, maxgap = 3)
      )

    # Removing obs at the beginning/end if they are still missing
    df_current <-
      df_current |>
      mutate(
        row_to_keep = !(is.na(y_new) & row_number() %in% c(1:2, (n()-1):(n())))
      ) |>
      filter(row_to_keep) |>
      select(-row_to_keep)

    # Keeping the longest series of continuous non-NA values
    df_current <-
      df_current |>
      mutate(
        row_to_keep = keep_values_longest_non_na(y_new)
      ) |>
      filter(row_to_keep) |>
      select(-row_to_keep)
  }

  rle_y_new <- rle(df_current$y_new)
  check_contiguous_zeros <- rle_y_new$lengths[rle_y_new$values==0]


  ## Percent deviation from regional average over the period
  resul <-
    df_current |>
    ungroup() |>
    mutate(
      y_new_normalized = case_when(
        mean(y_new) == 0~ 0,
        TRUE ~ y_new / mean(y_new)
      ),
      y_dev_pct = case_when(
        mean(y_new) == 0 ~ 0,
        TRUE ~ (y_new - mean(y_new)) / mean(y_new)
      )
    ) |>
    arrange(year) |>
    mutate(t = row_number()) |>
    nest(.by = c(product_eng, region_id)) |>
    mutate(
      ols_fit   = map(data, ~ lm(y_new_normalized ~ -1 + t + I(t^2), data = .x)),
      resid     = map(ols_fit, residuals),
      fitted    = map(ols_fit, predict)
    ) |>
    unnest(cols = c(data, resid, fitted)) |>
    mutate(
      y = resid
    ) |>
    select(
      product_eng, region_id, year,
      y_new, y_dev_pct, y_new_normalized, y, t
    ) |>
    ungroup() |>
    arrange(year)

  resul
}
