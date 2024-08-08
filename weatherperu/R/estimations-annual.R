#' Estimate Local Projections
#'
#' @param df original dataset
#' @param horizons number of horizons
#' @param y_name name of the exogenous variable
#' @param group_name name of the group variable
#' @param crop_name name of the crop to focus on
#' @param control_names vector of names of the control variables
#' @param weather_names vector of names of the weather variables
#' @param detrend if `TRUE` a group-wise quadratic temporal effect is estimated
#'  (group:time + group:I(time^2))
#' @param add_year_fe should columns with year dummy variables be added?
#'   Default to `TRUE`
#' @param add_intercept should an intercept we added to the regressions?
#'   (default to `FALSE`)
#' @param share_geo vector of names of the variables that contain the share of
#'   each type of geographical pattern. By default `NULL`: no share used
#' @param std type of standard error (`"NW"` for Newey-West, `"Cluster"`,
#'   `"Standard"` otherwise)
#' @param transition_name name of the variable used to define the transition to
#'   the two states. By default `NULL`
#' @param transition_method if transition function, name of the method to use:
#'   `logistic` or `normal` (default to `NULL`, i.e., no transition)
#' @param state_names name of the two states in a vector of characters (only if
#'   `transition_name` is not `NULL`). First period corresponds to mapped values
#'   of `transition_name` close to 0, which is for large positive values of
#'   `transition_name`
#' @param gamma logistic growth rate (default to 3, only used if
#'   `transition_name` is not `NULL`)
#' @param other_var_to_keep vector of names of other variables to keep in the
#'   returned dataset (default to `NULL`: no additional vairable kept)
#' @export
#' @importFrom dplyr mutate sym ungroup summarise across left_join
#' @importFrom stringr str_c str_detect
#' @importFrom purrr map map_dbl list_rbind
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_longer
#' @importFrom sandwich NeweyWest
#' @importFrom stats sd model.matrix nobs residuals lm coef
estimate_linear_lp <- function(df,
                              horizons,
                              y_name,
                              group_name,
                              crop_name,
                              control_names,
                              weather_names,
                              detrend = FALSE,
                              add_year_fe = TRUE,
                              add_intercept = FALSE,
                              share_geo = NULL,
                              transition_name = NULL,
                              transition_method = NULL,
                              state_names = c("planted", "harvested"),
                              gamma = 3,
                              std = c("nw", "cluster", "standard"),
                              other_var_to_keep = NULL) {

  # Format the dataset
  data_lp <-
    get_data_lp(
      df = df,
      horizons = horizons,
      y_name = y_name,
      group_name = group_name,
      crop_name = crop_name,
      control_names = control_names,
      weather_names = weather_names,
      share_geo = share_geo,
      transition_name = transition_name,
      transition_method = transition_method,
      state_names = state_names,
      gamma = gamma,
      other_var_to_keep = other_var_to_keep
    )

  # Recode levels for the groups
  for(h in 0:horizons){
    data_lp[[h + 1]] <-
      data_lp[[h + 1]] |>
      mutate(
        !!group_name := as.factor(as.character(!!sym(group_name)))
      )
  }

  control_names_full <- control_names
  weather_names_full <- weather_names
  ind_names_groups <- str_detect(
    colnames(data_lp[[1]]), str_c("^", group_name, "_")
  )
  group_names_full <- colnames(data_lp[[1]])[ind_names_groups]

  if (!is.null(share_geo)) {
    # Name of the weather variables
    weather_names_full <- paste(
      rep(weather_names, each = length(share_geo)),
      share_geo,
      sep = "_"
    )
  }

  if (!is.null(transition_name)) {

    state_1_name <- str_c(state_names[1], "_")
    state_2_name <- str_c(state_names[2], "_")

    # Name of the variables
    weather_names_full <- str_c(
      rep(
        c(state_1_name, state_2_name),
        each = length(weather_names)
      ),
      rep(weather_names, 2)
    )
    control_names_full <- str_c(
      rep(
        c(state_1_name, state_2_name),
        each = length(control_names)
      ),
      rep(control_names, 2)
    )
    ind_names_groups <- str_detect(
      colnames(data_lp[[1]]),
      str_c("(^", state_1_name, "|", state_2_name, ")", group_name, "_")
    )
    group_names_full <- colnames(data_lp[[1]])[ind_names_groups]
  }

  # Observed standard deviations in the data
  sd_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = sd
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "std_shock") |>
    mutate(horizon = as.numeric(horizon))

  # Observed median value in the data
  median_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = ~quantile(.x, probs = .5)
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "median_shock") |>
    mutate(horizon = as.numeric(horizon))

  # Observed quantile of order 0.05 value in the data
  q05_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = ~quantile(.x, probs = .05)
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "q05_shock") |>
    mutate(horizon = as.numeric(horizon))

  # Observed quantile of order 0.95 value in the data
  q95_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = ~quantile(.x, probs = .95)
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "q95_shock") |>
    mutate(horizon = as.numeric(horizon))


  if (detrend == TRUE) {
    # Formula for the regressions
    formula_lp <- paste0(
      "y_lead",
      " ~ -1+",
      # " ~ 1+", # intercept
      paste(weather_names_full, collapse = " + "),
      " + ",
      paste(control_names_full, collapse = " + "),
      " + ",
      paste0(
        c(
          paste0(group_names_full, ":time"),
          paste0(group_names_full, ":I(time^2)")
        ),
        collapse = " + "
      )
    )
  } else {
    # Formula for the regressions
    formula_lp <- paste0(
      "y_lead",
      " ~ -1+",
      # " ~ 1+", # intercept
      paste(weather_names_full, collapse = " + "),
      " + ",
      paste(control_names_full, collapse = " + "),
      " + ",
      ifelse(
        add_intercept,
        # removing last group
        yes = paste(group_names_full[-length(group_names_full)], collapse = " + "),
        # keeping last group
        no = paste(group_names_full, collapse = " + ")
      )
    )
  }

  if (add_year_fe) {
    names_year_fe <-
      colnames(data_lp[[1]])[str_detect(colnames(data_lp[[1]]), "^year_[[:digit:]]{4}")]
    names_year_fe <- str_c(names_year_fe, collapse = " + ")
    formula_lp <- paste0(formula_lp, " + ", names_year_fe)
  }

  empty_res <- vector(mode = "list", length = horizons + 1)
  reg_lp <- empty_res
  sig_ols <- empty_res
  log_likelihood <- empty_res
  mse <- empty_res
  coefs <- empty_res
  cl_std <- empty_res


  for (h in 0:horizons) {
    # Global assignment... otherwise, errors with coeftest()
    current_data_h <<- data_lp[[h+1]]
    # Regression
    reg_h <- lm(formula = formula_lp, data = current_data_h)
    # Standard error of the residuals
    sig_ols_h <- sd(reg_h$residuals)
    # Log likelihood
    u_h <- reg_h$residuals
    log_likelihood_h <-
      sum(log(1 / sqrt(2 * pi * sig_ols_h^2) * exp(-u_h^2 / (2 * sig_ols_h^2))))
    mse_h <- mean(u_h^2)
    coefs_h <- enframe(coef(reg_h)) |> mutate(horizon = h)
    if (std == "Cluster") {
      cl_std_h <- coeftest(
        reg_h,
        vcov = vcovCL,
        cluster = formula(str_c("~", group_name)))[, "Std. Error"] |>
        enframe() |>
        mutate(horizon = h)
    } else {
      cl_std_h <- enframe(sqrt(diag(vcov(reg_h))), value = "std") |>
        mutate(horizon = h)
    }

    # Store results in lists
    reg_lp[[h+1]] <- reg_h
    sig_ols[[h+1]] <- sig_ols_h
    log_likelihood[[h+1]] <- log_likelihood_h
    mse[[h+1]] <- mse_h
    coefs[[h+1]] <- coefs_h
    cl_std[[h+1]] <- cl_std_h
  }
  se_df <- list_rbind(cl_std) |> rename(std = value)

  coefs <-
    coefs |> list_rbind() |>
    left_join(se_df, by = c("horizon", "name")) |>
    mutate(
      crop = crop_name,
      horizon = as.numeric(horizon)
    ) |>
    left_join(sd_weather_shock, by = c("horizon", "name")) |>
    left_join(median_weather_shock, by = c("horizon", "name")) |>
    left_join(q05_weather_shock, by = c("horizon", "name")) |>
    left_join(q95_weather_shock, by = c("horizon", "name"))

  list(
    # reg_lp = reg_lp,
    coefs = coefs,
    horizons = horizons,
    log_likelihood = log_likelihood |> list_c(),
    mse = mse |> list_c(),
    crop_name = crop_name,
    data_lp = data_lp
  )
}
