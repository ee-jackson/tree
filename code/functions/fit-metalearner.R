#' Fit meta-learner using tidymodels random forests.
#'
#' Hyperparameters are deliberately fixed by default. This is intended for large
#' simulation studies where tuning every base learner would be too expensive and
#' would make comparisons across meta-learners less clean.
#'
#' @param df_train The training data.
#' @param df_assigned The full dataset to which treatment has been assigned.
#' @param learner The choice of meta-learner: "s", "t", "x", or "dr".
#' @param var_omit Omission of a variable from the feature list, either `none`,
#'    or `omit_covariate`
#' @param test_plot_location Test plots selected from "stratified", "edge", or
#'   "core".
#' @param seed Optional random seed.
#' @param trees Number of trees for each random forest.
#' @param mtry Number of variables randomly sampled at each split. If NULL, uses
#'   floor(sqrt(p)), where p is the number of predictors for that model.
#' @param min_n Minimum node size.
#' @param num_threads Number of ranger threads per base learner. Use 1 if
#'   parallelising over simulation runs.
#' @param trim Propensity score truncation value.
#' @param return_model Logical. If TRUE, attaches fitted model objects in a
#'   list-column called `metalearner_fit`. Defaults to FALSE to keep simulation
#'   output smaller.
#' @return Test data with `cate_pred` and `cate_real` columns.
#' @import dplyr tidymodels ranger
#' @importFrom tidyselect all_of
#' @export

fit_metalearner <- function(df_train, df_assigned, learner, var_omit = FALSE,
                            test_plot_location = "stratified",
                            seed = NULL,
                            trees = 500,
                            mtry = NULL,
                            min_n = 5,
                            num_threads = 1,
                            trim = 0.01,
                            return_model = FALSE) {
  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  learner <- as.character(learner)
  test_plot_location <- as.character(test_plot_location)
  learner <- tolower(learner)

  if (!learner %in% c("s", "t", "x", "dr")) {
    stop("`learner` should be one of 's', 't', 'x', or 'dr'.", call. = FALSE)
  }

  if (!test_plot_location %in% c("stratified", "edge", "core")) {
    stop("`test_plot_location` should be 'stratified', 'edge', or 'core'.",
         call. = FALSE)
  }

  if (!var_omit %in% c("none", "omit_covariate")) {
    stop("`var_omit` should be 'omit_covariate', or 'none'.", call. = FALSE)
  }

  feat_list <- get_metalearner_features(var_omit = var_omit)
  prop_feat_list <- feat_list

  test_data <- df_assigned |>
    dplyr::filter(.data$sampling_location == test_plot_location)

  train_data <- prepare_metalearner_data(df_train)
  test_data_model <- prepare_metalearner_data(test_data)

  p <- length(feat_list)
  mtry_outcome <- resolve_mtry(mtry = mtry, p = p)
  mtry_prop <- resolve_mtry(mtry = mtry, p = length(prop_feat_list))

  rf_reg_spec <- make_rf_reg_spec(
    trees = trees,
    mtry = mtry_outcome,
    min_n = min_n,
    num_threads = num_threads
  )

  rf_prop_spec <- make_rf_class_spec(
    trees = trees,
    mtry = mtry_prop,
    min_n = min_n,
    num_threads = num_threads
  )

  fit <- switch(
    learner,
    s = fit_s_learner_tidymodels(
      train_data = train_data,
      test_data = test_data_model,
      feat_list = feat_list,
      rf_reg_spec = rf_reg_spec
    ),
    t = fit_t_learner_tidymodels(
      train_data = train_data,
      test_data = test_data_model,
      feat_list = feat_list,
      rf_reg_spec = rf_reg_spec
    ),
    x = fit_x_learner_tidymodels(
      train_data = train_data,
      test_data = test_data_model,
      feat_list = feat_list,
      prop_feat_list = prop_feat_list,
      rf_reg_spec = rf_reg_spec,
      rf_prop_spec = rf_prop_spec,
      trim = trim
    ),
    dr = fit_dr_learner_tidymodels(
      train_data = train_data,
      test_data = test_data_model,
      feat_list = feat_list,
      prop_feat_list = prop_feat_list,
      rf_reg_spec = rf_reg_spec,
      rf_prop_spec = rf_prop_spec,
      trim = trim
    )
  )

  out <- test_data |>
    dplyr::mutate(
      cate_pred = fit$cate_pred,
      cate_real = .data$soil_carbon_1 - .data$soil_carbon_0
    )

  if (return_model) {
    out <- out |>
      dplyr::mutate(metalearner_fit = list(fit))
  }

  out
}

get_metalearner_features <- function(var_omit = "none") {
  base_features <- c(
    "soil_moist_code", "mat_5yr", "soil_carbon_initial",
    "map_5yr", "altitude", "no_of_stems", "ditch",
    "volume_pine", "volume_spruce", "volume_birch",
    "volume_aspen", "volume_oak", "volume_beech",
    "volume_southern_broadleaf", "volume_contorta",
    "volume_other_broadleaf", "volume_larch"
  )

  if (var_omit == "omit_covariate") {
    base_features <- setdiff(base_features, "soil_moist_code")
  }

  base_features
}

prepare_metalearner_data <- function(data) {
  data |>
    dplyr::mutate(
      .tr_num = as.integer(.data$tr),
      .tr_factor = factor(
        dplyr::if_else(.data$.tr_num == 1L, "treated", "control"),
        levels = c("control", "treated")
      )
    )
}

resolve_mtry <- function(mtry = NULL, p) {
  if (!is.null(mtry)) {
    return(max(1L, min(as.integer(mtry), as.integer(p))))
  }

  max(1L, floor(sqrt(p)))
}

make_rf_reg_spec <- function(trees = 500, mtry, min_n = 5,
                             num_threads = 1) {
  parsnip::rand_forest(
    trees = trees,
    mtry = mtry,
    min_n = min_n
  ) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine(
      "ranger",
      importance = "none",
      num.threads = num_threads
    )
}

make_rf_class_spec <- function(trees = 500, mtry, min_n = 5,
                               num_threads = 1) {
  parsnip::rand_forest(
    trees = trees,
    mtry = mtry,
    min_n = min_n
  ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine(
      "ranger",
      probability = TRUE,
      importance = "none",
      num.threads = num_threads
    )
}

make_formula <- function(outcome, predictors) {
  stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
}

fit_regression_workflow <- function(data, outcome, predictors, spec) {
  workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(make_formula(outcome, predictors)) |>
    parsnip::fit(data = data)
}

predict_regression <- function(fit, new_data) {
  stats::predict(fit, new_data = new_data)$.pred
}

fit_propensity_workflow <- function(data, predictors, spec) {
  workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(make_formula(".tr_factor", predictors)) |>
    parsnip::fit(data = data)
}

predict_propensity <- function(fit, new_data, trim = 0.01) {
  pred <- stats::predict(fit, new_data = new_data, type = "prob")

  if (!".pred_treated" %in% names(pred)) {
    stop("Could not find `.pred_treated` in propensity predictions.",
         call. = FALSE)
  }

  pmin(pmax(pred$.pred_treated, trim), 1 - trim)
}

fit_s_learner_tidymodels <- function(train_data, test_data, feat_list,
                                     rf_reg_spec) {
  predictors <- c(".tr_factor", feat_list)

  mu_fit <- fit_regression_workflow(
    data = train_data,
    outcome = "soil_carbon_obs",
    predictors = predictors,
    spec = rf_reg_spec
  )

  test_treated <- test_data |>
    dplyr::mutate(.tr_factor = factor("treated", levels = c("control", "treated")))

  test_control <- test_data |>
    dplyr::mutate(.tr_factor = factor("control", levels = c("control", "treated")))

  cate_pred <- predict_regression(mu_fit, test_treated) -
    predict_regression(mu_fit, test_control)

  list(
    cate_pred = cate_pred,
    models = list(mu = mu_fit)
  )
}

fit_t_learner_tidymodels <- function(train_data, test_data, feat_list,
                                     rf_reg_spec) {
  treated_data <- train_data |>
    dplyr::filter(.data$.tr_num == 1L)

  control_data <- train_data |>
    dplyr::filter(.data$.tr_num == 0L)

  check_treatment_split(treated_data, control_data)

  mu1_fit <- fit_regression_workflow(
    data = treated_data,
    outcome = "soil_carbon_obs",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  mu0_fit <- fit_regression_workflow(
    data = control_data,
    outcome = "soil_carbon_obs",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  cate_pred <- predict_regression(mu1_fit, test_data) -
    predict_regression(mu0_fit, test_data)

  list(
    cate_pred = cate_pred,
    models = list(mu1 = mu1_fit, mu0 = mu0_fit)
  )
}

fit_x_learner_tidymodels <- function(train_data, test_data, feat_list,
                                     prop_feat_list, rf_reg_spec,
                                     rf_prop_spec, trim = 0.01) {
  treated_data <- train_data |>
    dplyr::filter(.data$.tr_num == 1L)

  control_data <- train_data |>
    dplyr::filter(.data$.tr_num == 0L)

  check_treatment_split(treated_data, control_data)

  mu1_fit <- fit_regression_workflow(
    data = treated_data,
    outcome = "soil_carbon_obs",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  mu0_fit <- fit_regression_workflow(
    data = control_data,
    outcome = "soil_carbon_obs",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  treated_tau_data <- treated_data |>
    dplyr::mutate(.tau_pseudo = .data$soil_carbon_obs - predict_regression(mu0_fit, treated_data))

  control_tau_data <- control_data |>
    dplyr::mutate(.tau_pseudo = predict_regression(mu1_fit, control_data) - .data$soil_carbon_obs)

  tau1_fit <- fit_regression_workflow(
    data = treated_tau_data,
    outcome = ".tau_pseudo",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  tau0_fit <- fit_regression_workflow(
    data = control_tau_data,
    outcome = ".tau_pseudo",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  prop_fit <- fit_propensity_workflow(
    data = train_data,
    predictors = prop_feat_list,
    spec = rf_prop_spec
  )

  e_hat_test <- predict_propensity(prop_fit, test_data, trim = trim)

  tau1_pred <- predict_regression(tau1_fit, test_data)
  tau0_pred <- predict_regression(tau0_fit, test_data)

  cate_pred <- e_hat_test * tau0_pred + (1 - e_hat_test) * tau1_pred

  list(
    cate_pred = cate_pred,
    models = list(
      mu1 = mu1_fit,
      mu0 = mu0_fit,
      tau1 = tau1_fit,
      tau0 = tau0_fit,
      propensity = prop_fit
    )
  )
}

fit_dr_learner_tidymodels <- function(train_data, test_data, feat_list,
                                      prop_feat_list, rf_reg_spec,
                                      rf_prop_spec,
                                      trim = 0.01) {
  treated_data <- train_data |>
    dplyr::filter(.data$.tr_num == 1L)

  control_data <- train_data |>
    dplyr::filter(.data$.tr_num == 0L)

  check_treatment_split(treated_data, control_data)

  mu1_fit <- fit_regression_workflow(
    data = treated_data,
    outcome = "soil_carbon_obs",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  mu0_fit <- fit_regression_workflow(
    data = control_data,
    outcome = "soil_carbon_obs",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  prop_fit <- fit_propensity_workflow(
    data = train_data,
    predictors = prop_feat_list,
    spec = rf_prop_spec
  )

  mu1_hat <- predict_regression(mu1_fit, train_data)
  mu0_hat <- predict_regression(mu0_fit, train_data)
  e_hat <- predict_propensity(prop_fit, train_data, trim = trim)

  dr_score <- mu1_hat - mu0_hat +
    train_data$.tr_num * (train_data$soil_carbon_obs - mu1_hat) / e_hat -
    (1 - train_data$.tr_num) * (train_data$soil_carbon_obs - mu0_hat) / (1 - e_hat)

  tau_data <- train_data |>
    dplyr::mutate(.dr_score = dr_score)

  tau_fit <- fit_regression_workflow(
    data = tau_data,
    outcome = ".dr_score",
    predictors = feat_list,
    spec = rf_reg_spec
  )

  cate_pred <- predict_regression(tau_fit, test_data)

  list(
    cate_pred = cate_pred,
    models = list(
      mu1 = mu1_fit,
      mu0 = mu0_fit,
      propensity = prop_fit,
      tau = tau_fit
    ),
    nuisance_predictions = list(
      mu1_hat = mu1_hat,
      mu0_hat = mu0_hat,
      e_hat = e_hat,
      dr_score = dr_score
    )
  )
}

check_treatment_split <- function(treated_data, control_data) {
  if (nrow(treated_data) < 2L || nrow(control_data) < 2L) {
    stop("Both treatment groups need at least two observations to fit the learner.",
         call. = FALSE)
  }
}
