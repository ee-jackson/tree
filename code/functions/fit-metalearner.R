#' Fit meta-learner.
#' @param df_train The training data.
#' @param df_assigned The full dataset to which treatment has been assigned.
#' @param var_omit Logical indicating if `soil_carbon_initial` should be omitted from the feature list.
#' @param test_plot_location Should test plots should be selected by stratified random sampling `stratified` or selected from a geographically distinct area `edge` or `core`.
#' @param learner The choice of metalearner `s`, `t` or `x`
#' @param restrict_confounder Logical indicating if the confounders for propensity score estimation should
#' be restricted to `soil_carbon_initial`, `soil_moist_code`, `mat_5yr`? Only valid when `learner == x`.
#' @return The ITE
#' @import dplyr causalToolbox
#' @importFrom tidyselect all_of
#' @export

fit_metalearner <- function(df_train, df_assigned, learner, var_omit = FALSE,
                            test_plot_location, restrict_confounder = FALSE,
                            seed = NULL) {
  set.seed(seed = seed)

  train_plot_list <- pull(df_train, description)

  test_data_core <- df_assigned |>
    dplyr::filter(sampling_location == "core")

  test_data_edge <- df_assigned |>
    dplyr::filter(sampling_location == "edge")

  test_data_stratified <- df_assigned |>
    dplyr::filter(sampling_location == "stratified")

  if (test_plot_location == "core") {

    test_data <- test_data_core

  } else if (test_plot_location == "edge") {

    test_data <- test_data_edge

  } else if (test_plot_location == "stratified") {

    test_data <- test_data_stratified

  } else {

    print("`test_plot_location` should be either `stratified`, `edge` or `core`")

  }

  if (var_omit == FALSE) {

    feat_list <- c("soil_moist_code", "mat_5yr", "soil_carbon_initial",
                   "map_5yr", "altitude", "no_of_stems", "ditch",
                   "volume_pine", "volume_spruce", "volume_birch",
                   "volume_aspen", "volume_oak", "volume_beech",
                   "volume_southern_broadleaf", "volume_contorta",
                   "volume_other_broadleaf", "volume_larch")

  } else if (var_omit == TRUE) {

    feat_list <- c("soil_moist_code", "mat_5yr",
                   "map_5yr", "altitude", "no_of_stems", "ditch",
                   "volume_pine", "volume_spruce", "volume_birch",
                   "volume_aspen", "volume_oak", "volume_beech",
                   "volume_southern_broadleaf", "volume_contorta",
                   "volume_other_broadleaf", "volume_larch")

  } else {

    print("`var_omit` should be either `TRUE` or `FALSE`")

  }


  if (learner == "s" & restrict_confounder == FALSE) {

    # create the hte object
    s_learn <- causalToolbox::S_RF(
      feat = dplyr::select(df_train, tidyselect::all_of(feat_list)),
      tr = df_train$tr,
      yobs = df_train$soil_carbon_obs,
      nthread = 2)

    # estimate the CATE
    cate_s_learn <- causalToolbox::EstimateCate(s_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    s_learn_out <- test_data |>
      dplyr::mutate(cate_pred = cate_s_learn,
             cate_real = soil_carbon_1 - soil_carbon_0)

    return(s_learn_out)

  } else if (learner == "t" & restrict_confounder == FALSE) {

    # create the hte object
    t_learn <- causalToolbox::T_RF(
      feat = dplyr::select(df_train, tidyselect::all_of(feat_list)),
      tr = df_train$tr,
      yobs = df_train$soil_carbon_obs,
      nthread = 2)

    # estimate the CATE
    cate_t_learn <- causalToolbox::EstimateCate(t_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    t_learn_out <- test_data |>
      dplyr::mutate(cate_pred = cate_t_learn,
             cate_real = soil_carbon_1 - soil_carbon_0)

    return(t_learn_out)

  } else if (learner == "x" & restrict_confounder == FALSE) {

    # create the hte object
    x_learn <- causalToolbox::X_RF(
      feat = dplyr::select(df_train,
                           tidyselect::all_of(feat_list)),
      tr = df_train$tr,
      yobs = df_train$soil_carbon_obs,
      nthread = 2)

    cate_x_learn <- causalToolbox::EstimateCate(x_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    x_learn_out <- test_data |>
      dplyr::mutate(cate_pred = cate_x_learn,
             cate_real = soil_carbon_1 - soil_carbon_0)

    return(x_learn_out)

  } else if (learner == "x" & restrict_confounder == TRUE & var_omit == FALSE) {

    # create the hte object
    x_learn <- causalToolbox::X_RF(
      feat = dplyr::select(df_train,
                           tidyselect::all_of(feat_list)),
      tr = df_train$tr,
      yobs = df_train$soil_carbon_obs,
      nthread = 2,
      e.forestry = list(relevant.Variable = 1:3,
                        ntree = 500, replace = TRUE, sample.fraction = 0.5,
                        mtry = 3, nodesizeSpl = 11, nodesizeAvg = 33,
                        nodesizeStrictSpl = 2, nodesizeStrictAvg = 1, splitratio = 1,
                        middleSplit = FALSE, OOBhonest = TRUE))

    cate_x_learn <- causalToolbox::EstimateCate(x_learn,
                                                dplyr::select(test_data,
                                                              tidyselect::all_of(feat_list)))

    x_learn_out <- test_data |>
      dplyr::mutate(cate_pred = cate_x_learn,
                    cate_real = soil_carbon_1 - soil_carbon_0)

    return(x_learn_out)

  }else if (learner == "x" & restrict_confounder == TRUE & var_omit == TRUE) {

    # create the hte object
    x_learn <- causalToolbox::X_RF(
      feat = dplyr::select(df_train,
                           tidyselect::all_of(feat_list)),
      tr = df_train$tr,
      yobs = df_train$soil_carbon_obs,
      nthread = 2,
      e.forestry = list(relevant.Variable = 1:2,
                        ntree = 500, replace = TRUE, sample.fraction = 0.5,
                        mtry = 2, nodesizeSpl = 11, nodesizeAvg = 33,
                        nodesizeStrictSpl = 2, nodesizeStrictAvg = 1, splitratio = 1,
                        middleSplit = FALSE, OOBhonest = TRUE))

    cate_x_learn <- causalToolbox::EstimateCate(x_learn,
                                                dplyr::select(test_data,
                                                              tidyselect::all_of(feat_list)))

    x_learn_out <- test_data |>
      dplyr::mutate(cate_pred = cate_x_learn,
                    cate_real = soil_carbon_1 - soil_carbon_0)

    return(x_learn_out)

  } else {
    print("learner should be either 's', 't' or 'x'.
          restrict_confounder == TRUE is only valid when learner == 'x'")
  }
}

# Change Rforestry function so that warning is thrown rather than error when
# changing x-leaner propensity model confounders

custom_testing_data_checker <- function(object, newdata, hasNas) {
  if(ncol(newdata) != object@processed_dta$numColumns) {
    warning(paste0("newdata has ", ncol(newdata), " but the forest was trained with ",
                   object@processed_dta$numColumns, " columns.")
    )
  }
  if(!is.null(object@processed_dta$featNames)) {
    if(!all(names(newdata) == object@processed_dta$featNames)) {
      warning("newdata columns have been reordered so that they match the training feature matrix")
      matchingPositions <- match(object@processed_dta$featNames, names(newdata))
      newdata <- newdata[, matchingPositions]
    }
  }

  # If linear is true we can't predict observations with some features missing.
  if(object@linear && any(is.na(newdata))) {
    stop("linear does not support missing data")
  }
  return(newdata)
}

environment(custom_testing_data_checker) <- asNamespace('Rforestry')
assignInNamespace("testing_data_checker", custom_testing_data_checker, ns = "Rforestry")
