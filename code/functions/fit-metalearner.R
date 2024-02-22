#' Fit meta-learner
#' @param df The clean data.
#' @param prop_not_treated Sample imbalance - the proportion of the data that will be assigned 0.
#' @param learner Choice of meta-learner "s", "t" or "x".
#' @param n_train The number of plots in the train dataset (sample size). Maximum is 1,414.
#' @param var_omit Logical indicating if `soil_carbon_initial` should be omitted from the predictor variables.
#' @param random_test_plots Logical indicating if test plots should be randomly selected `TRUE` or selected from a geographically distinct area `FALSE`.
#' @return The ITE
#' @import dplyr causalToolbox
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @export

fit_metalearner <- function(df, prop_not_treated, n_train, learner,
                            var_omit = FALSE, random_test_plots = TRUE) {

  features <- df |>
    dplyr::filter(period == 0) |>
    dplyr::select(
      description,
      soil_moist_code,
      altitude, mat_5yr, map_5yr, ditch, no_of_stems, volume_pine, volume_spruce,
      volume_birch, volume_aspen, volume_oak, volume_beech,
      volume_southern_broadleaf, volume_larch
    )

  data_obs <- df |>
    dplyr::filter(in_square == FALSE) |>
    dplyr::select(description, tr, control_category_name, total_soil_carbon) |>
    tidyr::pivot_wider(id_cols = c(description, tr),
                names_from = control_category_name,
                values_from = total_soil_carbon) |>
    dplyr::mutate(soil_carbon_obs =
                    dplyr::case_when(tr == 0 ~ `SetAside (Unmanaged)`,
                                       tr == 1 ~ `BAU - NoThinning`)) |>
    dplyr::rename(soil_carbon_initial = `Initial state`,
           soil_carbon_0 = `SetAside (Unmanaged)`,
           soil_carbon_1 = `BAU - NoThinning`) |>
    dplyr::left_join(features,
                     by = "description")

  lat_lon <- df |>
    dplyr::select(description, ost_wgs84, nord_wgs84) %>%
    dplyr::distinct() %>%
    tidyr::drop_na()

  # sample train
  train_data_0 <- data_obs |>
    dplyr::filter(tr == 0) |>
    dplyr::slice_sample(n = as.integer(n_train*prop_not_treated))

  train_data_1 <- data_obs |>
    dplyr::filter(tr == 1) |>
    dplyr::slice_sample(n = n_train - as.integer(n_train*prop_not_treated))

  train_data <- dplyr::bind_rows(train_data_0, train_data_1)

  # sample random test
  test_data_0 <- data_obs |>
    dplyr::filter(! description %in% train_data$description) |>
    dplyr::filter(tr == 0) |>
    dplyr::slice_sample(n = 98)

  test_data_1 <- data_obs |>
    dplyr::filter(! description %in% train_data$description) |>
    dplyr::filter(tr == 1) |>
    dplyr::slice_sample(n = 98)

  test_data_random <- dplyr::bind_rows(test_data_0, test_data_1)

  # give "square" test data even treatment assignment
  no_treat_ids_square <- df |>
    dplyr::filter(in_square == TRUE) |>
    dplyr::select(description) |>
    dplyr::distinct() |>
    dplyr::slice_sample(prop = 0.5)

  data_assigned_square <- df |>
    dplyr::filter(in_square == TRUE) |>
    dplyr::mutate(tr =
                    dplyr::case_when(
                      description %in% no_treat_ids_square$description ~ 0,
                      .default = 1)
    )

  test_data_square <- data_assigned_square |>
    dplyr::select(description, tr, control_category_name, total_soil_carbon) |>
    tidyr::pivot_wider(id_cols = c(description, tr),
                       names_from = control_category_name,
                       values_from = total_soil_carbon) |>
    dplyr::mutate(soil_carbon_obs =
                    dplyr::case_when(tr == 0 ~ `SetAside (Unmanaged)`,
                                     tr == 1 ~ `BAU - NoThinning`)) |>
    dplyr::rename(soil_carbon_initial = `Initial state`,
                  soil_carbon_0 = `SetAside (Unmanaged)`,
                  soil_carbon_1 = `BAU - NoThinning`) |>
    dplyr::left_join(features,
                     by = "description")

  if (random_test_plots == FALSE) {
    test_data <- test_data_square
  } else if (random_test_plots == TRUE) {
    test_data <- test_data_random
  } else {
    print0("`random_test_plots` should be either `TRUE` or `FALSE`")
  }

  if (var_omit == FALSE) {
    feat_list <- c("soil_carbon_initial", "altitude",
                   "mat_5yr", "map_5yr", "ditch", "no_of_stems", "volume_pine",
                   "volume_spruce", "volume_birch", "volume_aspen",
                   "volume_oak", "volume_beech", "soil_moist_code",
                   "volume_southern_broadleaf", "volume_larch")
  } else if (var_omit == TRUE) {
    feat_list <- c("altitude",
                   "mat_5yr", "map_5yr", "ditch", "no_of_stems", "volume_pine",
                   "volume_spruce", "volume_birch", "volume_aspen",
                   "volume_oak", "volume_beech", "soil_moist_code",
                   "volume_southern_broadleaf", "volume_larch")
  } else {
    print0("`var_omit` should be either `TRUE` or `FALSE`")
  }


  if (learner == "s") {

    # create the hte object
    s_learn <- causalToolbox::S_RF(
      feat = dplyr::select(train_data, tidyselect::all_of(feat_list)),
      tr = train_data$tr,
      yobs = train_data$soil_carbon_obs,
      nthread = 2,
      mu.forestry = list(relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                  tidyselect::all_of(feat_list))),
                         ntree = 1000, replace = TRUE, sample.fraction = 0.9,
                         mtry = ncol(dplyr::select(train_data,
                                                   tidyselect::all_of(feat_list))),
                         nodesizeSpl = 1, nodesizeAvg = 3, nodesizeStrictSpl = 3,
                         nodesizeStrictAvg = 1, splitratio = 1, middleSplit = FALSE,
                         OOBhonest = TRUE)
    )

    # estimate the CATE
    cate_s_learn <- causalToolbox::EstimateCate(s_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    s_learn_out <- test_data |>
      dplyr::mutate(cate_s_learn = cate_s_learn,
             cate_real = soil_carbon_1 - soil_carbon_0,
             diff = cate_s_learn - cate_real) |>
      dplyr::left_join(lat_lon,
                       by = "description")

    return(s_learn_out)

  } else if (learner == "t") {

    # create the hte object
    t_learn <- causalToolbox::T_RF(
      feat = dplyr::select(train_data, tidyselect::all_of(feat_list)),
      tr = train_data$tr,
      yobs = train_data$soil_carbon_obs,
      nthread = 2,
      mu0.forestry = list(relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                   tidyselect::all_of(feat_list))),
                          ntree = 1000, replace = TRUE, sample.fraction = 0.9,
                          mtry = ncol(dplyr::select(train_data,
                                                    tidyselect::all_of(feat_list))),
                          nodesizeSpl = 1, nodesizeAvg = 3, nodesizeStrictSpl = 1,
                          nodesizeStrictAvg = 1, splitratio = 1, middleSplit = FALSE,
                          OOBhonest = TRUE),
      mu1.forestry = list(relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                   tidyselect::all_of(feat_list))),
                          ntree = 1000, replace = TRUE, sample.fraction = 0.9,
                          mtry = ncol(dplyr::select(train_data,
                                                    tidyselect::all_of(feat_list))),
                          nodesizeSpl = 1, nodesizeAvg = 3, nodesizeStrictSpl = 1,
                          nodesizeStrictAvg = 1, splitratio = 1, middleSplit = FALSE,
                          OOBhonest = TRUE)
    )

    # estimate the CATE
    cate_t_learn <- causalToolbox::EstimateCate(t_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    t_learn_out <- test_data |>
      dplyr::mutate(cate_t_learn = cate_t_learn,
             cate_real = soil_carbon_1 - soil_carbon_0,
             diff = cate_t_learn - cate_real) |>
      dplyr::left_join(lat_lon,
                       by = "description")

    return(t_learn_out)

  } else if (learner == "x") {

    # create the hte object
    x_learn <- causalToolbox::X_RF(
      feat = dplyr::select(train_data,
                           tidyselect::all_of(feat_list)),
      tr = train_data$tr,
      yobs = train_data$soil_carbon_obs,
      nthread = 2,
      mu.forestry = list(relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                  tidyselect::all_of(feat_list))),
                         ntree = 1000, replace = TRUE, sample.fraction = 0.8,
                         mtry = round(ncol(dplyr::select(train_data,
                                                         tidyselect::all_of(feat_list))) * 13/20),
                         nodesizeSpl = 2, nodesizeAvg = 1,
                         nodesizeStrictSpl = 2, nodesizeStrictAvg = 1, splitratio = 1,
                         middleSplit = TRUE, OOBhonest = TRUE),
      tau.forestry = list(relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                   tidyselect::all_of(feat_list))),
                          ntree = 1000, replace = TRUE, sample.fraction = 0.7,
                          mtry = round(ncol(dplyr::select(train_data,
                                                          tidyselect::all_of(feat_list))) * 17/20),
                          nodesizeSpl = 5, nodesizeAvg = 6,
                          nodesizeStrictSpl = 3, nodesizeStrictAvg = 1, splitratio = 1,
                          middleSplit = TRUE, OOBhonest = TRUE)
    )

    cate_x_learn <- causalToolbox::EstimateCate(x_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    x_learn_out <- test_data |>
      dplyr::mutate(cate_x_learn = cate_x_learn,
             cate_real = soil_carbon_1 - soil_carbon_0,
             diff = cate_x_learn - cate_real) |>
      dplyr::left_join(lat_lon,
                       by = "description")

    return(x_learn_out)

  } else {
    print0("learner should be either 's', 't' or 'x'")
  }
}
