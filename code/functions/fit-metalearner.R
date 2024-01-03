#' Fit meta-learner
#' @param df The clean data.
#' @param learner Choice of meta-learner "s", "t" or "x".
#' @param n_train The number of plots in the train dataset (sample size).
#' @param var_omit Logical indicating if `soil_carbon_initial` should be omitted from the predictor variables.
#' @return The ITE
#' @import dplyr causalToolbox
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @export

fit_metalearner <- function(df, learner, n_train, var_omit) {

  features <- df |>
    dplyr::filter(period == 0) |>
    dplyr::select(
      description,
      soil_moist_code,
      altitude, mat, map, ditch, no_of_stems, volume_pine, volume_spruce,
      volume_birch, volume_aspen, volume_oak, volume_beech,
      volume_southern_broadleaf, volume_larch
    )

  data_obs <- df |>
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
    dplyr::slice_sample(n = n_train/2)

  train_data_1 <- data_obs |>
    dplyr::filter(tr == 1) |>
    dplyr::slice_sample(n = n_train/2)

  train_data <- dplyr::bind_rows(train_data_0, train_data_1)

  # sample test
  test_data_0 <- data_obs |>
    dplyr::filter(! description %in% train_data$description) |>
    dplyr::filter(tr == 0) |>
    dplyr::slice_sample(n = 107)

  test_data_1 <- data_obs |>
    dplyr::filter(! description %in% train_data$description) |>
    dplyr::filter(tr == 1) |>
    dplyr::slice_sample(n = 107)

  test_data <- dplyr::bind_rows(test_data_0, test_data_1)

  if (var_omit == FALSE) {
    feat_list <- c("soil_carbon_initial", "altitude",
                   "mat", "map", "ditch", "no_of_stems", "volume_pine",
                   "volume_spruce", "volume_birch", "volume_aspen",
                   "volume_oak", "volume_beech", "soil_moist_code",
                   "volume_southern_broadleaf", "volume_larch")
  } else if (var_omit == TRUE) {
    feat_list <- c("altitude",
                   "mat", "map", "ditch", "no_of_stems", "volume_pine",
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
      mu.forestry = list(mtry = 6, nodesizeSpl = 2,
                         relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                  tidyselect::all_of(feat_list))),
                         ntree = 1000, replace = TRUE,
                         sample.fraction = 0.9, nodesizeAvg = 3,
                         nodesizeStrictSpl = 3, nodesizeStrictAvg = 1,
                         splitratio = 1, middleSplit = FALSE, OOBhonest = TRUE)
    )

    # estimate the CATE
    cate_s_learn <- causalToolbox::EstimateCate(s_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    s_learn_out <- test_data |>
      dplyr::mutate(cate_s_learn = cate_s_learn,
             cate_real = soil_carbon_1 - soil_carbon_0,
             diff = abs(cate_s_learn - cate_real)) |>
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
      mu0.forestry = list(mtry = 6, nodesizeSpl = 2,
                          relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                   tidyselect::all_of(feat_list))),
                          ntree = 1000, replace = TRUE,
                          sample.fraction = 0.9, nodesizeAvg = 3,
                          nodesizeStrictSpl = 1, nodesizeStrictAvg = 1,
                          splitratio = 1, middleSplit = FALSE,
                          OOBhonest = TRUE),
      mu1.forestry = list(mtry = 6, nodesizeSpl = 2,
                          relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                   tidyselect::all_of(feat_list))),
                          ntree = 1000, replace = TRUE,
                          sample.fraction = 0.9, nodesizeAvg = 3,
                          nodesizeStrictSpl = 1, nodesizeStrictAvg = 1,
                          splitratio = 1, middleSplit = FALSE,
                          OOBhonest = TRUE)
    )

    # estimate the CATE
    cate_t_learn <- causalToolbox::EstimateCate(t_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    t_learn_out <- test_data |>
      dplyr::mutate(cate_t_learn = cate_t_learn,
             cate_real = soil_carbon_1 - soil_carbon_0,
             diff = abs(cate_t_learn - cate_real)) |>
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
      mu.forestry = list(mtry = 6, nodesizeSpl = 2,
                         relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                  tidyselect::all_of(feat_list))),
                         ntree = 1000, replace = TRUE,
                         sample.fraction = 0.8, nodesizeAvg = 1,
                         nodesizeStrictSpl = 2, nodesizeStrictAvg = 1,
                         splitratio = 1, middleSplit = TRUE, OOBhonest = TRUE),
      tau.forestry = list(mtry = 6, nodesizeSpl = 2,
                          relevant.Variable = 1:ncol(dplyr::select(train_data,
                                                                   tidyselect::all_of(feat_list))),
                          ntree = 1000, replace = TRUE,
                          sample.fraction = 0.7, nodesizeAvg = 6,
                          nodesizeStrictSpl = 3, nodesizeStrictAvg = 1,
                          splitratio = 1, middleSplit = TRUE, OOBhonest = TRUE)
    )

    cate_x_learn <- causalToolbox::EstimateCate(x_learn,
                                 dplyr::select(test_data,
                                               tidyselect::all_of(feat_list)))

    x_learn_out <- test_data |>
      dplyr::mutate(cate_x_learn = cate_x_learn,
             cate_real = soil_carbon_1 - soil_carbon_0,
             diff = abs(cate_x_learn - cate_real)) |>
      dplyr::left_join(lat_lon,
                       by = "description")

    return(x_learn_out)

  } else {
    print0("learner should be either 's', 't' or 'x'")
  }
}
