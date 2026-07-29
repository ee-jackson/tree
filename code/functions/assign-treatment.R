#' Assign treatment group to individual plot.
#' @param df_clean The clean data.
#' @param assignment One of "random" or "non_random".
#' @param seed Optional random seed.
#' @return df_assigned
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export

assign_treatment <- function(df_clean, assignment = "stratified", seed = NULL) {

    if (!is.null(seed)) {
      set.seed(seed = seed)
    }

  features <- df_clean |>
    dplyr::filter(period == 0) |>
    dplyr::select(
      description, ost_wgs84, nord_wgs84,
      sampling_location, soil_moist_code,
      altitude, mat_5yr, map_5yr, ditch, no_of_stems,
      volume_pine, volume_spruce, volume_birch,
      volume_aspen, volume_oak, volume_beech,
      volume_southern_broadleaf, volume_contorta,
      volume_other_broadleaf, volume_larch, wet
    )

  # first assign test data

  # give "core" test data even treatment assignment
  sample_core <- df_clean |>
    dplyr::filter(sampling_location == "core") |>
    dplyr::select(description) |>
    dplyr::distinct()

  no_treat_ids_core <- sample_core |>
    dplyr::slice_sample(prop = 0.5)

  data_assigned_core <- df_clean |>
    dplyr::filter(description %in% sample_core$description) |>
    dplyr::mutate(tr =
                    dplyr::case_when(
                      description %in% no_treat_ids_core$description ~ 0,
                      .default = 1)
    )

  # give "edge" test data even treatment assignment
  sample_edge <- df_clean |>
    dplyr::filter(sampling_location == "edge") |>
    dplyr::select(description) |>
    dplyr::distinct()

  no_treat_ids_edge <- sample_edge |>
    dplyr::slice_sample(prop = 0.5)

  data_assigned_edge <- df_clean |>
    dplyr::filter(description %in% sample_edge$description) |>
    dplyr::mutate(tr =
                    dplyr::case_when(
                      description %in% no_treat_ids_edge$description ~ 0,
                      .default = 1)
    )

  # give "stratified" test data even treatment assignment
  sample_stratified <- df_clean |>
    dplyr::filter(sampling_location == "stratified") |>
    dplyr::select(description) |>
    dplyr::distinct()

  no_treat_ids_stratified <- sample_stratified |>
    dplyr::slice_sample(prop = 0.5)

  data_assigned_stratified <- df_clean |>
    dplyr::filter(description %in% sample_stratified$description) |>
    dplyr::mutate(tr =
                    dplyr::case_when(
                      description %in% no_treat_ids_stratified$description ~ 0,
                      .default = 1)
    )

  test_assigned <- dplyr::bind_rows(data_assigned_core,
                                    data_assigned_edge,
                                    data_assigned_stratified)

  # now assign training data

  if (assignment == "random") {

    no_treat_ids_rand <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::select(description) |>
      dplyr::distinct() |>
      dplyr::slice_sample(prop = 0.5)

    data_assigned_rand <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_rand$description ~ 0,
                         .default = 1)
                    ) |>
      dplyr::bind_rows(test_assigned)

    data_obs_rand <- data_assigned_rand |>
      dplyr::select(description, tr, control_category_name, total_soil_carbon) |>
      tidyr::pivot_wider(id_cols = c(description, tr),
                         names_from = control_category_name,
                         values_from = total_soil_carbon) |>
      dplyr::mutate(soil_carbon_obs =
                      dplyr::case_when(tr == 1 ~ `SetAside (Unmanaged)`,
                                       tr == 0 ~ `BAU - NoThinning`)) |>
      dplyr::rename(soil_carbon_initial = `Initial state`,
                    soil_carbon_1 = `SetAside (Unmanaged)`,
                    soil_carbon_0 = `BAU - NoThinning`) |>
      dplyr::left_join(features,
                       by = "description")

    return(data_obs_rand)

  } else if (assignment == "non_random") {

    # wet plots more likely to be in treat/set aside group
    treat_ids_corr <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::select(description, wet) |>
      dplyr::distinct() |>
      dplyr::mutate(wet = wet + 1) |>
      dplyr::slice_sample(prop = 0.5,
                          weight_by = wet) |>
      dplyr::select(description)

    data_assigned_wet <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% treat_ids_corr$description ~ 1,
                         .default = 0)
                    ) |>
      dplyr::bind_rows(test_assigned)

    data_obs_wet <- data_assigned_wet |>
      dplyr::select(description, tr, control_category_name, total_soil_carbon) |>
      tidyr::pivot_wider(id_cols = c(description, tr),
                         names_from = control_category_name,
                         values_from = total_soil_carbon) |>
      dplyr::mutate(soil_carbon_obs =
                      dplyr::case_when(tr == 1 ~ `SetAside (Unmanaged)`,
                                       tr == 0 ~ `BAU - NoThinning`)) |>
      dplyr::rename(soil_carbon_initial = `Initial state`,
                    soil_carbon_1 = `SetAside (Unmanaged)`,
                    soil_carbon_0 = `BAU - NoThinning`) |>
      dplyr::left_join(features,
                       by = "description")

    return(data_obs_wet)

  } else {
    print("assignment should be either 'random' or 'non_random'")
  }
}
