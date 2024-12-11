#' Assign treatment group to individual plot.
#' @param df_clean The clean data.
#' @param assignment One of "random", "correlated_region" or "correlated_altitude".
#' @return df_assigned
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export

assign_treatment <- function(df_clean, assignment) {

  features <- df_clean |>
    dplyr::filter(period == 0) |>
    dplyr::select(
      description, sampling_location, soil_moist_code,
      altitude, mat_5yr, map_5yr, ditch, no_of_stems,
      volume_pine, volume_spruce, volume_birch,
      volume_aspen, volume_oak, volume_contorta,
      volume_southern_broadleaf, volume_larch,
      volume_other_broadleaf, volume_beech
    )

  # first assign test data for specified locations

  # give "centre" test data even treatment assignment
  sample_centre <- df_clean |>
    dplyr::filter(sampling_location == "centre") |>
    dplyr::select(description) |>
    dplyr::distinct() |>
    dplyr::slice_sample(n = 162)

  no_treat_ids_centre <- sample_centre |>
    dplyr::slice_sample(prop = 0.5)

  data_assigned_centre <- df_clean |>
    dplyr::filter(description %in% sample_centre$description) |>
    dplyr::mutate(tr =
                    dplyr::case_when(
                      description %in% no_treat_ids_centre$description ~ 0,
                      .default = 1)
    )

  # give "edge" test data even treatment assignment
  sample_edge <- df_clean |>
    dplyr::filter(sampling_location == "edge") |>
    dplyr::select(description) |>
    dplyr::distinct() |>
    dplyr::slice_sample(n = 162)

  no_treat_ids_edge <- sample_edge |>
    dplyr::slice_sample(prop = 0.5)

  data_assigned_edge <- df_clean |>
    dplyr::filter(description %in% sample_edge$description) |>
    dplyr::mutate(tr =
                    dplyr::case_when(
                      description %in% no_treat_ids_edge$description ~ 0,
                      .default = 1)
    )

  test_assigned <- dplyr::bind_rows(data_assigned_centre, data_assigned_edge)

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
                      dplyr::case_when(tr == 0 ~ `SetAside (Unmanaged)`,
                                       tr == 1 ~ `BAU - NoThinning`)) |>
      dplyr::rename(soil_carbon_initial = `Initial state`,
                    soil_carbon_0 = `SetAside (Unmanaged)`,
                    soil_carbon_1 = `BAU - NoThinning`) |>
      dplyr::left_join(features,
                       by = "description")

    return(data_obs_rand)

  } else if (assignment == "correlated_region") {

    id_region_ord <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::select(description, region) |>
      dplyr::distinct() |>
      dplyr::mutate(sample_weight = case_when(region == 1 ~ 0.40,
                                       region == 21 ~ 0.40,
                                       region == 22 ~ 0.30,
                                       region == 3 ~ 0.20,
                                       region == 4 ~ 0.10,
                                       region == 5 ~ 0.10 ))

    no_treat_ids_region <- dplyr::slice_sample(id_region_ord,
                 prop = 0.5,
                 weight_by = sample_weight)

    data_assigned_region <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_region$description ~ 0,
                        .default = 1)
      ) |>
      dplyr::bind_rows(test_assigned)

    data_obs_region <- data_assigned_region |>
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

    return(data_obs_region)

  } else if (assignment == "correlated_altitude") {

    no_treat_ids_corr <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::select(description, altitude) |>
      dplyr::distinct() |>
      dplyr::slice_sample(prop = 0.5,
                   weight_by = altitude) |>
      dplyr::select(description)

    data_assigned_altitude <- df_clean |>
      dplyr::filter(sampling_location == "other") |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_corr$description ~ 0,
                         .default = 1)
                    ) |>
      dplyr::bind_rows(test_assigned)

    data_obs_altitude <- data_assigned_altitude |>
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

    return(data_obs_altitude)

  } else {
    print("assignment should be either 'random', 'correlated_region' or 'correlated_altitude'")
  }
}
