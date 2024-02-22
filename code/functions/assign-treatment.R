#' Assign treatment group to individual plot.
#' @param df The clean data.
#' @param assignment One of "random", "correlated_region" or "correlated_altitude".
#' @return The data with an extra column "tr" indicating treatment assignment.
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @export

assign_treatment <- function(df, assignment) {

  if (assignment == "random") {

    df |>
      dplyr::filter(in_square == FALSE) |>
      dplyr::select(description) |>
      dplyr::distinct() |>
      dplyr::slice_sample(prop = 0.5) -> no_treat_ids_rand

    df |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_rand$description ~ 0,
                        in_square == TRUE ~ NA,
                         .default = 1)
                    ) -> data_assigned_rand

    return(data_assigned_rand)

  } else if (assignment == "correlated_region") {

    df |>
      dplyr::filter(in_square == FALSE) |>
      dplyr::select(description, region) |>
      dplyr::distinct() |>
      dplyr::mutate(sample_weight = case_when(region == 1 ~ 0.10,
                                       region == 21 ~ 0.10,
                                       region == 22 ~ 0.20,
                                       region == 3 ~ 0.30,
                                       region == 4 ~ 0.40,
                                       region == 5 ~ 0.40 )) -> id_region_ord

    dplyr::slice_sample(id_region_ord,
                 prop = 0.5,
                 weight_by = sample_weight) -> no_treat_ids_block_ord

    df |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_block_ord$description ~ 0,
                        in_square == TRUE ~ NA,
                        .default = 1)
      ) -> data_assigned_block_ord

    return(data_assigned_block_ord)

  } else if (assignment == "correlated_altitude") {

    df |>
      dplyr::filter(in_square == FALSE) |>
      dplyr::select(description, altitude) |>
      dplyr::distinct() |>
      dplyr::slice_sample(prop = 0.5,
                   weight_by = altitude) |>
      dplyr::select(description) -> no_treat_ids_corr

    df |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_corr$description ~ 0,
                        in_square == TRUE ~ NA,
                         .default = 1)
                    ) -> data_assigned_corr

    return(data_assigned_corr)

  } else {
    print("assignment should be either 'random', 'correlated_region' or 'correlated_altitude'")
  }
}
