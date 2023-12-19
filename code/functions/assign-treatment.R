#' Assign treatment group to individual plot.
#' @param df The clean data.
#' @param assignment One of "random", "blocked" or "correlated".
#' @param proportion_not_treated The proportion of the data that will be
#' assigned 0. Not applicable when assignment = "blocked".
#' @return The data with an extra column "tr" indicating treatment assignment.
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @export

assign_treatment <- function(df, assignment, proportion_not_treated = 0.5) {

  if (assignment == "random") {

    df |>
      dplyr::select(description) |>
      dplyr::distinct() |>
      dplyr::slice_sample(prop = proportion_not_treated) -> no_treat_ids_rand

    df |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_rand$description ~ 0,
                         .default = 1)
                    ) -> data_assigned_rand

    return(data_assigned_rand)

  } else if (assignment == "blocked") {

    df |>
      dplyr::select(description, region) |>
      dplyr::distinct() -> id_region

    tibble::tibble(region_code = c(1, 3:5, 21, 22),
           proportion = c(0.12, 0.085, 0.07, 0.07, 0.125, 0.125)) -> weights

    purrr::pmap(weights, slice_by_region, data = id_region) |>
      dplyr::bind_rows() |>
      dplyr::select(description) -> no_treat_ids_block

    df |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_block$description ~ 0,
                        .default = 1)
      ) -> data_assigned_block

    return(data_assigned_block)

  } else if (assignment == "correlated") {

    df |>
      dplyr::select(description, altitude) |>
      dplyr::distinct() |>
      dplyr::slice_sample(prop = proportion_not_treated,
                   weight_by = altitude) |>
      dplyr::select(description) -> no_treat_ids_corr

    df |>
      dplyr::mutate(tr =
                      dplyr::case_when(
                        description %in% no_treat_ids_corr$description ~ 0,
                         .default = 1)
                    ) -> data_assigned_corr

    return(data_assigned_corr)

  } else {
    print0("assignment should be either 'random', 'blocked' or 'correlated'")
  }
}

# helpers -----------------------------------------------------------------

slice_by_region <- function(region_code, proportion, data) {
  data |>
    dplyr::filter(region == region_code) |>
    dplyr::slice_sample(prop = proportion)
}

