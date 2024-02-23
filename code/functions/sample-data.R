#' Sample training data.
#' @param df_assigned The data which has been assigned treatments.
#' @param prop_not_treated Sample imbalance - the proportion of the data that will be assigned 0.
#' @param n_train The number of plots in the train dataset (sample size).
#' @return df_train
#' @import dplyr
#' @export

sample_data <- function(df_assigned, prop_not_treated, n_train) {

  # sample train
  df_train_0 <- df_assigned |>
    dplyr::filter(sampling_location == "other") |>
    dplyr::filter(tr == 0) |>
    dplyr::slice_sample(n = as.integer(n_train*prop_not_treated))

  df_train_1 <- df_assigned |>
    dplyr::filter(sampling_location == "other") |>
    dplyr::filter(tr == 1) |>
    dplyr::slice_sample(n = n_train - as.integer(n_train*prop_not_treated))

  df_train <- dplyr::bind_rows(df_train_0, df_train_1)

  return(df_train)

}
