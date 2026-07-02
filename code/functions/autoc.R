#' Calculate the AUTOC coefficient.
#'
#' Calculates the AUTOC coefficient using a true individual treatment effect
#' and a predicted treatment effect / uplift score.
#'
#' AUTOC is calculated as the signed area under the TOC curve, where:
#'
#' TOC(q) = mean tau among the top q fraction of observations - mean tau overall
#'
#' Observations are ranked from highest to lowest predicted treatment effect.
#' This version is intended for simulated data where the true individual
#' treatment effect is known.
#'
#' @param data A data frame containing the true and predicted treatment effects.
#' @param truth The column containing the true individual treatment effect, tau.
#' @param estimate The column containing the predicted treatment effect / uplift score.
#' @param na_rm Logical. Should rows with missing values be removed? Defaults to TRUE.
#'
#' @return A tibble with columns .metric, .estimator, and .estimate.
#'
#' @import dplyr
#' @import tibble
#' @import rlang
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   tau = rnorm(100),
#'   tau_hat = tau + rnorm(100, sd = 0.5)
#' )
#'
#' autoc(df, truth = tau, estimate = tau_hat)
autoc <- function(data, truth, estimate, na_rm = TRUE) {

  truth <- rlang::eval_tidy(rlang::enquo(truth), data)
  estimate <- rlang::eval_tidy(rlang::enquo(estimate), data)

  autoc_value <- autoc_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm
  )

  result <- tibble::tibble(
    .metric = "autoc",
    .estimator = "standard",
    .estimate = autoc_value
  )

  return(result)

}


#' Calculate the AUTOC coefficient from vectors.
#'
#' Vector method for calculating the AUTOC coefficient.
#'
#' @param truth A numeric vector containing the true individual treatment effect, tau.
#' @param estimate A numeric vector containing the predicted treatment effect / uplift score.
#' @param na_rm Logical. Should missing values be removed? Defaults to TRUE.
#'
#' @return A numeric value.
#'
#' @export
autoc_vec <- function(truth, estimate, na_rm = TRUE) {

  if (length(truth) != length(estimate)) {
    stop("`truth` and `estimate` must have the same length.")
  }

  df <- tibble::tibble(
    truth = truth,
    estimate = estimate
  )

  if (na_rm) {

    df <- df |>
      dplyr::filter(stats::complete.cases(df))

  } else {

    if (anyNA(df)) {
      return(NA_real_)
    }

  }

  n <- nrow(df)

  if (n == 0) {
    return(NA_real_)
  }

  df <- df |>
    dplyr::arrange(dplyr::desc(estimate)) |>
    dplyr::mutate(
      q = dplyr::row_number() / n,
      toc = cumsum(truth) / dplyr::row_number() - mean(truth)
    )

  x <- c(0, df$q)

  # Match your compute_autoc() convention:
  # limit as q -> 0 is approximated using the top-ranked unit.
  y <- c(df$toc[1], df$toc)

  autoc_value <- sum(
    diff(x) * (head(y, -1) + tail(y, -1)) / 2
  )

  return(autoc_value)

}
