#' Calculate Precision at k.
#'
#' Calculates Precision at k using a true individual treatment effect and a
#' predicted treatment effect / uplift score.
#'
#' Precision at k is computed as:
#'
#'   | top k by predicted score intersect top k by true treatment effect | / k
#'
#' where k is defined as ceiling(n * k_frac).
#'
#' @param data A data frame containing the true and predicted treatment effects.
#' @param truth The column containing the true individual treatment effect, tau.
#' @param estimate The column containing the predicted treatment effect / uplift score.
#' @param k_frac Numeric. The fraction of observations to include in the top-k set.
#'   Defaults to 0.20.
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
#' top_k(df, truth = tau, estimate = tau_hat, k_frac = 0.20)
top_k <- function(data, truth, estimate, k_frac = 0.20, na_rm = TRUE) {

  truth <- rlang::eval_tidy(rlang::enquo(truth), data)
  estimate <- rlang::eval_tidy(rlang::enquo(estimate), data)

  precision_value <- top_k_vec(
    truth = truth,
    estimate = estimate,
    k_frac = k_frac,
    na_rm = na_rm
  )

  tibble::tibble(
    .metric = "top_k",
    .estimator = "standard",
    .estimate = precision_value
  )
}


#' Calculate Precision at k from vectors.
#'
#' Vector method for calculating Precision at k.
#'
#' @param truth A numeric vector containing the true individual treatment effect, tau.
#' @param estimate A numeric vector containing the predicted treatment effect / uplift score.
#' @param k_frac Numeric. The fraction of observations to include in the top-k set.
#'   Defaults to 0.20.
#' @param na_rm Logical. Should missing values be removed? Defaults to TRUE.
#'
#' @return A numeric value.
#'
#' @export
top_k_vec <- function(truth, estimate, k_frac = 0.20, na_rm = TRUE) {

  if (length(truth) != length(estimate)) {
    stop("`truth` and `estimate` must have the same length.")
  }

  if (length(k_frac) != 1 || !is.numeric(k_frac) || is.na(k_frac)) {
    stop("`k_frac` must be a single non-missing numeric value.")
  }

  if (k_frac <= 0 || k_frac > 1) {
    stop("`k_frac` must be greater than 0 and less than or equal to 1.")
  }

  df <- tibble::tibble(
    truth = truth,
    estimate = estimate
  )

  if (na_rm) {
    df <- df[stats::complete.cases(df), , drop = FALSE]
  } else {
    if (anyNA(df)) {
      return(NA_real_)
    }
  }

  n <- nrow(df)

  if (n == 0) {
    return(NA_real_)
  }

  n_top <- ceiling(n * k_frac)

  true_top <- order(df$truth, decreasing = TRUE)[seq_len(n_top)]
  pred_top <- order(df$estimate, decreasing = TRUE)[seq_len(n_top)]

  length(intersect(true_top, pred_top)) / n_top
}
