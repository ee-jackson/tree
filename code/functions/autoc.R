#' Calculate the normalized AUTOC coefficient.
#'
#' Calculates the normalized AUTOC coefficient using a true individual
#' treatment effect and a predicted treatment effect / uplift score.
#'
#' Normalized AUTOC is computed as:
#'
#'   AUTOC(model) / AUTOC(perfect ranking)
#'
#' where perfect ranking means sorting observations by the true treatment
#' effect from highest to lowest.
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
#' autoc_norm(df, truth = tau, estimate = tau_hat)
autoc_norm <- function(data, truth, estimate, na_rm = TRUE) {

  truth <- rlang::eval_tidy(rlang::enquo(truth), data)
  estimate <- rlang::eval_tidy(rlang::enquo(estimate), data)

  autoc_value <- autoc_norm_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm
  )

  tibble::tibble(
    .metric = "autoc_norm",
    .estimator = "standard",
    .estimate = autoc_value
  )
}


#' Calculate the 1-normalized AUTOC coefficient from vectors.
#'
#' Vector method for calculating normalized AUTOC.
#'
#' @param truth A numeric vector containing the true individual treatment effect, tau.
#' @param estimate A numeric vector containing the predicted treatment effect / uplift score.
#' @param na_rm Logical. Should missing values be removed? Defaults to TRUE.
#'
#' @return A numeric value.
#'
#' @export
autoc_norm_vec <- function(truth, estimate, na_rm = TRUE) {

  if (length(truth) != length(estimate)) {
    stop("`truth` and `estimate` must have the same length.")
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

  compute_autoc_from_order <- function(tau_sorted) {
    toc <- cumsum(tau_sorted) / seq_along(tau_sorted) - mean(tau_sorted)
    q <- seq_along(tau_sorted) / length(tau_sorted)

    x <- c(0, q)
    y <- c(toc[1], toc)

    sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  }

  # Model AUTOC
  tau_sorted_model <- df$truth[order(df$estimate, decreasing = TRUE)]
  autoc_model <- compute_autoc_from_order(tau_sorted_model)

  # Perfect AUTOC: sort by the true treatment effect itself
  tau_sorted_perfect <- sort(df$truth, decreasing = TRUE)
  autoc_perfect <- compute_autoc_from_order(tau_sorted_perfect)

  if (isTRUE(all.equal(autoc_perfect, 0))) {
    return(NA_real_)
  }

  1 - (autoc_model / autoc_perfect)
}
