#' Normalize experimental means across batches
#'
#' This function will correct sample values by the difference between batch means
#' and the grand mean of the measurement value.
#'
#' @param .data A data frame
#' @param .value Column name of experimental values
#' @param ... Column name(s) of grouping variables
#'
#' @return A data frame
#' @export
#'
#' @examples
#' df <- assay[!is.na(assay$sample), ]
#' normalize(df, value, sample)
#'
normalize <- function(.data, .value, ...) {
  val <- rlang::ensym(.value)
  grand_mean <- mean(.data[[val]])
  new_name <- paste0(val, "_cor")

  .data |>
    dplyr::group_by(...) |>
    dplyr::mutate(
      exp_mean = mean({{ .value }}),
      cf = grand_mean - .data$exp_mean,
      value_cor = {{ .value }} + .data$cf,
      .after = {{ .value }}
    ) |>
    dplyr::rename(!!new_name := "value_cor") |>
    dplyr::select(-c("exp_mean", "cf"))
}
