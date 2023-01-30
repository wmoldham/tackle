# metabolomics.R

#' Impute missing values using random forest
#'
#' This function will impute missing values in a SummarizedExperiment assay slot
#' using random forest. The seed is set inside the function so that it will return
#' the same replacement values each time it is called.
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A SummarizedExperiment object without missing assay values.
#' @export
#'
#' @examples
#' se <- impute_rf(se)
#'
impute_rf <- function(se) {
  set.seed(42)
  SummarizedExperiment::assay(se) <-
    missForest::missForest(
      t(SummarizedExperiment::assay(se)),
      maxiter = 10
    )$ximp |>
    t()
  se
}


#' Normalize using probabilistic quotient normalization
#'
#' This function will perform a probabilistic quotient normalization on the assay
#' values in a SummarizedExperiment object. If the `rowData` contains a column
#' named `reference`, these values will be used for normalization, otherwise, the
#' mean feature value across all samples will be used.
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A SummarizedExperiment object with normalized assay values.
#' @export
#'
#' @examples
#' se <- norm_pqn(se)
#'
norm_pqn <- function(se) {
  mat <- SummarizedExperiment::assay(se)

  has_reference <-
    SummarizedExperiment::rowData(se) |>
    names() |>
    {\(x) "reference" %in% x}()

  if (has_reference) {
    quotients <- mat / SummarizedExperiment::rowData(se)$reference
  } else {
    reference <- apply(mat, 1, mean)
    quotients <- mat / reference
  }
  quotient_medians <- apply(quotients, 2, stats::median)
  SummarizedExperiment::assay(se) <- t(t(mat) / quotient_medians)
  se
}
