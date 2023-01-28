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
