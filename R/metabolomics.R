# metabolomics.R

#' Filter missing metabolites
#'
#' Remove features with missingness greater than a defined fraction (0.2 is the
#' default). The `group` argument may be used to define experimental groups, in
#' which case, features will only be removed if all groups individually have
#' missingness greater than the defined fraction. If no experimental groups are
#' defined, then features are removed based on the missingness across all samples.
#'
#' @param x A tibble formatted for the `as_SummarizedExperiment` method. The column
#'     containing assay data should be `a.NAME`. The columns containing feature and
#'     sample IDs should be `f.id` and `s.id`, respectively. The remaining columns
#'     should be tagged as either feature ("`f.NAME`") or sample ("`s.NAME`") values.
#'     This format is returned by the `as_tibble` method.
#' @param group A character vector of column names defining experiment groups. The
#'     default value will remove features where > 20% values are missing.
#' @param frac The maximum fraction of missing values tolerated for a feature.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' # add missing values to feature 1
#' assay <- SummarizedExperiment::assay(se)
#' assay[(1 + 200 * c(0, 2, 3))] <- NA
#' SummarizedExperiment::assay(se) <- assay
#' assay
#'
#' # remove based on missingness across all samples
#' tibble::as_tibble(se) |> rm_missing(frac = 0.4) # f.id == 1 removed
#'
#' # remove based on missingness within groups
#' tibble::as_tibble(se) |> rm_missing(group = "s.treatment", frac = 0.4) # f.id == 1 preserved
#'
rm_missing <- function(x, group = NULL, frac = 0.2) {
  assay_nm <- rlang::sym(grep("a\\.", names(x), value = TRUE))
  x |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$f.id, dplyr::across(dplyr::all_of(group))) |>
    dplyr::mutate(missing = sum(is.na(!!assay_nm)) / dplyr::n()) |>
    dplyr::group_by(.data$f.id) |>
    dplyr::filter(!all(missing >= frac)) |>
    dplyr::select(-"missing") |>
    dplyr::ungroup()
}

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
