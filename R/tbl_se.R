# tbl_se.R

#' Convert between SummarizedExperiments and Tibbles
#'
#' @description
#' A `tbl_se` identifies the assay, feature, and sample data required to convert
#' a Tibble into a SummarizedExperiment.
#'
#' @param tbl A tibble.
#' @param assay The column name of the assay data.
#' @param features The column name of the feature data or IDs.
#' @param feature_data The column name(s) of additional feature information.
#' @param samples The column name of the sample data or IDs.
#' @param sample_data The column name(s) of additional sample information.
#'
#' @export
#'
#' @rdname tbl_se
#' @aliases as_tibble.SummarizedExperiment as_SummarizedExperiment.tbl_se tbl_se
#'
tbl_se <- function(
    tbl,
    assay,
    features,
    feature_data = NULL,
    samples,
    sample_data = NULL
) {
  new_tbl_se(
    tbl = tbl,
    assay = assay,
    features = features,
    feature_data = feature_data,
    samples = samples,
    sample_data = sample_data
  )
}

new_tbl_se <- function(
    tbl,
    assay,
    features,
    feature_data = NULL,
    samples,
    sample_data = NULL
) {
  structure(
    tbl,
    class = c("tbl_se", class(tbl)),
    assay = assay,
    features = features,
    feature_data = feature_data,
    samples = samples,
    sample_data = sample_data
  )
}

#' @description
#' `as_tibble` will convert a SummarizedExperiment into a `tbl_se`.
#'
#' @param x A `SummarizedExperiment` object.
#' @param assay_name The name for the column of assay data (*e.g.*, "counts").
#'
#' @export
#'
#' @rdname tbl_se
#'
#' @examples
#' x <- tibble::as_tibble(se, assay_name = "counts")
#'
as_tibble.SummarizedExperiment <- function(x, assay_name) {
  assay <-
    SummarizedExperiment::assay(x) |>
    tibble::as_tibble(rownames = "feature") |>
    tidyr::pivot_longer(
      -.data$feature,
      names_to = "sample",
      values_to = assay_name
    )

  feature_data <-
    SummarizedExperiment::rowData(x) |>
    tibble::as_tibble(rownames = "feature")

  sample_data <-
    SummarizedExperiment::colData(x) |>
    tibble::as_tibble(rownames = "sample")

  assay |>
    dplyr::left_join(feature_data, by = "feature") |>
    dplyr::left_join(sample_data, by = "sample") |>
    new_tbl_se(
      assay = assay_name,
      features = "feature",
      feature_data = setdiff(names(feature_data), "feature"),
      samples = "sample",
      sample_data = setdiff(names(sample_data), "sample")
    )
}

# #' @rdname tbl_se
# #' @export
# setMethod(
#   "as_tibble",
#   signature = c(x = "SummarizedExperiment"),
#   as_tibble.SummarizedExperiment
# )

#' @export
#' @rdname tbl_se
as_SummarizedExperiment <- function(tbl) {
  UseMethod("as_SummarizedExperiment")
}

#' @description
#' `as_SummarizedExperiment` will convert a `tbl_se` into a SummarizedExperiment.
#' Since a SummarizedExperiment requires one row for each feature, sample-specific
#' differences in features (*i.e.*, retention times) are summarized as the mean
#' and range across all samples.
#'
#' @param tbl A `tbl_se` object generated from `tbl_se` or `as_tibble`.
#'
#' @export
#'
#' @rdname tbl_se
#'
#' @examples
#' as_SummarizedExperiment(x)
#'
as_SummarizedExperiment.tbl_se <- function(tbl) {
  assay_data <-
    tbl |>
    dplyr::select(
      attr(tbl, "features"),
      attr(tbl, "samples"),
      attr(tbl, "assay")
    ) |>
    tidyr::pivot_wider(
      names_from = attr(tbl, "samples"),
      values_from = attr(tbl, "assay")
    ) |>
    tibble::column_to_rownames(attr(tbl, "features"))

  mean_rng <- list(
    mean = \(x) mean(x, na.rm = TRUE),
    min = \(x) min(x, na.rm = TRUE),
    max = \(x) max(x, na.rm = TRUE)
  )

  feature_data <-
    tbl |>
    dplyr::select(
      attr(tbl, "features"),
      attr(tbl, "feature_data"),
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(dplyr::across(attr(tbl, "features"))) |>
    dplyr::summarise(
      dplyr::across(tidyselect::where(is.character), unique),
      dplyr::across(tidyselect::where(is.numeric), mean_rng)
    ) |>
    tibble::column_to_rownames(attr(tbl, "features")) |>
    {\(x) x[match(rownames(assay_data), rownames(x)), , drop = FALSE]}()

  sample_data <-
    tbl |>
    dplyr::select(
      attr(tbl, "samples"),
      attr(tbl, "sample_data")
    ) |>
    dplyr::distinct() |>
    tibble::column_to_rownames(attr(tbl, "samples")) |>
    {\(x) x[match(colnames(assay_data), rownames(x)), , drop = FALSE]}()

  out <-
    SummarizedExperiment::SummarizedExperiment(
      assays = assay_data,
      rowData = feature_data,
      colData = sample_data
    )
  SummarizedExperiment::assayNames(out) <- attr(tbl, "assay")
  out
}
