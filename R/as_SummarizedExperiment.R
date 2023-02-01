# as_SummarizedExperiment.R

#' @include as_tibble.R

#' @export
#' @rdname tbl_se
as_SummarizedExperiment <- function(tbl) {
  UseMethod("as_SummarizedExperiment")
}

#' @description
#' `as_SummarizedExperiment` will convert a `tbl` into a SummarizedExperiment based
#' on column name annotations. The column containing assay data should be `a.NAME`.
#' The columns containing feature and sample IDs should be `f.id` and `s.id`,
#' respectively. The remaining columns should be tagged as either feature ("`f.NAME`")
#' or sample ("`s.NAME`") values. This format is returned by the `as_tibble` method.
#' Since a SummarizedExperiment requires one row for each feature, sample-specific
#' differences in features (*e.g.*, retention times) are summarized as the mean
#' and range across all samples.
#'
#' @param tbl A `tbl_df` object with appropriately annotated column names.
#'
#' @return A `SummarizedExperiment`
#'
#' @export
#'
#' @rdname tbl_se
#'
#' @examples
#' as_SummarizedExperiment(tbl)
#'
as_SummarizedExperiment.tbl_df <- function(tbl) {
  tbl <- dplyr::ungroup(tbl)
  col_names <- names(tbl)
  feature_nms <- col_names[grep("f\\.", col_names)]
  sample_nms <- col_names[grep("s\\.", col_names)]
  assay_nm <- col_names[grep("a\\.", col_names)]

  if ("f.id" %nin% feature_nms) {
    rlang::abort(
      "The tibble must identify feature IDs with a column named 'f.id'",
      class = "bad_format"
    )
  }
  if ("s.id" %nin% sample_nms) {
    rlang::abort(
      "The tibble must identify sample IDs with a column named 's.id'",
      class = "bad_format"
    )
  }
  if (length(assay_nm) != 1) {
    rlang::abort(
      "The tibble must contain only one column tagged with 'a.NAME' for assay data.",
      class = "bad_format"
    )
  }

  assay_data <-
    tbl |>
    dplyr::select(
      "f.id",
      "s.id",
      tidyselect::all_of(assay_nm)
    ) |>
    tidyr::pivot_wider(
      names_from = "s.id",
      values_from = assay_nm
    ) |>
    tibble::column_to_rownames("f.id")

  mean_rng <- list(
    mean = \(x) mean(x, na.rm = TRUE),
    min = \(x) min(x, na.rm = TRUE),
    max = \(x) max(x, na.rm = TRUE)
  )

  feature_data <-
    tbl |>
    dplyr::select(tidyselect::all_of(feature_nms)) |>
    dplyr::distinct() |>
    dplyr::group_by(dplyr::across("f.id")) |>
    dplyr::summarise(
      dplyr::across(tidyselect::where(is.character), unique),
      dplyr::across(tidyselect::where(is.numeric), mean_rng)
    ) |>
    dplyr::rename_with(\(x) sub("f.", "", x), .cols = -c("f.id")) |>
    tibble::column_to_rownames("f.id") |>
    {\(x) x[match(rownames(assay_data), rownames(x)), , drop = FALSE]}()

  sample_data <-
    tbl |>
    dplyr::select(tidyselect::all_of(sample_nms)) |>
    dplyr::distinct() |>
    dplyr::rename_with(\(x) sub("s.", "", x), .cols = -c("s.id")) |>
    tibble::column_to_rownames("s.id") |>
    {\(x) x[match(colnames(assay_data), rownames(x)), , drop = FALSE]}()

  assay_nm <- sub("a.", "", assay_nm)
  out <-
    SummarizedExperiment::SummarizedExperiment(
    assays = assay_data,
    rowData = feature_data,
    colData = sample_data
  )
  SummarizedExperiment::assayNames(out) <- assay_nm
  out
}
