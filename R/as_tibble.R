# as_tibble.R

#' Convert between SummarizedExperiments and Tibbles
#'
#' @description
#' `as_tibble` will convert a SummarizedExperiment into a `tbl_df`. To keep track
#' of feature, sample, and assay data, the column names are annotated.
#'
#' @param se A `SummarizedExperiment` object.
#' @param a.name The name for the column of assay data (*e.g.*, "counts").
#'
#' @return A `tbl` containing one row for each combination of sample and feature.
#'
#' @export
#'
#' @rdname tbl_se
#'
#' @examples
#' tbl <- tibble::as_tibble(se, a.name = "counts")
#'
as_tibble.SummarizedExperiment <- function(se, a.name = "value") {
  assay_data <-
    SummarizedExperiment::assay(se) |>
    tibble::as_tibble(rownames = "f.id") |>
    tidyr::pivot_longer(
      -"f.id",
      names_to = "s.id",
      values_to = paste0("a.", a.name)
    )

  feature_data <-
    SummarizedExperiment::rowData(se) |>
    tibble::as_tibble(rownames = "id") |>
    dplyr::rename_with(\(x) paste0("f.", x))

  sample_data <-
    SummarizedExperiment::colData(se) |>
    tibble::as_tibble(rownames = "id") |>
    dplyr::rename_with(\(x) paste0("s.", x))

  assay_data |>
    dplyr::left_join(feature_data, by = "f.id") |>
    dplyr::left_join(sample_data, by = "s.id")
}
