# se.R

set.seed(42)
nrows <- 200
ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowData <-
  S4Vectors::DataFrame(
    metabolite = do.call(paste0, replicate(5, sample(LETTERS, 200, TRUE), FALSE)),
    row.names = 1:200
  )
colData <-
  S4Vectors::DataFrame(
    treatment = rep(c("control", "treatment"), 3),
    row.names = LETTERS[1:6]
  )
se <-
  SummarizedExperiment::SummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    rowData = rowData,
    colData = colData
  )

usethis::use_data(se, overwrite = TRUE)
