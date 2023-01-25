# se.R

nrows <- 200
ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
colData <-
  S4Vectors::DataFrame(
    Treatment = rep(c("ChIP", "Input"), 3),
    row.names = LETTERS[1:6]
  )
se <-
  SummarizedExperiment::SummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts),
    colData = colData
  )

usethis::use_data(se, overwrite = TRUE)
