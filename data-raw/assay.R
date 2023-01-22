# assay.R

set.seed(42)
id <- 1:9
sample <- c(rep(NA, 3), rep(c("A", "B"), each = 3))
conc <- c(0, 5, 10, rep(NA, 6))
value <- c(100, 605, 1091, sample(100:1100, 6))
assay <- data.frame(id = id, sample = sample, conc = conc, value = value)

usethis::use_data(assay, overwrite = TRUE)
