# Assay-class.R

#' @rdname Assay-class
setClass(
  "Assay",
  slots = c(
    raw = "data.frame",
    model = "function",
    fit = "lm",
    quality = "numeric",
    clean = "data.frame"
  )
)

Assay <- function(df, model = \(x) stats::lm(value ~ conc, data = x)) {
  methods::new(
    "Assay",
    raw = df,
    model = model
  )
}

setMethod(
  "initialize",
  "Assay",
  function(.Object, raw, model) {
    .Object@raw <- raw
    .Object@model <- model
    .Object@fit <- model(raw)
    .Object@quality <- summary(.Object@fit)$r.squared
    .Object@clean <- interpolate(.Object@raw, .Object@fit)

    methods::validObject(.Object)
    .Object
  }
)

methods::setValidity(
  "Assay",
  function(object) {
    msg <- NULL

    if (is.null(msg)) TRUE else msg
  }
)

interpolate <- function(raw, fit) {
  standards <- raw[!is.na(raw$conc), ]
  samples <- raw[is.na(raw$conc), ]
  x <- stats::model.frame(fit)[[deparse(fit$terms[[3]])]]
  p <- polynom::polynomial(stats::coefficients(fit))
  new_y <- as.list(samples[[deparse(fit$terms[[2]])]])
  new_x <- unlist(
    lapply(new_y, \(y) {
      roots <- solve(p, y)
      roots <- round(roots, digits = 8)
      root <- roots[which(Im(roots) == 0 & Re(roots) >= 0 & Re(roots) <= 1.25 * max(x))]
      ifelse(identical(root, numeric(0)), NA, Re(root))
    })
  )
  samples$conc <- new_x
  rbind(standards, samples)
}
