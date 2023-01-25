# Assay-class.R

#' @rdname Assay-class
setClass(
  "Assay",
  slots = c(
    raw = "data.frame",
    model = "function",
    fit = "lm",
    quality = "numeric",
    x_col = "character",
    y_col = "character",
    standards = "numeric",
    samples = "numeric",
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
    .Object@x_col <- deparse(.Object@fit$terms[[3]])
    .Object@y_col <- deparse(.Object@fit$terms[[2]])
    .Object@standards <- which(!is.na(raw[[.Object@x_col]]))
    .Object@samples <- which(is.na(raw[[.Object@x_col]]))
    .Object@clean <-
      interpolate(
        .Object@raw,
        .Object@fit,
        .Object@standards,
        .Object@samples,
        .Object@x_col,
        .Object@y_col
      )

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

interpolate <- function(raw, fit, standards, samples, x_col, y_col) {
  p <- polynom::polynomial(stats::coefficients(fit))
  new_y <- as.list(raw[samples, y_col])
  new_x <- unlist(
    lapply(new_y, \(y) {
      roots <- solve(p, y)
      roots <- round(roots, digits = 8)
      root <-
        roots[
          which(
            Im(roots) == 0 &
              Re(roots) >= 0 &
              Re(roots) <= 1.25 * max(raw[standards, x_col])
          )
        ]
      ifelse(identical(root, numeric(0)), NA, Re(root))
    })
  )
  raw[samples, x_col] <- new_x
  raw
}

setMethod(
  "show",
  "Assay",
  function(object) {
    standards <- length(object@standards)
    samples <- length(object@samples)
    cat(
      is(object)[[1]], ":\n",
      "- Standards = ", standards, "\n",
      "- Samples   = ", samples, "\n",
      "- Formula   = ", deparse(object@fit$call), "\n",
      "- R squared = ", object@quality, "\n\n",
      sep = ""
    )
    print(object@clean)
  }
)

setMethod(
  "plot",
  signature = c(x = "Assay"),
  function(x, samples = TRUE) {
    p <-
      ggplot2::ggplot(x@clean) +
      ggplot2::aes(
        x = !!rlang::sym(x@x_col),
        y = !!rlang::sym(x@y_col)
      ) +
      ggplot2::geom_function(
        fun = as.function(polynom::polynomial(stats::coefficients(x@fit))),
        color = "cornflowerblue",
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = x@clean[x@standards, ],
        shape = 16,
        size = 3
      ) +
      ggplot2::theme_minimal()

    if (samples) {
      p +
        ggplot2::geom_point(
          data = x@clean[x@samples, ],
          shape = 1,
          size = 3
        )
    } else {
      p
    }
  }
)

setGeneric("results", function(x, ...) methods::standardGeneric("results"))

setMethod(
  "results",
  signature = c(x = "Assay"),
  function(x, standards = FALSE) {
    if (!standards) {
      x@clean[x@samples, ]
    } else if (standards) {
      x@clean
    }
  }
)
