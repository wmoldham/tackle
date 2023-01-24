# Assay-class.R

#' @rdname Assay-class
setClass(
  "Assay",
  slots = c(
    raw = "data.frame",
    model = "function",
    fit = "ANY"
  )
  # prototype = list(
  #   raw = data.frame(),
  #   model =
  # )
)

Assay <- function(df, model = \(x) stats::lm(conc ~ value, data = x)) {
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

    methods::validObject(.Object)
    .Object
  }
)

methods::setValidity(
  "Assay",
  function(object) {
    msg <- NULL

    raw <- object@raw
    fit <- object@fit

    # data frame
    if ("conc" %nin% names(raw)) {
      msg <- c(msg, "Data must contain a column named 'conc'")
    }
    if ("value" %nin% names(raw)) {
      msg <- c(msg, "Data must contain a column named 'value'")
    }

    # fit
    if (length(intersect(class(fit), c("lm", "rlm"))) == 0) {
      msg <- c(msg, "Model must use: stats::lm, MASS::rlm")
    }

    if (is.null(msg)) TRUE else msg
  }
)
