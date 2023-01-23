# Assay-class.R

#' @rdname Assay-class
setClass(
  "Assay",
  slots = c(
    raw = "list",
    model = "function"
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

    methods::validObject(.Object)
    .Object
  }
)

methods::setValidity(
  "Assay",
  function(object) {
    msg <- NULL

    raw <- object@raw

    # data frame
    if ("conc" %nin% names(raw)) {
      msg <- c(msg, "Data must contain a column named 'conc'")
    }
    if ("value" %nin% names(raw)) {
      msg <- c(msg, "Data must contain a column named 'value'")
    }

    if (is.null(msg)) TRUE else msg
  }
)
