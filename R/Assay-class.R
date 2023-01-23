# Assay-class.R

#' @rdname Assay-class
setClass(
  "Assay",
  slots = c(
    raw = "list"
  ),
  prototype = list(
    raw = data.frame()
  )
)

Assay <- function(df) {
  methods::new(
    "Assay",
    raw = df
  )
}

setMethod(
  "initialize",
  "Assay",
  function(.Object, raw) {
    .Object@raw <- raw

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
