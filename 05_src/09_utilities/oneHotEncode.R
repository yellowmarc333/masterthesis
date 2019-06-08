#' One-hot-encoder / dichotomizer
#'
#' Converts into a \code{data.table} with \code{TRUE}/\code{FALSE} encoding.
#'
#' @author Philipp Hallmeier
#' @param    x atomic vector or \code{data.table}.
#' @param cols \code{character} or \code{numeric}. Columns to one-hot-encode.
#'             The default encodes all columns.
#' @param  sep \code{character(1)}. Separator of column and variable names
#'             (for \code{data.table}s). Default is \dQuote{_}.
#' @return     \code{data.table} with \code{TRUE}/\code{FALSE} encoding.
#' @examples
#' x <- iris$Species[48:52]
#' x[3] <- NA
#' oneHotEncode(x)
#'
#' dt <- data.table(A = x, B = rev(x))
#' oneHotEncode(dt, sep = "-")
#' oneHotEncode(dt, cols = 1)
#' @export
oneHotEncode <- function(x, cols = NULL, sep = "_"){
  assert(checkAtomicVector(x), checkDataTable(x, col.names = "named"))

  oneHotEncodeVector <- function(x){
    x <- as.factor(x)
    oldNaAction <- getOption("na.action")
    on.exit(options(na.action = oldNaAction))
    options(na.action = na.pass)
    modelMatrix <- model.matrix(~ X - 1, data.frame(X = x))
    colnames(modelMatrix) <- levels(x)
    storage.mode(modelMatrix) <- "logical"
    return(as.data.table(modelMatrix))
  }

  if (is.data.table(x)) {
    if (is.null(cols)) cols <- colnames(x)
    assert(checkCharacter(cols),
           checkIntegerish(cols, tol = .Machine$double.xmin))
    if (is.numeric(cols)) cols <- colnames(x)[cols]
    assertString(sep)

    return(do.call(cbind, lapply(colnames(x), function(name){
      if (name %in% cols) {
        encodedDT <- oneHotEncodeVector(x[[name]])
        setnames(encodedDT, paste(name, colnames(encodedDT), sep = sep))
        return(encodedDT)
      } else {
        return(x[, ..name])
      }
    })))
  } else {
    return(oneHotEncodeVector(x))
  }
}
