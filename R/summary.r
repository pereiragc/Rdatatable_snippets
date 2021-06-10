#' Weighted summary  by category (internal function)
.wsummby <- function(DT, .w, .x, bycols, bycols_excludena = NULL, f, ...) {
  u <- data.table::as.data.table(
                     do.call(expand.grid,
                             .lunique(DT, bycols, bycols_excludena))
                   )

  dtsumm <- DT[u, on = bycols][, {
    l <- lapply(.x, function(xn) .SD[, f(get(xn), get(.w), ...)])
    names(l) <- .x
    l
  }, bycols]
}

#' Weighted summary by category
#'
#' This is a wrapper around `data.table` aggregation by category that can be
#' reused in different contexts with user supplied column names, and specific
#' behavior about NA categories.
#'
#'
#' @param DT the `data.table` data set
#' @param .w lenth 1 character vector indicating weight column
#' @param .x the name of the column to be summarized
#' @param bycols the name of the columns by which we are computing the summaries
#' @param bycols_excludena one of `NULL`, `TRUE`, `FALSE` or a character vector
#'   specifying which of `bycols` should have NAs ignored
#'
#' @return a `data.table` with the aggregation
#' @import data.table
#' @export
summby <- function(DT, .x, bycols, f, bycols_excludena = NULL, ..., .w = NULL) {
  if (".x" %in% .x) stop("[summ] Illegal column name: please replace `.x` with some other name")
  if (".w" %in% .w) stop("[summ] Illegal column name: please replace `.w` with some other name")

  fn <- f
  if (is.null(.w)) {
    .w <- 1
    fn <- function(x, y) f(x)
  }

  return(.wsummby(DT, .w, .x, bycols, bycols_excludena, fn))
}


countby <- function(DT, .x, .x_excludena = FALSE, bycols,
                    bycols_excludena = FALSE, .w = NULL, side_effects = TRUE, na.rm = TRUE) {
  DTc <- DT
  if (!side_effects) DTc <- copy(DT)

  DTc[ ,  .one := 1]


  if (is.null(.w)) {
    f <- function(x) sum(x, na.rm = na.rm)
  } else {
    f <- function(x, w) sum(x*w, na.rm = na.rm)
  }


  lsumm <- lapply(.x, function(xn) {
    excludena_agg <- c(.excludena_algebra(xn, .x_excludena),
                       .excludena_algebra(bycols, bycols_excludena))

    dtsumm <- summby(DTc, .x = ".one", bycols = c(xn, bycols),
           f = f,  # CHANGE!
           bycols_excludena = excludena_agg,
           .w = .w)
    data.table::setnames(dtsumm, ".one", "count")
    return(dtsumm)
  })
  names(lsumm) <- .x

  DTc[, .one := NULL][]


  return(lsumm)
}
