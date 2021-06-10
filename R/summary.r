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


#' Tabulation utility for data table (internal)
#'
#' This function provides a count of columns in `.x` by groups defined by
#' columns `bycols`. The count function is specified as .f, and it is supposed
#' to act on vectors of ones for each unique value of `.x[i]` in groups.
#'
#' If weights are provided, `.f` should accept two arguments, one of which is a
#' vector of ones, and the other is a vector of weights.
#'
#' @param .x character vector containing columns to be tabulated
#' @param .x_excludena one of NULL, a boolean, or a character vector -- see
#'   description
#' @param bycols a character vector containing columns by which to group data
#' @param bycols_excludena one of NULL, a boolean, or a character vector -- see
#'   description
#' @param .w a character vector specifying the weight column
#' @param .f the counting function acting on either a vector of ones when
#'   weights are not supplied, or acting on a vector of ones and weights when
#'   weights are supplied.
#' @param side_effects whether we should alter `DT` in-place (`side_effects =
#'   TRUE`) or make a copy (`side_effects = FALSE`)
#'
#' @return a named list where entries correspond to variables in `.x`,
#'   containing their tabulation
#'
#' @import data.table
#'
.wcountby <- function(DT, .x, bycols,
                      .x_excludena, bycols_excludena,
                      .w, .f, side_effects) {
  DTc <- DT
  if (!side_effects) DTc <- data.table::copy(DT)

  DTc[ ,  .one := 1]

  lsumm <- lapply(.x, function(xn) {
    excludena_agg <- c(.excludena_algebra(xn, .x_excludena),
                       .excludena_algebra(bycols, bycols_excludena))

    dtsumm <- summby(DTc, .x = ".one", bycols = c(xn, bycols),
           f = .f,
           bycols_excludena = excludena_agg,
           .w = .w)
    data.table::setnames(dtsumm, ".one", "count")
    return(dtsumm)
  })
  names(lsumm) <- .x

  DTc[, .one := NULL][]


  return(lsumm)
}

#' Tabulation utility for data.table
#'
#' Tabulate columns specified in `.x` of dataset `DT` (of class `data.table`),
#' groupped by columns specified in `bycols`. Allows specifying how missing
#' values should be handled both in grouping and the target variables.
#'
#' Allows simple weights, specified by `.w`. When weights are provided, the
#' element count is the sum of weights in the group for a particular value.
#'
#' @import data.table
#' @export
countby <-  function(DT, .x, bycols,
                     .x_excludena = FALSE, bycols_excludena = FALSE,
                     wide = TRUE,
                     .w = NULL, side_effects = FALSE) {
  .f <- if (is.null(.w)) {
         function(x) sum(x, na.rm = TRUE)
       } else {
         function(x, w) sum(x * w, na.rm = TRUE)
       }

  lcounts <- .wcountby(DT, .x, bycols, .x_excludena, bycols_excludena, .w,
                   .f, side_effects)

  if (wide) {
    lcounts <- lapply(seq_along(lcounts), function(ii) {
      vname <- names(lcounts)[ii]
      form <- paste(
        paste(bycols, collapse = " + "),
        .x[ii],
        sep = "~"
      )

      DT <- data.table::dcast(lcounts[[ii]], formula = form,
                              value.var = "count")

      old_val_names <- setdiff(colnames(DT), bycols)
      new_val_names <- paste(.x[ii], old_val_names, sep = " = ")

      data.table::setnames(DT, old_val_names, new_val_names)

    })

    names(lcounts) <- .x
  }

  return(lcounts)
}
