#' List unique values of `data.table` object columns
#'
#' Compute a named list whose element `i` corresponds to the unique values of
#' the data.table `DT` column `z[i]`. If `exclude_na` is `NULL` or `FALSE`,
#' missing values are included in the list of unique values. If `exclude_na =
#' TRUE`, missing values are excluded for all columns in `z`. If `exclude_na` is
#' a subset of `z`, the missing values of those columns will be excluded.
#'
#'
#' @param DT the data.table object
#' @param z the column names whose unique values are to be computed
#' @param exclude_na either a logical value or a subset of `z` values
#'
#' @import data.table
.lunique <- function(DT, z, exclude_na = NULL) {

  exclude_na <- .excludena_algebra(z, exclude_na)

  l <- lapply(z, function(zn) {
    u <- DT[, unique(get(zn))]
    if (zn %in% exclude_na) u <- na.exclude(u)

    return(u)
  })

  names(l) <- z

  return(l)
}

.excludena_algebra <- function(vlist, vexclude) {
  if (is.null(vexclude) || vexclude == FALSE) return(NULL)
  if (vexclude == TRUE) return(vlist)

  return(intersect(vlist, vexclude))

}


#' Given a list of IDates, compute associated ages.
#'
#' @param vdate0 `IDate` vector
#' @param date1 `IDate` vector of length one, or of the same length as `vdate0`
#'
#' @export
#' @import data.table
age_calc <- function(vdate0, date1 = as.Date(Sys.time())) {
  y1 <- data.table::year(date1)
  m1 <- data.table::month(date1)
  d1 <- data.table::mday(date1)

  y0 <- data.table::year(vdate0)
  m0 <- data.table::month(vdate0)
  d0 <- data.table::mday(vdate0)

  idx0 <- m0 > m1
  idx1 <- m1 == m0
  idx2 <- d0 > d1


  ret0 <- y1 - y0

  ret0[!is.na(idx0) & idx0] <- ret0[!is.na(idx0) & idx0] - 1

  ret0[!is.na(idx1) & !is.na(idx2) & idx1 & idx2] <-
    ret0[!is.na(idx1) & !is.na(idx2) & idx1 & idx2] - 1

  return(ret0)
}
