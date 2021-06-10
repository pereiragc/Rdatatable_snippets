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
