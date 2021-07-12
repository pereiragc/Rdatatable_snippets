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



union_ids <- function(DT, c1, c2, name_join, maxiter=50) {
  if (name_join %in% colnames(DT)) name_join <- paste0(name_join, "_new")

  DT[!is.na(get(c1)), .c1 := .GRP, c1]
  DT[!is.na(get(c2)), .c2 := .GRP, c2]

  max1 <- DT[, max(.c1, na.rm = TRUE)]
  max2 <- DT[, max(.c2, na.rm = TRUE)]

  DT[is.na(.c1), .c1 := max1 + .I]
  DT[is.na(.c2), .c2 := max2 + .I]

  DT[, .join_sv := seq_len(.N)]
  DT[!is.na(.c1), .join := min(.join_sv), .c1]

  sep <- c(".c1" = ".c2",
           ".c2" = ".c1")

  i <- 1
  x0 <- ".c2"

  allsame <- DT[, all(.join == .join_sv)]

  while (!allsame && i <= maxiter) {
    DT[!is.na(get(x0)), .join := min(.join), x0]

    allsame <- DT[, all(.join_sv == .join, na.rm = TRUE)]

    DT[, .join_sv := .join][]

    x0 <- sep[x0]

    i <- i + 1
  }


  if (!allsame) {
    warning(glue::glue("[union_ids] Could not join IDs after {i} iterations; consider chaing `maxiter` parameter"))
  }

  DT[is.na(get(c1)) & is.na(get(c2)), .join := NA]

  DT[, c(".join_sv", ".c1", ".c2") := NULL]

  setnames(DT, ".join",  name_join)

  return(DT)
}
