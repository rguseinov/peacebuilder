#' Check year range
#'
#' @param start_year First year.
#' @param end_year Last year.
#'
#' @keywords internal
check_year_range <- function(start_year, end_year) {

  if (!is.numeric(start_year) || length(start_year) != 1 || is.na(start_year)) {
    stop("`start_year` must be a single numeric value.", call. = FALSE)
  }

  if (!is.numeric(end_year) || length(end_year) != 1 || is.na(end_year)) {
    stop("`end_year` must be a single numeric value.", call. = FALSE)
  }

  if (start_year > end_year) {
    stop("`start_year` must be less than or equal to `end_year`.", call. = FALSE)
  }

  invisible(TRUE)
}


#' Check that data are uniquely identified by key columns
#'
#' @param data A data frame.
#' @param keys Character vector of key column names.
#'
#' @keywords internal
check_unique_key <- function(data, keys) {

  missing_keys <- setdiff(keys, names(data))

  if (length(missing_keys) > 0) {
    stop(
      "Missing key columns: ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }

  dupes <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::filter(.data$n > 1)

  if (nrow(dupes) > 0) {
    stop(
      "Data are not uniquely identified by: ",
      paste(keys, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

#' Load an RData file from package extdata
#'
#' @param filename Character. Name of the `.RData` file located in
#'   `inst/extdata`.
#'
#' @return The single object stored in the `.RData` file.
#'
#' @keywords internal
read_rdata_from_extdata <- function(filename) {
  path <- system.file(
    "extdata",
    filename,
    package = "peacebuilder"
  )
  if (path == "") {
    stop(
      "Could not find `", filename, "` in package extdata.",
      call. = FALSE
    )
  }
  env <- new.env(parent = emptyenv())
  obj_names <- load(path, envir = env)
  if (length(obj_names) != 1) {
    stop(
      "`", filename, "` must contain exactly one object, but contains: ",
      paste(obj_names, collapse = ", "),
      call. = FALSE
    )
  }
  env[[obj_names]]
}
