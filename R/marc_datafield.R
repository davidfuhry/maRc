new_marcdatafield <- function(tag = numeric(), ind_1 = character(), ind_2 = character(), codes = c(), values = c()) {
  stopifnot(is.numeric(tag))
  stopifnot(is.character(ind_1))
  stopifnot(is.character(ind_2))
  stopifnot(is.vector(codes))
  stopifnot(is.vector(values))

  obj <- list(
    tag = tag,
    ind_1 = ind_1,
    ind_2 = ind_2,
    codes = codes,
    values = values
  )

  class(obj) <- "marcdatafield"
  obj
}

validate_marcdatafield <- function(obj) {
  if (identical(obj$tag, numeric())) {
    stop("Tag must be provided", call. = FALSE)
  }

  # We limit checking of tags to a general range instead of hard lists
  # as various sources use tags in non standard ways
  if (!(obj$tag > 0 & obj$tag < 999 & obj$tag %% 1 == 0)) {
    stop("Datafield tag must be whole number between 1 and 999", call. = FALSE)
  }

  if (!grepl("^[a-z0-9 ]{1}$", obj$ind_1)) {
    stop("Indicator 1 must be any lowercase ascii letter, number or a blank space", call. = FALSE)
  }

  if (!grepl("^[a-z0-9 ]{1}$", obj$ind_2)) {
    stop("Indicator 2 must be any lowercase ascii letter, number or a blank space", call. = FALSE)
  }

  if (length(obj$codes) != length(obj$values)) {
    stop("There need to be exactly as many subfield codes as values", call. = FALSE)
  }

  if (!all(grepl("^[A-z0-9]{1}$", obj$codes))) {
    stop("Each subfield code must be exactly one letter or number", call. = FALSE)
  }

  obj
}

marcdatafield <- function(tag = numeric(), ind_1 = character(), ind_2 = character(), codes = c(), values = c()) {
  tag = as.numeric(tag)
  ind_1 = as.character(ind_1)
  ind_2 = as.character(ind_2)
  codes = as.character(codes)
  values = as.character(values)
  validate_marcdatafield(new_marcdatafield(tag, ind_1, ind_2, codes, values))
}


#' Simplifie Marc Datafield to a Data.frame
#'
#' @param x Marcdatafield object to coerce into a data.frame
#' @param ... Additional arguments. Will be ignored.
#'
#' @return All values from the provided object in a data.frame, one row per code-value pair
#'
#' @export as.data.frame.marcdatafield
#' @export
as.data.frame.marcdatafield <- function(x, ...) {
  data.frame(tag = formatC(x$tag, width = 3, format = "d", flag = 0),
             ind_1 = x$ind_1,
             ind_2 = x$ind_2,
             code = x$codes,
             value = x$values)
}

#' Print Marc Datafield
#'
#' @param x Marcdatafield object to print
#' @param ... Additional arguments. Will be ignored.
#'
#' @export print.marcdatafield
#' @export
print.marcdatafield <- function(x, ...) {
  cat("Marc21 Datafield:\n")
  cat("  tag: ", formatC(x$tag, width = 3, format = "d", flag = 0), "\n", sep = "")
  cat("  ind_1: ", x$ind_1, "\n", sep = "")
  cat("  ind_2: ", x$ind_2, "\n", sep = "")
  if (length(x$codes) > 0) {
    cat("Data:\n")
    for (i in 1:length(x$codes)) {
      cat("  ", x$codes[i], ": ", x$values[i], "\n", sep = "")
    }
  } else {
    cat("No data.")
  }
}


#' Get Values from Marc Datafield
#'
#' @param .data A marcdatafield object or a list of marcdatafield objects
#' @param codes One or multiple codes for which to retrieve the values
#' @param include_code If true the returned values will be named with their original code
#'
#' @return Either a vector of the matched values or a list of those vectors, depending on input. NA if no codes were matched.
#' @export
get_field_values <- function(.data, codes, include_code = FALSE) {
  if (class(.data) == "marcdatafield") {
    return(get_marcdatafield_values(.data, codes, include_code))
  }

  if (class(.data) == "list" & all(sapply(.data, class) == "marcdatafield")) {
    values <- lapply(.data, get_marcdatafield_values, codes = codes, include_code)
    names(values) <- sapply(.data, "[[", "tag")
    return(values)
  }

  stop(".data needs to be an marcdatafield or a list of marcdatafields")
}

get_marcdatafield_values <- function(field, codes, include_code) {
  values <- field$values[field$codes %in% codes]
  if (identical(values, character(0))) {
    warning("No values exist for the given codes")
    NA
  } else {
    if (include_code) {
      names(values) <- field$codes[field$codes %in% codes]
    }
    values
  }
}


