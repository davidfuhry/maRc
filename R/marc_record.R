new_marcrecord <- function(leader = character(), type = character(), controlfields = list(), datafields = list()) {
  stopifnot(is.character(leader))
  stopifnot(is.character(type))
  stopifnot(is.list(controlfields))
  stopifnot(is.list(datafields))

  obj <- list(
    leader = leader,
    type = type,
    controlfields = controlfields,
    datafields = datafields
  )

  class(obj) <- "marcrecord"
  obj
}

validate_marcrecord <- function(obj) {
  # TODO: Verify leader integrity according to Marc21 specifications

  if (identical(obj$leader, character())) {
    stop("Leader must not be empty", call. = FALSE)
  }

  if (nchar(obj$leader) != 24) {
    stop("Leader must have exactly 24 characters", call. = FALSE)
  }

  if (identical(obj$type, character())) {
    stop("Record type must not be empty", call. = FALSE)
  }

  if (!(obj$type %in% c("Bibliographic", "Authority", "Holdings", "Classification", "Community"))) {
    stop("Record type must be one of 'Bibliographic', 'Authority', 'Holdings', 'Classification' or 'Community'", call. = FALSE)
  }

  if (length(obj$controlfields) > 0 & is.null(names(obj$controlfields))) {
    stop("controlfields must be a named list", call. = FALSE)
  }

  if (length(obj$controlfields) > 0 & !all(grepl("^[1-9]{1}$", names(obj$controlfields)))) {
    stop("controlfields names must be single digits", call. = FALSE)
  }

  if (length(obj$datafields) > 0 & !all(sapply(obj$datafields, class) == "marcdatafield")) {
    stop("datafields property must be a list of marcdatafield objects or empty", call. = FALSE)
  }

  obj
}

marcrecord <- function(leader = character(), type = character(), controlfields = list(), datafields = list()) {
  leader = as.character(leader)
  if (identical(leader, character())) {
    leader = strrep(" ", 24)
  }

  validate_marcrecord(new_marcrecord(leader, type, controlfields, datafields))
}


