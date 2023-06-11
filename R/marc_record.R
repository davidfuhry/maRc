new_marcrecord <- function(leader = character(), type = character(), controlfields = list(), datafields = list()) {
  stopifnot(is.character(leader))
  stopifnot(is.character(type))
  stopifnot(is.list(controlfields))
  stopifnot(is.list(datafields))

  obj <- list(
    leader = leader,
    type = type,
    controlfields = controlfields,
    datafields = datafields,
    .datafield_tags = numeric(),
    .datafield_ind_1 = character(),
    .datafield_ind_2 = character()
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

  obj <- validate_marcrecord(new_marcrecord(leader, type, controlfields, datafields))
  rebuild_indices(obj)
}

rebuild_indices <- function(obj) {
  if (length(obj[["datafields"]]) == 0) {
    obj[[".datafield_tags"]] <- numeric()
    obj[[".datafield_ind_1"]] <- character()
    obj[[".datafield_ind_2"]] <- character()
  } else {
    obj[[".datafield_tags"]] <- sapply(obj[["datafields"]], "[[", name = "tag")
    obj[[".datafield_ind_1"]] <- as.character(sapply(obj[["datafields"]], "[[", name = "ind_1"))
    obj[[".datafield_ind_2"]] <- as.character(sapply(obj[["datafields"]], "[[", name = "ind_2"))
  }
  obj
}

#' Read MarcXML files
#'
#' This function will read a MarcXML file and parse it into an marcrecord object.
#'
#' @param source File to read. Can be an url, a path or literal xml.
#'
#' @return Parsed record as marcrecord object
#' @export
read_marcxml <- function(source) {
  if (class(source) == "character") {
    # read_xml can deal with urls, local files as well as literal xml strings
    parsed_xml <- tryCatch({
      xml2::read_xml(source)
    }, error = function(error) {
      stop("Failed to parse xml from given source")
    })
  } else if ("xml_document" %in% class(source)) {
    parsed_xml <- source
  } else {
    stop(paste0("Don't know how to process data of class '", paste(class(source), collapse = ", "), "', giving up."))
  }

  record_list <- xml2::as_list(parsed_xml)

  if (length(record_list) == 1 && names(record_list) == "record") {
    process_marcdata(record_list[["record"]])
  } else if (all(names(record_list) == "record")) {
    lapply(record_list, process_marcdata)
  } else {
    process_marcdata(record_list)
  }

}

process_marcdata <- function(.data) {
  leader <- .data[["leader"]][[1]]


  type <- attr(.data, "type")

  # Parse control fields
  # TODO: Make sure this doesn't break when no control fields exist

  controlfields <- lapply(.data[names(.data) == "controlfield"], "[[", 1)
  names(controlfields) <- as.numeric(sapply(.data[names(.data) == "controlfield"], attr, which = "tag"))

  # Parse the data fields

  datafields <- unname(lapply(.data[names(.data) == "datafield"], function(x) {
    tag <- as.numeric(attr(x, "tag"))
    ind_1 <- as.character(attr(x, "ind1"))
    ind_2 <- as.character(attr(x, "ind2"))

    values <- sapply(x[names(x) == "subfield"], "[[", 1, USE.NAMES = FALSE)
    codes <- sapply(x[names(x) == "subfield"], attr, which = "code", USE.NAMES = FALSE)
    marcdatafield(tag, ind_1, ind_2, codes, values)
  }))

  marcrecord(leader, type, controlfields, datafields)
}

