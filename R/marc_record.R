#' R6 Class Representing a Marc21 record
#'
#' @description
#' This is the base class used to interact with marc records
#'
#' @details
#' This class provides functions for reading marc records from online or disk sources
#' and to retrieve information from them.
#'
#' Currently only  MarcXML files are supported.
MarcRecord <- R6::R6Class("Marc21Record",
                          public = list(
                              #' @field controlfields The control fields (00X) of the record
                              controlfields = list(),

                              #' @field datafields The data fields (>= 010) of the record
                              datafields = list(),


                                #' @description
                                #' Retrieve data fields from this record while applying optional filtering.
                                #'
                                #' @param tag Optional. Tag(s) for which to filter the record, must be a vector of size >= 1 and type numeric or character
                                #' @param ind_1 Optional. First indicator(s) for which to filter the record, must be a vector of size >= 1 and type character
                                #' @param ind_2 Optional. Second indicator(s) for which to filter the record, must be a vector of size >= 1 and type character
                                #' @param simplify If set to true the data fields will be coerced in to a data frame before returning, otherwise a list of \code{\link[maRc]{MarcDatafield}} will be returned
                                #'
                                #' @return The requested data as either a data frame or a list, depending on \code{simplify}
                                #' @export
                                #'
                                #' @examples
                                #' record <- MarcRecord$new()
                                #' record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")
                                #'
                                #' # Get fields with optional filtering
                                #' record$get_fields(tag = c(548, 550), simplify = TRUE)
                              get_fields = function(tag, ind_1, ind_2, simplify = FALSE) {
                                  tag_matches <- rep(TRUE, length(self$datafields))
                                  if (!missing(tag)) {
                                      if (class(tag) == "character") tag <- as.numeric(tag)
                                      tag_matches <- private$datafield_tags %in% tag
                                  }

                                  ind1_matches <- rep(TRUE, length(self$datafields))
                                  if (!missing(ind_1)) ind1_matches <- private$datafield_ind_1 %in% ind_1

                                  ind2_matches <- rep(TRUE, length(self$datafields))
                                  if (!missing(ind_2)) ind2_matches <- private$datafield_ind_2 %in% ind_2

                                  matching_datafields <- self$datafields[tag_matches & ind1_matches & ind2_matches]

                                  if (simplify && length(matching_datafields) > 0) {
                                      res <- list()
                                      for (i in 1:length(matching_datafields)) {
                                          frame <- matching_datafields[[i]]$to_data_frame()
                                          frame$field_index <- i
                                          frame <- frame[, c(6,1,2,3,4,5)]
                                          res[[i]] <- frame
                                      }
                                      do.call(rbind, res)
                                  } else {
                                      matching_datafields
                                  }
                              },



                            #' @description
                            #'
                            #' Read a marc record from disk or an online source.
                            #' Currently only marcxml is supported.
                            #'
                            #' @param path Path or URL of the data to read. Must be a character vector of size 1.
                            #'
                            #' @export
                            #'
                            #' @examples
                            #' record <- MarcRecord$new()
                            #' record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")
                            #' record
                              read_record = function(path) {
                                  # Right now we only support marcxml

                                  if (class(path) == "character") {
                                      # We most likey got a path or an url, both of which should be finde to let xml2 handle
                                      parsed_xml <- tryCatch({
                                          xml2::read_xml(path)
                                      }, error = function(error) {
                                          stop("Failed to parse xml from given location, as of now only MarcXML is supported.")
                                      })
                                  } else if ("xml_document" %in% class(path)) {
                                      parsed_xml <- path
                                  } else {
                                      stop(paste0("Don't know how to process data of class ", paste(class(path), collapse = ", "), ". Giving up."))
                                  }

                                  record_list <- xml2::as_list(parsed_xml)

                                  # Extract the record

                                  if(length(record_list) == 1 && names(record_list) == "record") {
                                      record <- record_list$record
                                  } else if(all(names(record_list) == "record")) {
                                      stop("Multiple records in one xml file are not supported by this method")
                                  } else {
                                      record <- record_list
                                  }

                                  # We don't do more with the leader right now though that might change

                                  private$leader <- record$leader[[1]]

                                  private$namespace <- xml2::xml_attr(parsed_xml, "xmlns")

                                  private$type <- xml2::xml_attr(parsed_xml, "type")

                                  # Now onto parsing the control fields


                                  controlfields <- record[names(record) == "controlfield"]

                                  control_fields <- list()

                                  for (field in controlfields) {
                                      c_value <- field[[1]]
                                      c_name <- attr(field, "tag")
                                      control_fields[c_name] <- c_value
                                  }

                                  self$controlfields <- control_fields

                                  # Parse the data fields

                                  datafields <- record[names(record) == "datafield"]

                                  # Preparing internal indices

                                  datafield_tags <- numeric()
                                  datafield_ind1 <- character()
                                  datafield_ind2 <- character()

                                  data_fields <- list()

                                  for (field in datafields) {
                                      # Extract the tag

                                      tag <- as.numeric(attr(field, "tag"))
                                      datafield_tags <- c(datafield_tags, tag)

                                      # Extrag ind_1 and ind_2

                                      ind1 <- attr(field, "ind1")
                                      datafield_ind1 <- c(datafield_ind1, ind1)
                                      ind2 <- attr(field, "ind2")
                                      datafield_ind2 <- c(datafield_ind2, ind2)

                                      # We might miss some attributes of viaf marcxml right now, will need to look into this later

                                      codes <- unname(sapply(field[sapply(field, class) == "list"], attr, which = "code"))
                                      values <- unname(unlist(field[sapply(field, class) == "list"]))

                                      # Create the object

                                      datafield <- MarcDatafield$new(tag, ind1, ind2, codes, values)


                                      data_fields <- append(data_fields, datafield)
                                  }

                                  self$datafields = data_fields
                                  private$datafield_tags = datafield_tags
                                  private$datafield_ind_1 = datafield_ind1
                                  private$datafield_ind_2 = datafield_ind2

                                  invisible(self)
                              },



                            #' @description
                            #'
                            #' Print method for a marc record
                            #'
                            #' @export
                              print = function(...) {
                                  cat("MarcRecord with:\n")
                                  cat(paste0("   ", length(self$controlfields), " control fields\n"))
                                  cat(paste0("   ", length(self$datafields), " data fields"))
                              }
                          ),
                          private = list(
                              # XML namespace of the record
                              namespace = NULL,
                              # Type of the record
                              type = NULL,
                              # Unparsed leader
                              leader = NULL,
                              # Indices used for faster operations than on the actual data fields
                              datafield_tags = NULL,
                              datafield_ind_1 = NULL,
                              datafield_ind_2 = NULL
                          )
)
