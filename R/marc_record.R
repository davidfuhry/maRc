MarcRecord <- R6::R6Class("Marc21Record",
                          public = list(
                              namespace = NULL,
                              type = NULL,
                              leader = NULL,
                              controlfields = list(),
                              datafields = list(),
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
                              read_record = function(data) {
                                  # Right now we only support marcxml

                                  if (class(data) == "character") {
                                      # We most likey got a path or an url, both of which should be finde to let xml2 handle
                                      parsed_xml <- tryCatch({
                                          xml2::read_xml(data)
                                      }, error = function(error) {
                                          stop("Failed to parse xml from given location, as of now only MarcXML is supported.")
                                      })
                                  } else if ("xml_document" %in% class(data)) {
                                      parsed_xml <- data
                                  } else {
                                      stop(paste0("Don't know how to process data of class ", paste(class(data), collapse = ", "), ". Giving up."))
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

                                  self$leader = record$leader[[1]]

                                  # Now onto parsing the control fields


                                  controlfields <- record[names(record) == "controlfield"]

                                  control_fields <- character()

                                  for (field in controlfields) {
                                      value <- field[[1]]
                                      names(value) <- attr(field, "tag")
                                      control_fields <- c(control_fields, value)
                                  }

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
                              print = function(...) {
                                  stop("Not implemented yet")
                              }
                          ),
                          private = list(
                              datafield_tags = NULL,
                              datafield_ind_1 = NULL,
                              datafield_ind_2 = NULL
                          )
)
