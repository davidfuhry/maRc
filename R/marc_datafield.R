#' R6 Class Representing a Marc21 data field
#'
#' @description
#' This is the base class used to store and interact with marc data fields
#'
#' @details
#' This class provides the base functionality for storing marc data, as well as providing methods
#' to retrieve that data and convert it into other formats like data.frames
MarcDatafield <- R6::R6Class("Marc21Datafield",
                             public = list(

                                 #' @field tag Marc tag of the data field
                                 tag = NA,

                                 #' @field ind_1 First indictator of the data field
                                 ind_1 = NA,

                                 #' @field ind_2 Second indictator of the data field
                                 ind_2 = NA,

                                 #' @field codes Subfield codes in this data field
                                 codes = list(),

                                 #' @field values Subfield values of this data field
                                 values = list(),

                                 #' @description
                                 #'
                                 #' Simplifies this data field by coercing it into a data frame.
                                 #'
                                 #' @return A data frame containing the contents of this data field
                                 #' @export
                                 #'
                                 #' @examples
                                 #' record <- MarcRecord$new()
                                 #' record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")
                                 #'
                                 #' data_field <- record$get_fields(tag = "550")[[1]]
                                 #' data_field$to_data_frame()
                                 to_data_frame = function() {
                                     data.frame(tag = formatC(self$tag, width = 3, format = "d", flag = 0),
                                                ind_1 = self$ind_1,
                                                ind_2 = self$ind_2,
                                                code = self$codes,
                                                values = self$values)
                                 },

                                 #' @description
                                 #'
                                 #' Retrieves values for one or more codes from the data field
                                 #'
                                 #' @param codes The code or codes for which to retreive values. Must be a vector of size >= 1 and type character
                                 #'
                                 #' @return Character vector containing the requested values if there are any, NA otherwise
                                 #' @export
                                 #'
                                 #' @examples
                                 #' record <- MarcRecord$new()
                                 #' record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")
                                 #'
                                 #' data_field <- record$get_fields(tag = "550")[[1]]
                                 #' data_field$get_values(c("i", "a"))
                                 get_values = function(codes) {
                                     values <- self$values[self$codes %in% codes]
                                     if (identical(values, character(0))) {
                                         warning("No values found for the given codes")
                                         NA
                                     } else {
                                         values
                                     }
                                 },

                                 #' @description
                                 #'
                                 #' Print method for a marc data field
                                 #'
                                 #' @export
                                 print = function(...) {
                                     cat("Marc21 Datafield:\n")
                                     cat("  tag: ", formatC(self$tag, width = 3, format = "d", flag = 0), "\n", sep = "")
                                     cat("  ind_1: ", self$ind_1, "\n", sep = "")
                                     cat("  ind_2: ", self$ind_2, "\n", sep = "")
                                     if (length(self$codes) > 0) {
                                         cat("Data:\n")
                                         for (i in 1:length(self$codes)) {
                                             cat("  ", self$codes[i], ": ", self$values[i], "\n", sep = "")
                                         }
                                     } else {
                                         cat("No data.")
                                     }
                                     invisible(self)
                                 },


                                 #' @description
                                 #'
                                 #' Constructor method for a marc data field.
                                 #'
                                 #' @param tag Optional. The tag of the data field.
                                 #' @param ind_1 Optional. First indicator of the data field.
                                 #' @param ind_2 Optional. Second indicator of the data field.
                                 #' @param codes Optional. Data codes of the data field.
                                 #' @param values Optional. Data values of the data field.
                                 #'
                                 #' @export
                                 #'
                                 #' @examples
                                 #' field <- MarcDatafield$new(tag = 550,
                                 #'    ind_1 = " ",
                                 #'    ind_2 = " ",
                                 #'    codes = c("0", "a"),
                                 #'    values = c("A value", "Another value"))
                                 #' field
                                 initialize = function(tag, ind_1, ind_2, codes, values) {
                                     if (!missing(tag)) {
                                         self$tag = tag
                                     }

                                     if (!missing(ind_1)) {
                                         self$ind_1 = ind_1
                                     }

                                     if (!missing(ind_2)) {
                                         self$ind_2 = ind_2
                                     }

                                     if (!missing(codes) && !missing(values)) {
                                         if (length(codes) != length(values)) {
                                             warning("codes and values must be of the same length. This record may be broken.")
                                         }
                                         self$codes = codes
                                         self$values = values
                                     }
                                 }
                             )

)
