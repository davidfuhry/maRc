MarcDatafield <- R6::R6Class("Marc21Datafield",
                             public = list(
                                 tag = NA,
                                 ind_1 = NA,
                                 ind_2 = NA,
                                 codes = list(),
                                 values = list(),
                                 as.data.frame = function() {
                                     data.frame(tag = formatC(self$tag, width = 3, format = "d", flag = 0),
                                                ind_1 = self$ind_1,
                                                ind_2 = self$ind_2,
                                                code = self$codes,
                                                values = self$values)
                                 },
                                 get_values = function(codes) {
                                     stop("Not implemented yet")
                                 },
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
                                             warning("codes and values must be of the same length, will ignore these arguments")
                                         }
                                         self$codes = codes
                                         self$values = values
                                     }
                                 }
                             )

)
