MarcDatafield <- R6::R6Class("Marc21Datafield",
                             public = list(
                                 as.data.frame = function() {
                                     stop("Not implemented yet")
                                 },
                                 get_values = function(codes) {

                                 }
                             ),
                             private = list(
                                 tag = NA_integer_,
                                 ind_1 = NA_character_,
                                 ind_2 = NA_character_,
                                 codes = list(),
                                 values = list()
                             ),
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

                                 if (!missing(codes)) {
                                     self$codes = codes
                                 }

                                 if (!missing(values)) {
                                     self$values = values
                                 }
                             }
)
