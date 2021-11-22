MarcRecord <- R6::R6Class("Marc21Record",
                          public = list(
                              get_fields = function(tag, ind_1, ind_2) {
                                  stop("Not implemented yet")
                              },
                              read_record = function(data) {

                              }
                          ),
                          private = list(
                              leader = NA_character_,
                              datafields = list(),
                              errors = list()
                          )
)
