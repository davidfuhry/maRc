
test_that("record_creation_works", {
  record <- marcrecord(type = "Authority")
  expect_equal(class(record), "marcrecord")
  expect_equal(record$type, "Authority")

  record <- marcrecord(leader = strrep(" ", 24),
                       type = "Bibliographic",
                       controlfields = list("4" = "value"),
                       datafields = list(marcdatafield(tag = 400,
                                                       ind_1 = " ",
                                                       ind_2 = " ")))

  expect_equal(class(record), "marcrecord")
  expect_equal(record$type, "Bibliographic")
  expect_equal(length(record$datafields), 1)
})

test_that("record_validation_works", {
  expect_error(marcrecord(), "Record type must not be empty")
  expect_error(marcrecord(type = "notarecordtype"), "Record type must be one of 'Bibliographic', 'Authority', 'Holdings', 'Classification' or 'Community'")
  expect_error(marcrecord(leader = "notavalidleader"), "Leader must have exactly 24 characters")
  expect_error(marcrecord(type = "Bibliographic", controlfields = list("noname")), "controlfields must be a named list")
  expect_error(marcrecord(type = "Bibliographic", controlfields = list("123" = "toohighanumber")), "controlfields names must be single digits")
  expect_error(marcrecord(type = "Bibliographic", datafields = list("wrong", "type")), "datafields property must be a list of marcdatafield objects or empty")
  field <- marcdatafield(tag = 400,
                         ind_1 = " ",
                         ind_2 = " ")
  expect_error(marcrecord(type = "Bibliographic", datafields = list("mixed", field, "types")), "datafields property must be a list of marcdatafield objects or empty")
})

test_that("index_creation_works", {
  fields <- list(marcdatafield(tag = 854,
                               ind_1 = "a",
                               ind_2 = "7",
                               codes = c("a", "5"),
                               values = c("a value", "another value")),
                 marcdatafield(tag = 855,
                               ind_1 = "b",
                               ind_2 = "8",
                               codes = c("a", "5"),
                               values = c("a value", "another value")))

  record <- marcrecord(leader = strrep(" ", 24),
                       type = "Bibliographic",
                       controlfields = list("4" = "value"),
                       datafields = fields)

  expect_equal(record[[".datafield_tags"]], c(854, 855))
  expect_equal(record[[".datafield_ind_1"]], c("a", "b"))
  expect_equal(record[[".datafield_ind_2"]], c("7", "8"))

  record <- marcrecord(type = "Authority")

  expect_equal(record[[".datafield_tags"]], numeric())
  expect_equal(record[[".datafield_ind_1"]], character())
  expect_equal(record[[".datafield_ind_2"]], character())

})

