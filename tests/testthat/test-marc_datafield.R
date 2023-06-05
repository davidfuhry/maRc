
test_that("datafield_creation_works", {
  field <- marcdatafield(tag = 854,
                         ind_1 = "a",
                         ind_2 = "7",
                         codes = c("a", "5"),
                         values = c("a value", "another value"))
  expect_equal(class(field), "marcdatafield")
  expect_equal(field$tag, 854)
  expect_equal(field$ind_1, "a")
  expect_equal(field$ind_2, "7")
  expect_equal(field$codes, c("a", "5"))
  expect_equal(field$values, c("a value", "another value"))
  expect_equal(length(field$codes), 2)
  expect_equal(length(field$values), 2)
})

test_that("datafield_creation_casts_types_correctly", {
  field <- marcdatafield(tag = "854",
                         ind_1 = "a",
                         ind_2 = 7,
                         codes = c("a", 5),
                         values = list("a value", 7))

  expect_equal(class(field), "marcdatafield")
  expect_equal(field$tag, 854)
  expect_equal(field$ind_2, "7")
  expect_equal(field$codes, c("a", "5"))
  expect_equal(field$values, c("a value", "7"))
  expect_equal(length(field$codes), 2)
  expect_equal(length(field$values), 2)
})

test_that("datafield_validation_works", {
  expect_error(marcdatafield(), "Tag must be provided")
  expect_error(marcdatafield(tag = 1000, ind_1 = "a", ind_2 = "b", codes = c("a"), values = c("some value")), "Datafield tag must be whole number between 1 and 899")
  expect_error(marcdatafield(tag = 12.7, ind_1 = "a", ind_2 = "b", codes = c("a"), values = c("some value")), "Datafield tag must be whole number between 1 and 899")
  expect_error(marcdatafield(tag = 100, ind_1 = "", ind_2 = "b", codes = c("a"), values = c("some value")), "Indicator 1 must be any lowercase ascii letter, number or a blank space")
  expect_error(marcdatafield(tag = 100, ind_1 = "abc", ind_2 = "b", codes = c("a"), values = c("some value")), "Indicator 1 must be any lowercase ascii letter, number or a blank space")
  expect_error(marcdatafield(tag = 100, ind_1 = "ä", ind_2 = "b", codes = c("a"), values = c("some value")), "Indicator 1 must be any lowercase ascii letter, number or a blank space")
  expect_error(marcdatafield(tag = 100, ind_1 = "a", ind_2 = "b", codes = c("a", "7"), values = c("some value")), "There need to be exactly as many subfield codes as values")
  expect_error(marcdatafield(tag = 100, ind_1 = "a", ind_2 = "b", codes = c("abc"), values = c("some value")), "Each subfield code must be exactly one letter or number")
})

test_that("datafield_dataframe_casting_works", {
  field <- marcdatafield(tag = 854,
                         ind_1 = "a",
                         ind_2 = "7",
                         codes = c("a", "5"),
                         values = c("a value", "another value"))

  expected_result <- data.frame(
    tag = c("854", "854"),
    ind_1 = c("a", "a"),
    ind_2 = c("7", "7"),
    code = c("a", "5"),
    value = c("a value", "another value")
  )

  expect_equal(class(field), "marcdatafield")
  expect_equal(as.data.frame(field), expected_result)
})

test_that("datafield_value_extraction_works", {
  field <- marcdatafield(tag = 854,
                         ind_1 = "a",
                         ind_2 = "7",
                         codes = c("a", "5"),
                         values = c("a value", "another value"))

  expect_equal(get_field_values(field, "a"), "a value")
  expect_equal(get_field_values(field, c("a", "5")), c("a value", "another value"))
  expect_equal(get_field_values(field, "a", TRUE), c("a" = "a value"))
  expect_equal(get_field_values(list(field, field), "a", TRUE), list("854" = c("a" = "a value"), "854" = c("a" = "a value")))
  expect_warning(get_field_values(field, "x"), "No values exist for the given codes")
  expect_equal(suppressWarnings(get_field_values(field, "x")), NA)
})

test_that("datafield_printing_works", {
  field <- marcdatafield(tag = 854,
                         ind_1 = "a",
                         ind_2 = "7",
                         codes = c("a", "5"),
                         values = c("a value", "another value"))
  expect_snapshot(print(field))
})

