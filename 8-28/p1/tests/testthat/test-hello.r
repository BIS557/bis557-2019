library(testthat)

context("Test hello function")

test_that("The hello function works with a string.", {
    expect_equal(hello("BIS557"), "Hello, BIS557!")
})
