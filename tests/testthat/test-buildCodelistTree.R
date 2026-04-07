library(omopgenerics)

test_that("buildCodelistTree works correctly", {
  mock_list <- list(
    GroupA = list(Sub1 = newCodelist(list(a = 1:2))),
    GroupB = newCodelist(list(b = as.integer(3)))
  )
  
  res <- buildCodelistTree(mock_list)
  
  # Assert that the output maintains the nested structure
  expect_true(is.list(res$GroupA))
  expect_s3_class(res$GroupB, "codelist")
  
  # The root $merged codelist contains exactly the combined integer IDs c(1, 2, 3)
  expect_true("merged" %in% names(res))
  expect_s3_class(res$merged, "codelist")
  expect_equal(res$merged[[1]], c(1L, 2L, 3L))
  
  # The $GroupA$merged codelist contains exactly c(1, 2)
  expect_true("merged" %in% names(res$GroupA))
  expect_s3_class(res$GroupA$merged, "codelist")
  expect_equal(res$GroupA$merged[[1]], c(1L, 2L))
})
