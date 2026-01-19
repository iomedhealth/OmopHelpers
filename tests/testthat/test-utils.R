test_that("clean_name works correctly", {
    expect_equal(clean_name("My Name - 123"), "my_name")
    expect_equal(clean_name(" Simple Name "), "simple_name")
    expect_equal(clean_name("Complex@Name#Test"), "complex_name_test")
    expect_equal(clean_name("multiple___underscores"), "multiple_underscores")
})

test_that("process_codelists works correctly", {
    mock_codelist <- list(
        "Code A - 001" = list(1:5),
        "Code B" = list(6:10)
    )

    processed <- process_codelists(mock_codelist)

    expect_named(processed, c("code_a", "code_b"))
    expect_equal(processed$code_a, list(1:5))
    expect_equal(processed$code_b, list(6:10))
})
