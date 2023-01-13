ok_return_val <- function(fun_name,...) {
    testthat::test_that(paste("function",fun_name,"returns correct result."),...)
}
