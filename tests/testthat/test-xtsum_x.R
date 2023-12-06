testthat::test_that("Test1", {

  data("nlswork", package = "sampleSelection")
  sum_table <- xtsum(nlswork, "hours", id = "idcode", t = "year", na.rm = T, dec = 6, return.data.frame = T)


  testthat::expect_contains(class(sum_table), "data.frame")

  #Throw an error when type is dataframe and id and t are not provided
  testthat::expect_error(xtsum(nlswork, "hours"))

  testthat::expect_equal(as.numeric(sum_table[3,"SD"]),
                         round(between_sd(nlswork, "hours", id = "idcode", t = "year", na.rm = T),6))

  #Test that no error is thrown when data id and t are not provided for pdata.frame
  pDataFrame <- plm::pdata.frame(nlswork, index = c("idcode", "year"))
  testthat::expect_silent(pDataFrame)

  testthat::expect_equal(length(xtsum(nlswork, id = "idcode",
                                      t = "year", na.rm = T,
                                      return.data.frame = TRUE)), 7)


  # Test that when variable names are provided, summary stats are produced for all variables
  t <- "year"
  id <- "idcode"
  sum_table2 <- xtsum(nlswork, id = id, t = t, na.rm = T, return.data.frame = TRUE)
  variables2 <- colnames(nlswork)[unlist(lapply(nlswork, is.numeric))]
  testthat::expect_equal(nrow(sum_table2)/4, length(variables2[!variables2 %in% c(id,t)]))

})
