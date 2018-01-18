context("utilityFunctions")

test_that("bayesianGonzalezWu gives warnings and return values", {
  stims <- c(0.5, 0.7, 1.2, 0.9)
  
  expect_error(logOdds(stims)
                 , "Some inputs to logOdds were >1")
  expect_error(logOdds(stims-1)
                 , "Some inputs to logOdds were <0")
 results <- logOdds(seq(0,1, 0.1))
   expect_lt(max(abs(results -  c(-11.5129155,  -2.1972246,  -1.3862944,  -0.8472979,  -0.4054651,   0.0000000,
         0.4054651,   0.8472979,   1.3862944,   2.1972246,   11.5129155))), 1e-4)
  
})
