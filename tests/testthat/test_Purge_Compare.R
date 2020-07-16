library(testthat)
library(here)

source(here("Purge_Compare.R"))

test_that("Live datasets should have more rows than the demo datasets",{
  expect_gt(raw_ees_live %>% nrow(), raw_ees_demo %>% nrow())
  expect_gt(raw_services_live %>% nrow(), raw_services_demo %>% nrow())
  expect_gt(ees_live %>% nrow(), ees_demo %>% nrow())
  expect_gt(services_live %>% nrow(), services_demo %>% nrow())
})

test_that("Based on EE requirements (A), all needed clients are in Demo", {
  expect_true(length(base::setdiff(a_group, demo_client_ids)) == 0)
})







