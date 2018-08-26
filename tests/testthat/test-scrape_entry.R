context("scrape_entry")
test_that("scrape_entry works as expected", {
# test 1=====
# scrape existing entry
  library(tidyverse)
  result1 <- scrape_entry(1) %>%
    select(text)# first ever written eksi entry from the year 1999, 'pena'
  check1 <- readRDS('test_data/result1.rds') %>%
    select(text)# expect 520 rows and 99 columns
  expect_equal(result1,check1)
# test 2=====
# scrape removed entry
  result2 <- scrape_entry(2) # a deleted/removed entry
  check2 <- tibble::tibble(id="2", text=NA, date_time=NA, author_name=NA, author_id=NA, favourite_count=NA, title_text=NA, title_id=NA, title_slug=NA)
  expect_equal(result2,check2)
# test 3======
# text exporting entry to a csv file
  # tests 2 aspects:
  # 1. Exported csv file has the correct name.
  # 2. Exported csv file has the correct size.
  scrape_entry(76805451, export_csv = T)
  result3 <- file.size("eksi_entry_no_76805451.csv")
  check3 <- 659
  expect_equal(result3, check3)
# test 4======
  # text exporting deleted/removed entry to a csv file
  # tests 2 aspects:
  # 1. Exported csv file has the correct name.
  # 2. Exported csv file has the correct size.
  scrape_entry(3, export_csv = T)
  result4 <- file.size("eksi_entry_no_3.csv")
  check4 <- 113
  expect_equal(result4, check4)
})
