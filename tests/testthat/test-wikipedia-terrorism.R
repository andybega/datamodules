

test_that("URL dates are parsed", {
  
  url1 <- "https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_2006"
  out <- wikipedia_terrorism_parse_url_date(url1)
  expect_equal(out[["start"]], as.Date("2006-01-01"))
  expect_equal(out[["end"]], as.Date("2006-12-01"))
  
  url2 <- "https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_January%E2%80%93June_2011"
  out <- wikipedia_terrorism_parse_url_date(url2)
  expect_equal(out[["start"]], as.Date("2011-01-01"))
  expect_equal(out[["end"]], as.Date("2011-06-01"))
  
  url3 <- "https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_September_2019"
  out <- wikipedia_terrorism_parse_url_date(url3)
  expect_equal(out[["start"]], as.Date("2019-09-01"))
  expect_equal(out[["end"]], as.Date("2019-09-01"))
  
  urls <- c(url1, url2, url3)
  expect_warning(out <- wikipedia_terrorism_parse_url_date(urls), NA)
  
})

test_that("date stem parsing for tables works", {
  
  # prior to 2010, tables don't have ID and each page contains tables
  # for entire year
  url <- "https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_2006"
  table_id   <- NULL
  
  expect_equal(
    wikipedia_terrorism_parse_table_date(table_id, url),
    list(year = 2006L, month = NA)
  )
  
  table_id <- "NULL"
  expect_equal(
    wikipedia_terrorism_parse_table_date(table_id, url),
    list(year = 2006L, month = NA)
  )
  
  # 2010 and later tables have informative IDs, but between 2011-2014 the
  # pages have month range URLs
  url <- "https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_January%E2%80%93June_2011"
  table_id   <- "terrorIncidents2011Feb"
  expect_equal(
    wikipedia_terrorism_parse_table_date(table_id, url),
    list(year = 2011L, month = 2L)
  )
  
  table_id   <- "terrorIncidents2011Jun"
  expect_equal(
    wikipedia_terrorism_parse_table_date(table_id, url),
    list(year = 2011L, month = 6L)
  )
  
  # from 2015 on there is a page for each month and the tables have IDs
  # sometimes ID does not match page implied date
  url <- "https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_September_2019"
  
  table_id   <- "terrorIncidents2019Sep"
  expect_equal(
    wikipedia_terrorism_parse_table_date(table_id, url),
    list(year = 2019L, month = 9L)
  )
  
  table_id   <- "terrorIncidents2019Mar"
  expect_warning(out <- wikipedia_terrorism_parse_table_date(table_id, url))
  expect_equal(
    out,
    list(year = 2019L, month = 9L)
  )
  
})

