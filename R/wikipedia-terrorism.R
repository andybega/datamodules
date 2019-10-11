#' Scrape Wikipedia terrorism data
#'
#' Scrape events from the Wikipedia terrorism lists at
#' <https://en.wikipedia.org/wiki/Template:Lists_of_Terrorist_Incidents>
#'
#' @param from year or "YYYY-MM" string
#' @param to year or "YYYY-MM" string, if NULL will default to latest
#'
#' @export
wikipedia_terrorism_scrape <- function(from = 2019, to = NULL) {
  
  # parse from and to dates
  if (nchar(from)==4) from <- as.Date(paste0(from, "-01-01"))
  if (nchar(from)==7) from <- as.Date(paste0(from, "-01"))
  if (!inherits(from, "Date")) {
    stop("Could not parse 'from', form should be 'YYYY' or 'YYYY-MM'")
  }
  
  if (is.null(to)) to <- 2100
  if (nchar(to)==4) to <- as.Date(paste0(to, "-01-01"))
  if (nchar(to)==7) to <- as.Date(paste0(to, "-01"))
  if (!inherits(to, "Date")) {
    stop("Could not parse 'to', form should be 'YYYY' or 'YYYY-MM'")
  }
  
  urls <- wikipedia_terrorism_pages()
  # extract normalized date from URL
  url_date <- wikipedia_terrorism_parse_url_date(urls)[["start"]]
  
  want <- url_date >= from & url_date <= to
  urls <- urls[want]
  
  # the actual meat and potatoes...loop through and get the pages/tables/data
  datatbl <- list()
  for (i in seq_along(urls)) {
    tbl_i <- wikipedia_terrorism_scrape_table(urls[i])
    datatbl[[i]] <- tbl_i
  }
  datatbl <- dplyr::bind_rows(datatbl)
  datatbl
}

#' @export
wikipedia_terrorism_pages <- function() {
  url <- "https://en.wikipedia.org/wiki/Template:Lists_of_Terrorist_Incidents"
  page <- xml2::read_html(url)
  page_urls <- rvest::html_nodes(page, "a") %>%
    rvest::html_attr("href") %>%
    purrr::keep(stringr::str_detect(., "terrorist_incidents_in")) %>%
    paste0("https://en.wikipedia.org/", .)
  
  # from 2015 on, there are month pages but also summary year pages
  # exclude the year pages to avoid duplication
  exclude <- page_urls %>%
    stringr::str_extract(., "in_[0-9]{4}$") %>%
    stringr::str_extract(., "[0-9]+") %>%
    as.integer() %>%
    sapply(., function(x) isTRUE(x >= 2015))
  page_urls <- page_urls[!exclude]
  
  page_urls
}

#' @export
wikipedia_terrorism_scrape_table <- function(url) {
  
  page <- httr::GET(url)
  
  # a page can include multiple tables, some of which may be irrelevant
  all_tables <- XML::readHTMLTable(doc = httr::content(page, "text"),
                                   stringsAsFactors = FALSE,
                                   header = TRUE)
  # filter out irrelevant tables
  all_tables <- id_relevant_tables(all_tables)
  table_ids  <- names(all_tables)
  
  # iterate through data tables and clean
  datatbl <- list()
  for (i in seq_along(all_tables)) {
    tbl_i <- tibble::as_tibble(all_tables[[i]])
    table_id_i <- table_ids[i]
    
    date_stem <- wikipedia_terrorism_parse_table_date(table_id_i, url)
    
    colnames(tbl_i) <- colnames(tbl_i) %>%
      tolower() %>%
      stringr::str_replace_all(" ", "_") %>%
      stringr::str_replace_all("\n", "")
    
    # replace em-dash with regular dash for dates like 1-3 January
    tbl_i$date <- stringr::str_replace(tbl_i$date, "\u2013", "-")
    
    # some tables have dates like "1", "1-3"; others include the month,
    # like "January 1"
    # if no month, use the date stem from ULR to id
    month <- ifelse(stringr::str_detect(tbl_i$date, "^[0-9\\-]+$"),
                    date_stem$month,
                    tbl_i$date %>%
                      stringr::str_extract("^[A-Za-z]+") %>%
                      match(month.name)
    )
    
    # take out month in date if present, leaving only day
    # keep orig for debugging dump if parsing failures
    date_orig  <- tbl_i$date
    tbl_i$date <- tbl_i$date %>%
      stringr::str_extract("[0-9\\-]+$")
    
    # for multi-day dates, keep first day and record end date in end_date var
    tbl_i$end_date <- tbl_i$date %>%
      stringr::str_extract(., "-[0-9]{1,2}$") %>%
      stringr::str_remove("-") %>%
      ifelse(!is.na(.), paste0(month, .), .) %>%
      as.Date(format = "%Y-%m-%d")
    
    tbl_i$date <- tbl_i$date %>%
      stringr::str_extract(., "^[0-9]+") %>%
      paste(date_stem$year, month, ., sep = "-") %>%
      as.Date(format = "%Y-%m-%d")
    
    # parse minimum fatality count
    tbl_i$dead_min <- tbl_i$dead %>%
      stringr::str_extract(., "^[0-9]+") %>%
      as.numeric()
    
    # warn if there were parsing failures
    if (any(is.na(tbl_i$date))) {
      idx <- is.na(tbl_i$date)
      payload <- tibble::as_tibble(cbind(orig = date_orig[idx],
                                         tbl_i[idx, "date"],
                                         stringsAsFactors = FALSE))
      payload <- paste(capture.output(print(payload)), collapse = "\n")
      warning("Some dates were not parsed\n",
              "Table: ", table_id_i, "\n",
              payload)
    }
    
    if (any(is.na(tbl_i$dead_min))) {
      payload <- tbl_i[is.na(tbl_i$dead_min), c("dead", "dead_min")]
      payload <- paste(capture.output(print(payload)), collapse = "\n")
      warning("Some fatality figures were not parsed\n",
              "Table: ", table_id_i, "\n",
              payload)
    }
    
    # finished with this chunk
    datatbl[[i]] <- tbl_i
    
  }
  
  datatbl <- dplyr::bind_rows(datatbl)
  datatbl
}

wikipedia_terrorism_parse_table_date <- function(table_id, url) {
  
  stopifnot(length(table_id)==1 | is.null(table_id),
            length(url)==1)
  
  if (length(table_id)==1) {
    if (table_id=="NULL") table_id <- NULL
  }
  
  url_dates <- wikipedia_terrorism_parse_url_date(url)
  
  if (is.null(table_id)) {
    stem <- list(year = as.integer(substr(url_dates$start, 1, 4)),
                 month = NA)
  } else {
    # table_id like terrorIncidents2011Feb
    table_id_end <- stringr::str_extract(table_id, "[0-9]{4}[A-Za-z]{3}")
    year         <- as.integer(substr(table_id_end, 1, 4))
    month        <- match(substr(table_id_end, 5, 7), month.abb)
    
    # sometimes the table can have an incorrect ID on wikipedia, i.e.
    # page ULR for January 2019 events but the table ID ends in something
    # other
    # check for this and warn
    if (url_dates$start==url_dates$end) {
      url_month <- as.integer(substr(url_dates$start, 6, 7))
      if (url_month!=month) {
        warning("Wikipedia table ID date does not match implied URL date, using URL date:\n",
                "Table ID: ", table_id, "\n",
                "URL: ", url)
        month <- url_month
      }
    }
    stem <- list(year = year, month = month)
  }
  
  stem
}

wikipedia_terrorism_parse_url_date <- function(x) {
  # there are three kinds of URLs
  # year only: https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_2006
  # year and month range: https://en.wikipedia.org//wiki/List_of_terrorist_incidents_in_January%E2%80%93June_2011
  # year and month: List_of_terrorist_incidents_in_September_2019
  
  xbase <- basename(x)
  
  dates <- lapply(xbase, function(x) {
    year <- x %>%
      stringr::str_extract("[0-9]{4}$") %>%
      as.integer()
    
    if (stringr::str_detect(x, "incidents_in_[0-9]{4}$")) {
      start_date <- as.Date(paste0(year, "-01-01"))
      end_date   <- as.Date(paste0(year, "-12-01"))
    }
    
    if (stringr::str_detect(x, "%E2%80%93")) {
      month1 <- x %>%
        stringr::str_extract("[A-Za-z]+%E2%80%93") %>%
        stringr::str_remove("%E2%80%93")
      month2 <- x %>%
        stringr::str_extract("%E2%80%93[A-Za-z]+") %>%
        stringr::str_remove("%E2%80%93")
      start_date <- as.Date(paste0(year, "-", month1, "-01"), format = "%Y-%B-%d")
      end_date   <- as.Date(paste0(year, "-", month2, "-01"), format = "%Y-%B-%d")
    }
    
    if (stringr::str_detect(x, "_in_[A-Za-z]+_[0-9]+$")) {
      month <- x %>%
        stringr::str_extract(., "_[A-Za-z]+_[0-9]{4}$") %>%
        stringr::str_extract("[A-Za-z]+")
      start_date <- end_date <- as.Date(paste0(year, "-", month, "-01"),
                                        format = "%Y-%B-%d")
    }
    tibble::tibble(start = start_date, end = end_date)
  })
  
  # put it together
  dplyr::bind_rows(dates)
}


id_relevant_tables <- function(x) {
  # for year pages, e.g. 2011, the tables don't have names
  # for year-month pages the tables usually have names to ID them
  # easier to check if the first column has the name "Date\n"
  yes <- sapply(x, function(xx) {
    isTRUE(names(xx)[1]=="Date\n")
  })
  if (!any(yes)) {
    stop("Something is wrong, could not find any relevant tables")
  }
  x[yes]
}


