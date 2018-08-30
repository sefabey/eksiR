#' eksiR.
#'
#' @name eksiR
#' @author person("Sefa", "Ozalp", email = "ozalpas [at] cardiff.ac.uk", role = c("aut", "cre"))
#' @title Scrape Entries From eksisozluk.com
#' @description A webscraper for eksisozluk.com (sour dictionary) which is
#' one of the top 20 sites in Turkey (https://www.alexa.com/topsites/countries/TR).
#' It allows you to scrape data by entry number and returns a neat tibble. It
#' also returns informative errors for deleted entries.

#' @import dplyr
#' @import magrittr
#' @import readr
#' @import glue
#' @import rvest
#' @import xml2
#' @import curl
#' @import parallel
#' @import stringr
#' @docType package
NULL

curl_user_agent <- "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36"
