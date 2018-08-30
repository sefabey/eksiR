#' Originate topic urls from eksisozluk
#'
#' @param x Character. Takes eksisozluk topic url copied from browser.
#'
#' @return This small function stips the url from page numbers, dates, focus to certain entry IDs etc which all come after '?'
eksi_originate_topic_url <- function(x){
  ifelse(test=(stringr::str_detect(x, '\\?')), yes = (x=stringr::str_remove(x, '(?=\\?).+')), no=x)
}
