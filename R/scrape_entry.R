#' Scrape data by entry_id
#'
#' @param entry_id Integer. The entry ID that will be scraped.
#' @param user_agent Character. Used to set the user header string in the cURL request. As default, a generic one provided as per https://stackoverflow.com/a/31597823; however, users can provide a custom user agent.
#' @param export_csv Logical. If **TRUE** exports the tibble to a csv file.
#' @param sleep_time Numeric. A value to set Sys.sleep. Useful to not overwhelm the servers when scraping at scale. Defaults to 0.05.
#'
#' @return Always returns a tibble with same columns. If cURL request returns an error (such as if entry is deleted or servers are temporarily unavailable), returns a tibble with missing values filled with NA.
#'
#' @export
#'
#' @examples scrape_entry(1)
#' @examples scrape_entry(1, export_csv=TRUE)
scrape_entry <- function(entry_id, user_agent=curl_user_agent, export_csv=FALSE, sleep_time=0.05) {
  Sys.sleep(sleep_time) # sleep briefly to not overwhelm servers of the website when scraping at scale
  main_url <- paste0("https://eksisozluk.com/entry/",entry_id)
  query <- try(xml2::read_html(curl::curl(main_url, handle = curl::new_handle("useragent"= curl_user_agent)), encoding = 'UTF-8'),silent = F)
  if ("try-error" %in% class(query)) {
    print(glue::glue("No data returned for Entry ID: ", entry_id))
    entry <- tibble::tibble(
      id= as.character(entry_id),
      text=NA,
      date_time= NA,
      author_name = NA,
      author_id = NA,
      favourite_count =NA,
      title_text=NA,
      title_id=NA,
      title_slug=NA)
  } else {
    get_column <- function(x) {query %>%
        rvest::html_node(xpath = x) %>%
        rvest::html_text(trim = T)}
    print(glue::glue("Data returned for Entry ID: ", entry_id))
    entry <- tibble::tibble(
      id = get_column('//*[@id="entry-item-list"]/li//@data-id'),
      text= get_column('//*[@id="entry-item-list"]/li/div[1]'),
      date_time= get_column('//a[@class="entry-date permalink"]//text()'),
      author_name = get_column('//*[@id="entry-item-list"]/li//@data-author'),
      author_id = get_column('//*[@id="entry-item-list"]/li//@data-author-id'),
      favourite_count = get_column('//*[@id="entry-item-list"]/li//@data-favorite-count'),
      title_text=get_column('//h1[@id="title"]'),
      title_id= get_column('//h1[@id="title"]//@data-id'),
      title_slug= get_column('//h1[@id="title"]//@data-slug'))
  }
  if (export_csv==TRUE) {
    readr::write_csv(entry, path = glue::glue(getwd(),"/eksi_entry_no_",entry_id , ".csv"))
  } else {
    return(entry)
    }
}
