#' Scrape all entries inside a topic
#'
#' @param topic_url Character.
#' @param user_agent haracter. Used to set the user header string in the cURL request. As default, a generic one provided as per https://stackoverflow.com/a/31597823; however, users can provide a custom user agent.
#' @param export_csv Logical. If **TRUE** exports the tibble to a csv file with topic title.
#' @param sleep_time Numeric. A value to set Sys.sleep. Useful to not overwhelm the servers when scraping at scale. Defaults to 0.05.
#'
#' @return Always returns a tibble with same columns. Scrapes all current entires in all pages of the topic.
#' @export
#'
#' @examples scrape_topic(https://eksisozluk.com/tayyip-erdogan-fatih-sarac-ses-kaydi--4224837, export_csv=T)
eksi_scrape_topic <- function(topic_url, user_agent=curl_user_agent, export_csv=FALSE, sleep_time=0.05) {
  Sys.sleep(sleep_time) # sleep briefly to not overwhelm servers of the website when scraping at scale
  topic_url_stripped <- eksi_originate_topic_url(topic_url)
  query_for_max_page_no <- xml2::read_html(curl::curl(topic_url_stripped, handle = curl::new_handle("useragent"= curl_user_agent)), encoding = 'UTF-8')

  max_page_count <- query_for_max_page_no %>%
    html_nodes(xpath = '//*[@id="topic"]/div[1]/div[2]//@data-pagecount' ) %>%
    html_text(trim = T) %>%
    as.integer()

  get_entries_on_page <- function(page_count){
    topic_page_url <- paste0(topic_url_stripped, "?p=", page_count)
    query_topic_page <- xml2::read_html(curl::curl(topic_page_url, handle = curl::new_handle("useragent"= curl_user_agent)), encoding = 'UTF-8')

    get_columns_topic <- function(y) {query_topic_page %>%
        rvest::html_nodes(xpath = y) %>%
        rvest::html_text(trim = T)}

    topic_entries_on_current_page <- tibble::tibble(
      id = get_columns_topic('//*[@id="entry-item-list"]/li//@data-id'),
      text= get_columns_topic('//*[@id="entry-item-list"]/li/div[1]'),
      date_time= get_columns_topic('//a[@class="entry-date permalink"]//text()'),
      author_name = get_columns_topic('//*[@id="entry-item-list"]/li//@data-author'),
      author_id = get_columns_topic('//*[@id="entry-item-list"]/li//@data-author-id'),
      favourite_count = get_columns_topic('//*[@id="entry-item-list"]/li//@data-favorite-count'),
      title_text=get_columns_topic('//h1[@id="title"]'),
      title_id= get_columns_topic('//h1[@id="title"]//@data-id'),
      title_slug= get_columns_topic('//h1[@id="title"]//@data-slug'),
      error_message=NA)
    topic_entries_on_current_page
  }
  topic_dataset <- purrr::map_df(1:max_page_count, get_entries_on_page)

  if (export_csv==TRUE) {
    readr::write_csv(topic_dataset, path = glue::glue(getwd(),"/eksi_topic_dataset_",topic_dataset$title_slug[1L] ,".csv"))
  } else {
    return(topic_dataset)
  }
}





# tests
# someurl <- 'https://eksisozluk.com/sakarya-universitesi--257101?p=24'
# someurl2 <- "https://eksisozluk.com/almanyadaki-copunu-buraya-at-mehmet-yazisi--5429843"
# someurl3 <- "https://eksisozluk.com/dolar-tekrar-2-lira-olsa-alinacaklar--5771485?a=popular"
# someurl4 <- "https://eksisozluk.com/gmail--851474?day=2004-08-30"
# originate_topic_url(someurl)
# originate_topic_url(someurl2)
# originate_topic_url(someurl3)
# originate_topic_url(someurl4)
