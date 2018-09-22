#' Convert data_time column into POSIXct
#'
#' @param x A tibble. The result of scrape function will be the input of this function.
#'
#' @return A tibble. Adds two new columns (POSIXct type) to the existing tibble called original_date_time and edited_date_time. The latter is only for edited entries. Otherwise, fills NA.
#' @export
#'
#' @examples \dontrun{scrape_entry(3000010) %>% eksi_date_time_parse())}
eksi_date_time_parse <- function(x){
  x %>%
    mutate(original_date_time = NA) %>%
    mutate(edited_date_time = NA) %>%
    separate(date_time, into = c("original_date_time","edited_date_time"), sep= " ~ ", remove = F) %>%  # works fine if edited another year
    mutate(original_date_time = ifelse(!is.na(original_date_time),original_date_time,original_date_time)) %>%
    mutate(edited_date_time= case_when(
      nchar(edited_date_time)<6 ~ paste0(str_sub(date_time,1,11),edited_date_time),
      TRUE ~ edited_date_time)) %>% # kudos to tidyverse authors that made this possible
    mutate_at(vars(contains("_date_time")),lubridate::dmy_hm)
}

#' Convert data_time column into POSIXct (Legacy)
#'
#' Before the entry ID 49790 (ancient times), date_time object only stored date, not time. Therefore, this fuction is only necessary for parsing date of enties with id<49790.
#'
#' @param x a tibble. The result of scrape function will be the input of this function.
#'
#' @return A tibble. Adds two new columns (POSIXct type) to the existing tibble called original_date_time and edited_date_time. The latter is only for edited entries. Otherwise, fills NA.
#' @export
#'
#' @examples scrape_entry(3000010) %>% eksi_date_time_parse_legacy()
eksi_date_time_parse_legacy <- function(x){
  x %>%
    mutate(original_date_time = NA) %>%
    mutate(edited_date_time = NA) %>%
    separate(date_time, into = c("original_date_time","edited_date_time"), sep= " ~ ", remove = F) %>%
    mutate(original_date_time = ifelse(!is.na(original_date_time),paste0(original_date_time, " 12:00"),original_date_time)) %>%
    mutate(edited_date_time= case_when(
      nchar(edited_date_time)<6 ~ paste0(str_sub(date_time,1,11),edited_date_time),
      TRUE ~ edited_date_time)) %>% # kudos to tidyverse authors that made this possible
    mutate_at(vars(contains("_date_time")),lubridate::dmy_hm)
}

# Tests
#
# setwd("Desktop/temp")
# a <- purrr::map_df(3000000:3000100,scrape_entry,export_csv=T)
#
# entry_csvs <- list.files(getwd(),full.names = T, recursive = T,pattern = "^eksi_entry_no_" )
#
# b <-  map_df(entry_csvs[1:100], read_csv,col_types = "iccciicicc")
#
# f_na <- read_csv(entry_csvs[2],col_types = "iccciicicc")
# f_no_edit <- read_csv(entry_csvs[3],col_types = "iccciicicc")
# f_same_day <- read_csv(entry_csvs[38],col_types = "iccciicicc")
# f_diff_day <- read_csv(entry_csvs[11],col_types = "iccciicicc")
#
# eksi_date_time_parse(f_na)  #good
# eksi_date_time_parse(f_no_edit) #good, throws a warning
# eksi_date_time_parse(f_same_day) #good
# eksi_date_time_parse(f_diff_day) #good
