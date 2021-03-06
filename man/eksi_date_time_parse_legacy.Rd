% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eksi_date_time_parser.R
\name{eksi_date_time_parse_legacy}
\alias{eksi_date_time_parse_legacy}
\title{Convert data_time column into POSIXct (Legacy)}
\usage{
eksi_date_time_parse_legacy(x)
}
\arguments{
\item{x}{a tibble. The result of scrape function will be the input of this function.}
}
\value{
A tibble. Adds two new columns (POSIXct type) to the existing tibble called original_date_time and edited_date_time. The latter is only for edited entries. Otherwise, fills NA.
}
\description{
Before the entry ID 49790 (ancient times), date_time object only stored date, not time. Therefore, this fuction is only necessary for parsing date of enties with id<49790.
}
\examples{
scrape_entry(3000010) \%>\% eksi_date_time_parse_legacy()
}
