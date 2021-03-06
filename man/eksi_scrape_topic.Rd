% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_topic.R
\name{eksi_scrape_topic}
\alias{eksi_scrape_topic}
\title{Scrape all entries inside a topic}
\usage{
eksi_scrape_topic(topic_url, user_agent = curl_user_agent,
  export_csv = FALSE, sleep_time = 0.05)
}
\arguments{
\item{topic_url}{Character.}

\item{user_agent}{haracter. Used to set the user header string in the cURL request. As default, a generic one provided as per https://stackoverflow.com/a/31597823; however, users can provide a custom user agent.}

\item{export_csv}{Logical. If **TRUE** exports the tibble to a csv file with topic title.}

\item{sleep_time}{Numeric. A value to set Sys.sleep. Useful to not overwhelm the servers when scraping at scale. Defaults to 0.05.}
}
\value{
Always returns a tibble with same columns. Scrapes all current entires in all pages of the topic.
}
\description{
Scrape all entries inside a topic
}
\examples{
scrape_topic(https://eksisozluk.com/tayyip-erdogan-fatih-sarac-ses-kaydi--4224837, export_csv=T)
}
