#' A dense time-span text
#' 
#' When adding a time span text we often don't want to write *3 jun - 10 jun* but
#' shorten it to *3 - 10 jun* while retaining month and year info only if the span
#' crosses between months or years.
#' 
#' @param times The dates or posix timestamps to used for timespan
#' @param month_format The \code{\link[base]{strftime}} format string for months
#' @param full_year_format The \code{\link[base]{strftime}} format string for the full year
#' @param start_stop_glue_txt The string used in the \code{\link[glue]{glue}} for putting the 
#'  start and stop dates together into one string
#' @return \code{string} A string describing the time span
#'
#' @examples
#' time_to_span_txt(as.POSIXct(c("2020-01-02", "2020-03-01", NA)))
#' # 2 jan to 1 mar
#'
#' @importFrom lubridate mday as_date
#' @importFrom stringr str_trim
#' @importFrom glue glue
#' @export
time_to_span_txt <- function(times, 
                             month_format = getOption("Gmisc_time_to_span_txt_month", default = '%b'),
                             full_year_format = getOption("Gmisc_time_to_span_txt_month", default = '%e %b %y'),
                             start_stop_glue_txt = getOption("Gmisc_time_to_span_txt_template", default = "{start} to {stop}")) {
  times <- as_date(times)
  times <- times[!is.na(times) & !is.infinite(times)]
  min_time <- min(times)
  max_time <- max(times)
  
  if (year(min_time) != year(max_time)) {
    start <- strftime(min_time, format = full_year_format) %>% str_trim()
    stop <- strftime(max_time, format = full_year_format) %>% str_trim()
  } else if (month(min_time) != month(max_time)) {
    start <- strftime(min_time, format = glue("{mday(min_time)} {month_format}"))
    stop <- strftime(max_time, format = glue("{mday(max_time)} {month_format}"))
  } else {
    start <- day(min_time)
    stop <- strftime(max_time, format = glue("{mday(max_time)} {month_format}"))
  }
  
  glue(start_stop_glue_txt)
}
