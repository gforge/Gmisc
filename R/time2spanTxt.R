#' A dense time-span text
#'
#' When adding a time span text we often don't want to write *3 jun - 10 jun* but
#' shorten it to *3 - 10 jun* while retaining month and year info only if the span
#' crosses between months or years.
#'
#' @details
#'
#' There are options that can be set using the \code{\link[base:options]{options}}:
#'
#' * `Gmisc_time2spanTxt_day_month` The date with day + month as formatted by \code{\link[glue]{glue}} where the time is passed as `time`.
#' * `Gmisc_time2spanTxt_full_year` The full date with day + month + year as formatted by \code{\link[glue]{glue}} where the time is passed as `time`.
#' * `Gmisc_time2spanTxt_template` The merge of the stop & start elements using \code{\link[glue]{glue}}.
#'
#' @md
#' @param times The dates or POSIX timestamps to used for time span
#' @param day_month_glue_txt The \code{\link[glue]{glue}} string to format days and months with `time` as the time input
#' @param full_year_format The \code{\link[glue]{glue}} string to format the full year with `time` as the time input
#' @param start_stop_glue_txt The string used in the \code{\link[glue]{glue}} for putting the
#'  start and stop dates together into one string
#' @return \code{string} A string describing the time span
#'
#' @examples
#' time2spanTxt(as.POSIXct(c("2020-01-02", "2020-03-01", NA)))
#' # 2 jan to 1 mar
#'
#' @importFrom lubridate mday as_date year month
#' @importFrom stringr str_trim
#' @importFrom glue glue
#' @export
time2spanTxt <- function(times,
                         day_month_glue_txt = getOption("Gmisc_time2spanTxt_day_month",
                                                        default = "{mday(time)} {month(time, label = TRUE)}"),
                         full_year_format = getOption("Gmisc_time2spanTxt_full_year",
                                                      default = "{mday(time)} {month(time, label = TRUE)} {year(time)}"),
                         start_stop_glue_txt = getOption("Gmisc_time2spanTxt_template",
                                                         default = "{start} to {stop}")) {
  times <- as_date(times)
  times <- times[!is.na(times) & !is.infinite(times)]
  min_time <- min(times)
  max_time <- max(times)

  if (year(min_time) != year(max_time)) {
    start <- glue(full_year_format, time = min_time)
    stop <- glue(full_year_format, time = max_time)
  } else if (month(min_time) != month(max_time)) {
    start <- glue(day_month_glue_txt, time = min_time)
    stop <- glue(day_month_glue_txt, time = max_time)
  } else {
    start <- mday(min_time)
    stop <- glue(day_month_glue_txt, time = max_time)
  }

  glue(start_stop_glue_txt)
}
