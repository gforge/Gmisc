library('testthat')
context('pathJoin')

test_that("time2spanTxt basics", {
  expect_equal(time2spanTxt(as.POSIXct(c("2020-01-02", "2020-03-01", NA)),
                            day_month_glue_txt = "{mday(time)} {month(time, label = TRUE, locale = 'C')}",
                            start_stop_glue_txt = "{start}-{stop}"),
               "2 Jan-1 Mar")

  expect_equal(time2spanTxt(as.Date(c("2020-01-02", "2020-03-11", NA)),
                            day_month_glue_txt = "{mday(time)} {month(time, label = TRUE, locale = 'C')}",
                            start_stop_glue_txt = "{start}-{stop}"),
               "2 Jan-11 Mar")

  expect_equal(time2spanTxt(as.Date(c("2020-01-02", "2020-03-11", "2020-02-01")),
                            day_month_glue_txt = "{mday(time)} {month(time, label = TRUE, locale = 'C')}",
                            start_stop_glue_txt = "{start}-{stop}"),
               "2 Jan-11 Mar")
})
