context("expected output and errors")

test_that("Right duration returned with valid data",{
  time1 <- as.POSIXct("2016-01-05 10:00:00")
  time2 <- as.POSIXct("2016-01-05 13:30:00")
  expect_equal(difftime_office_hours(time1, time2),lubridate::duration(3.5, units = "hours"))
  expect_equal(
    difftime_office_hours(time1, time2, working_hours = c(8,16)),lubridate::duration(3.5, units = "hours")
  )
})
#
# time3 <- as.POSIXct("2016-02-05 13:30:00")
#
# difftime_office_hours(time3, time2)
