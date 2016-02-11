context("expected output and errors")

# Creation of time vectors for all tests
time1 <- as.POSIXct("2016-01-05 10:00:00")
time2 <- as.POSIXct("2016-01-05 13:30:00")
time3 <- as.POSIXct("2016-01-12 13:30:00")
time11 <- rep(time1,4)
time12 <- rep(time2,4)
time13 <- rep(time3,4)
time21 <- time11; time21[2] <- NA
time22 <- time12; time22[3] <- NA
time23 <- time13; time23[4] <- NA

test_that("Error thrown if input is invalid",{
  expect_error(difftime_office_hours())
  expect_error(difftime_office_hours(1,2))
  expect_error(difftime_office_hours(time1,2))
  expect_error(difftime_office_hours(time1,time11))
  expect_error(difftime_office_hours(time1,"foo"))
  expect_error(difftime_office_hours(time1,time2,working_hours = ""))
  expect_error(difftime_office_hours(time1,time2,working_hours = NA))
  expect_error(difftime_office_hours(time1,time2,working_hours = 8,16))
  expect_error(difftime_office_hours(time1,time2,working_hours = 8))
  expect_error(difftime_office_hours(time1,time2,working_hours = c(8,16,32)))
  expect_error(difftime_office_hours(time1,time2,working_hours = c(8,26)))
  expect_error(difftime_office_hours(time1,time2,working_hours = c(-1,16)))
  expect_error(difftime_office_hours(time1,time2,working_hours = c(16,8)))
})

test_that("positive duration return valid data",{
  expect_equal(difftime_office_hours(time1, time2),lubridate::duration(3.5, units = "hours"))
  expect_equal(
    difftime_office_hours(time1, time2, working_hours = c(8,16)),lubridate::duration(3.5, units = "hours")
  )
  expect_equal(difftime_office_hours(time1, time3),lubridate::duration(43.5, units = "hours"))
  expect_equal(difftime_office_hours(time11, time12),rep(lubridate::duration(3.5, units = "hours"),4))
  expect_equal(
    difftime_office_hours(time11, time13,working_hours = c(8,16)),rep(lubridate::duration(43.5, units = "hours"),4)
  )
  expect_equal(difftime_office_hours(as.POSIXlt(time1), as.POSIXlt(time2)),lubridate::duration(3.5, units = "hours"))
})

test_that("negative duration return valid data",{
  expect_equal(difftime_office_hours(time2, time1),lubridate::duration(-3.5, units = "hours"))
  expect_equal(
    difftime_office_hours(time2, time1, working_hours = c(8,16)),lubridate::duration(-3.5, units = "hours")
  )
  expect_equal(difftime_office_hours(time3, time1),lubridate::duration(-43.5, units = "hours"))
  expect_equal(
    difftime_office_hours(time3, time1, working_hours = c(8,16)),lubridate::duration(-43.5, units = "hours")
  )
  expect_equal(difftime_office_hours(time13, time11),rep(lubridate::duration(-43.5, units = "hours"),4))
  expect_equal(
    difftime_office_hours(time13, time11,working_hours = c(8,16)),rep(lubridate::duration(-43.5, units = "hours"),4)
  )
})

test_that("Missing values produces NA",{
  testthat::expect_equal(difftime_office_hours(time1,(as.POSIXct(NA))), lubridate::as.duration(NA))
  testthat::expect_equal(difftime_office_hours((as.POSIXct(NA)),time1), lubridate::as.duration(NA))
  testthat::expect_equal(difftime_office_hours(time21, time22), c(
    lubridate::duration(3.5, units = "hours"),NA, NA,c(lubridate::duration(3.5, units = "hours"))
  ))
  testthat::expect_equal(difftime_office_hours(time22, time21), c(
    lubridate::duration(-3.5, units = "hours"),NA, NA,c(lubridate::duration(-3.5, units = "hours"))
  ))
})

test_that("Start or end time out of office hours",{
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 06:00:00"), as.POSIXct("2016-01-05 10:00:00")
    ),lubridate::duration(2, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-05 06:00:00")
    ),lubridate::duration(-2, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-05 20:00:00")
    ),lubridate::duration(6, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 20:00:00"), as.POSIXct("2016-01-05 10:00:00")
    ),lubridate::duration(-6, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-02 10:00:00"), as.POSIXct("2016-01-05 10:00:00")
    ),lubridate::duration(10, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-02 10:00:00")
    ),lubridate::duration(-10, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-10 10:00:00")
    ),lubridate::duration(30, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-10 10:00:00"), as.POSIXct("2016-01-05 10:00:00")
    ),lubridate::duration(-30, units = "hours")
  )
})

test_that("Modified working_hours, same day",{
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 06:00:00"), as.POSIXct("2016-01-05 10:00:00"),working_hours = c(9, 16)
    ),lubridate::duration(1, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 06:00:00"), as.POSIXct("2016-01-05 10:00:00"),working_hours = c(10, 16)
    ),lubridate::duration(0, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 06:00:00"), as.POSIXct("2016-01-05 10:00:00"),working_hours = c(11, 16)
    ),lubridate::duration(0, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-05 20:00:00"),working_hours = c(8, 12)
    ),lubridate::duration(2, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-05 20:00:00"),working_hours = c(8, 22)
    ),lubridate::duration(10, units = "hours")
  )
})

test_that("Modified working_hours, different day",{
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-10 10:00:00"),working_hours = c(9, 15)
    ),lubridate::duration(23, units = "hours")
  )
  expect_equal(
    difftime_office_hours(
      as.POSIXct("2016-01-05 10:00:00"), as.POSIXct("2016-01-10 10:00:00"),working_hours = c(0, 24)
    ),lubridate::duration(86, units = "hours")
  )
  expect_equal(
    difftime_office_hours(time12, time13,working_hours = c(06, 18))
    ,rep(lubridate::duration(60, units = "hours"),4)
  )
  expect_equal(
    difftime_office_hours(time13, time12,working_hours = c(06, 18))
    ,rep(lubridate::duration(-60, units = "hours"),4)
  )
})
