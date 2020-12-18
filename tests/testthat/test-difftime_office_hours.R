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


test_that("Holidays are removed as expected",{
  expect_equal(
    difftime_office_hours( #first day is a holiday
      as.POSIXct("2016-01-05 8:00:00"), as.POSIXct("2016-01-07 16:00:00"),working_hours = c(8, 16),
      holidays = "2016-01-05"
    ),lubridate::duration(16, units = "hours")
  )
  
  expect_equal(
    difftime_office_hours( #first two days are holidays
      as.POSIXct("2016-01-05 8:00:00"), as.POSIXct("2016-01-07 16:00:00"),working_hours = c(8, 16),
      holidays = c("2016-01-05","2016-01-06")
    ),lubridate::duration(8, units = "hours")
  )
  
  expect_equal(
    difftime_office_hours( #both days are holidays
      as.POSIXct("2016-01-05 8:00:00"), as.POSIXct("2016-01-06 16:00:00"),working_hours = c(8, 16),
      holidays = c("2016-01-05","2016-01-06")
    ),lubridate::duration(0, units = "hours")
  )
  
  expect_equal(
    difftime_office_hours( #middle day is a holiday
      as.POSIXct("2016-01-05 8:00:00"), as.POSIXct("2016-01-07 16:00:00"),working_hours = c(8, 16),
      holidays = c("2016-01-06")
    ),lubridate::duration(16, units = "hours")
  )
  
  
  testthat::expect_equal(
    difftime_office_hours(as.POSIXct("2020-01-06 14:17:00"), 
                          as.POSIXct("2020-01-08 07:21:00")) %>% as.numeric(),
    34980)
  
  testthat::expect_equal(
    difftime_office_hours(as.POSIXct("2020-01-06 14:17:00"), 
                          as.POSIXct("2020-01-08 07:21:00"), 
                          working_hours = c(7, 19)) %>% as.numeric(), 
    61440)
  
  testthat::expect_equal(
    difftime_office_hours(as.POSIXct("2016-01-05 8:00:00"), 
                          as.POSIXct("2016-01-07 16:00:00"),
                          working_hours = c(8, 16), 
                          holidays = "2016-01-05") %>% as.numeric(),
    57600) 
  
  
  testthat::expect_equal(
    difftime_office_hours(as.POSIXct("2016-01-05 8:00:00"),
                          as.POSIXct("2016-01-07 16:00:00"),
                          working_hours = c(8, 16),
                          holidays = c("2016-01-05","2016-01-06")) %>% as.numeric(),
    28800   
  )
  
  
  
  testthat::expect_equal(
    difftime_office_hours(lubridate::mdy_hm('05/17/2020 12:15'), 
                          lubridate::mdy_hm('05/18/2020 08:01'), 
                          holidays = c('2020-05-01', '2020-05-08', '2020-05-21'), 
                          working_hours = c(8, 18)) %>% as.numeric(),
    60)
  
  
  testthat::expect_equal(
    difftime_office_hours(lubridate::mdy_hm('05/01/2020 10:00'), 
                          lubridate::mdy_hm('05/04/2020 08:59'), 
                          holidays = c('2020-05-01', '2020-05-08', '2020-05-21'), 
                          working_hours = c(8, 18)) %>% as.numeric(),
    3540)
  
  
  testthat::expect_equal(
    difftime_office_hours(lubridate::mdy_hm('05/04/2020 09:10'), 
                          lubridate::mdy_hm('05/04/2020 09:18'), 
                          holidays = c('2020-05-01', '2020-05-08', '2020-05-21'), 
                          working_hours = c(8, 18)) %>% as.numeric(),
    480)
  
  
  
})