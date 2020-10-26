# based on: http://stackoverflow.com/questions/34068109/how-to-extract-business-hours-between-two-different-times-values-in-r/
#' @title
#' Calculate number of office hours between time stamps
#'
#' @description
#' Calculate number of office hours between time stamps. Office hours is default mon - fri, 8:00 to 16:00
#'
#' @details
#' More info here later? Or remove...
#'
#' @importFrom magrittr %>%
#' @return
#' Number of office hours between time stamps
#' @export
#' @param started Start time of period, as POSIX
#' @param ended End time of period, as POSIX
#' @param working_hours Vector of length 2, start and end of office day in hours. Default c(8,16)
#' @param holidays any holiday or combination of holidays in the '%Y-%m-%d' format. Default c('1901-01-01', '1901-01-02')

# Wrapper function to verify input as valid and to handle NA-values.
difftime_office_hours <- function(started, 
                                  ended, 
                                  working_hours = c(8, 16), 
                                  holidays = c('1901-01-01', '1901-01-02')) {

    # Assert input is correct.
    ### Assertions of input ####
    assertthat::assert_that(is.vector(working_hours))
    assertthat::assert_that(length(working_hours) == 2)
    assertthat::assert_that(assertthat::is.number(working_hours[1]))
    assertthat::assert_that(assertthat::is.number(working_hours[2]))
    assertthat::assert_that(all(working_hours >= 0))
    assertthat::assert_that(all(working_hours <= 24))
    assertthat::assert_that(working_hours[1] <= working_hours[2])
    assertthat::assert_that(all(assertthat::is.time(started),assertthat::is.time(ended)))
    assertthat::assert_that(length(started) == length(ended))
    ### end assertions ####
    
    # Convert to POSIXct if input is class POSIXlt
    if(any(class(started) == "POSIXlt")) started <- as.POSIXct(started)
    if(any(class(ended) == "POSIXlt")) ended <- as.POSIXct(ended)
    
    # Vectors for subsets where time difference is 0/positive or negative
    # Presence of NA in input will give NA as output
    indat_pos <- which(started <= ended)
    indat_neg <- which(started > ended)
    
    # create empty result vector
    result <- rep(lubridate::as.duration(NA),length(started))
    
    # call function for
    result[indat_pos] <-
      difftime_office_hours_no_NA(started = started[indat_pos],
                                  ended = ended[indat_pos],
                                  working_hours = working_hours,
                                  holidays = holidays)
    
    result[indat_neg] <-
      -difftime_office_hours_no_NA(started = ended[indat_neg],
                                   ended = started[indat_neg],
                                   working_hours = working_hours,
                                   holidays = holidays)
    
    return(result)
    
  }

#' Internal calculation of office hours
#' @description Internal calculation of office hours between time stamps
#' @details Don't call this function externally, use \code{difftime_office_hours} that validates input and handles NA-values
#' @param started Start time of period, as POSIX. NA-values is NOT allowed
#' @param ended End time of period, as POSIX. NA-values is NOT allowed
#' @param working_hours Vector of length 2, start and end of office day in hours. Default c(8,16)
#' @param holidays any holiday or combination of holidays in the '%Y-%m-%d' format. Default c('1901-01-01', '1901-01-02')
#' @return Number of office hours between time stamps
#' @keywords internal

# Atomic internal function to actualy calculate difftime
difftime_office_hours_no_NA <- function(started, ended, working_hours, holidays) {
    # When does working hours start at day? decimal hours
    day_start <- lubridate::duration(working_hours[1], units = "hours")
    # When does working hours end at day? decimal hours
    day_end <- lubridate::duration(working_hours[2], units = "hours")
    
    # Set timestamp to be within working hours. all values as duration
    timestamp_day <- function(time_vec) {
      lapply (time_vec, function(timestamp) {
        timestamp %>% 
          time_as_duration %>%
          max(day_start) %>%
          min(day_end)
        }) %>% 
        unlist() %>% 
        lubridate::as.duration() %>% 
        return()
      }
    
    # Hours from start time stamp to end of working hours if not a holiday or a non-work day.
    hours_first_day <- day_end - ifelse(format(started, '%Y-%m-%d') %in% holidays,
                                        day_end,
                                        ifelse(work_days_n(started) == 1,
                                               timestamp_day(started),
                                               day_end)
                                        )
    
    # Hours from start of working hours to time stamp if not a holiday or a non-work day.
    hours_last_day <- (ifelse(format(ended, '%Y-%m-%d') %in% holidays,
                             day_start,
                             ifelse(work_days_n(ended) == 1, 
                                    timestamp_day(ended), 
                                    day_start))) - day_start
  
    #If calculating same-day times:
    hours_first_and_last_day <- ifelse(lubridate::date(started) == lubridate::date(ended) &
                                           !format(started, '%Y-%m-%d') %in% holidays,
                                       min(timestamp_day(ended), day_end) - 
                                           max(timestamp_day(started), day_start),
                                       hours_first_day + hours_last_day) %>% 
        lubridate::as.duration()
  
    # All dates in span started to ended, excluding first + last day
    dates_in_span <- mapply(dates_span_fun, started, ended, SIMPLIFY = FALSE)
    
    # Filter all dates to exclude holidays
    non_holiday_dates <- dates_in_span %>% 
      lapply(grab_non_holidays_only, holidays)
    
    # Get hours from all non-holiday, non-weekends in the days between the first and last day of the timespan:
    hours_working_days <- (work_days_n(non_holiday_dates)) * (day_end - day_start)
    
    return(hours_first_and_last_day + hours_working_days)
}

#' POSIX to duration since mid night
#' @param x a vector of objects of class POSIX time
#' @return object of class duration
#' @keywords internal

time_as_duration <- function(x) {
    lubridate::as.duration(x - lubridate::floor_date(x, unit = "day")) %>% 
    return()
}

#' Number of working days, atomic
#' Calculates number of work days, currently just by week days mon-fri
#'
#' @param x is a vector of dates in POSIX-format
#' @return a numeric vector of number of working days in period
#' @keywords internal

# Atomic function that returns number of days mon-fri in period.
work_days_atomic <- function (x) {
  (lubridate::wday(x) %in% c(2:6)) %>% 
    sum() %>% 
    return()
}

#' Number of working days, wrapper
#' Calls \code{work_days_atomic} that calculates number of work days
#'
#' @param x is a list of vector of dates in POSIX-format
#' @return a numeric vector of number of working days in period
#' @keywords internal
work_days_n <- function (x) {
    lapply(x, work_days_atomic) %>% 
    unlist() %>% 
    return()
}
#' Create sequence of days in span
#' Creates a sequence with dates from \code{started} to \code{ended}, including first + last day. Not vectorized.
#'
#' @param started An atomic vector of POSIX time for start time
#' @param ended An atomic vector of POSIX time for end time
#' @return a numeric vector of number of working days in period
#' @keywords internal
dates_span_fun <- function(started, ended)	{
    day_after_started <- started + lubridate::days(1)
    day_before_ended <- ended - lubridate::days(1)
    if(day_before_ended < day_after_started) {return(as.POSIXct(rep(NA, 1)))}
    return(
        seq(
            lubridate::floor_date(day_after_started, unit = "day"),
            lubridate::floor_date(day_before_ended, unit = "day"), 
            by = "days"
        )
    )
}

#' Filter sequence of days in span to exclude holidays. Vectorized.
#'
#' @param dates a numeric vector of number of working days in period
#' @param holidays any holiday or combination of holidays in the '%Y-%m-%d' format. Default c('1901-01-01', '1901-01-02')
#' @return a numeric vector of number of working days (minus holidays) in period
#' @keywords internal
grab_non_holidays_only <- function(dates, holidays) {
  holidays_to_remove <- holidays %>% as.POSIXct()
  non_holidays <- dates [! dates %in% holidays_to_remove]
  return(non_holidays)
}
