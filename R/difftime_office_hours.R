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
#' @param working_hours Vector of lenght 2, start and end of office day in hours. Default c(8,16)

# Wrapper function to verify input as valid and to handle NA-values.
difftime_office_hours <-
  function(started, ended, working_hours = c(8, 16)) {
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
                                  working_hours = working_hours)

    result[indat_neg] <-
      -difftime_office_hours_no_NA(started = ended[indat_neg],
                                   ended = started[indat_neg],
                                   working_hours = working_hours)

    return(result)

  }

#' Internal calculation of office hours
#' @description Internal calculation of office hours between time stamps
#' @details Don't call this funciton externally, use \code{difftime_office_hours} that validates input and handles NA-values
#' @param started Start time of period, as POSIX. NA-values is NOT allowed
#' @param ended End time of period, as POSIX. NA-values is NOT allowed
#' @param working_hours Vector of lenght 2, start and end of office day in hours. Default c(8,16)
#' @return Number of office hours between time stamps
#' @keywords internal

# Atomic internal function to actualy calculate difftime
difftime_office_hours_no_NA <-
  function(started, ended, working_hours) {
    # When does working hours start at day? decimal hours
    day_start <-
      lubridate::duration(working_hours[1], units = "hours")
    # When does working hours end at day? decimal hours
    day_end <-
      lubridate::duration(working_hours[2], units = "hours")

    # Set timestamp to be within working hours. all values as duration
    timestamp_day <-
      function(time_vec) {
        lapply (time_vec, function(timestamp) {
          timestamp %>% time_as_duration %>% max(day_start) %>%
            min(day_end)
        }) %>% unlist %>% lubridate::as.duration() %>% return
      }

    # Hours from start time stamp to end of working hours. Full day if not work day.
    hours_first_day <-
      day_end - ifelse(work_days_n(started) == 1, timestamp_day(started),day_start)
    # Hours from start of working hours to time stamp. Full day if not work day.
    hours_last_day <-
      ifelse(work_days_n(ended) == 1, timestamp_day(ended), day_end) - day_start

    # All dates in span started to end, indcluding first + last day
    dates_in_span <-
      mapply(dates_span_fun, started, ended, SIMPLIFY = FALSE)
    hours_working_days <- (work_days_n(dates_in_span) - 2) *
      (day_end - day_start)
    # hours_first_day + hours_last_day +
    return(hours_first_day + hours_last_day + hours_working_days)
  }

#' POSIX to duration since mid night
#' @param x a vector of objects of class POSIX time
#' @return object of class duration
#' @keywords internal

time_as_duration <-
  function(x)
    lubridate::as.duration(x - lubridate::floor_date(x, unit = "day")) %>% return

#' Number of working days, atomic
#' Calculates number of work days, currently just by week days mon-fri
#'
#' @param x is a vector of dates in POSIX-format
#' @return a numeric vector of number of working days in period
#' @keywords internal

# Atomic function that returns number of days mon-fri in period.
work_days_atomic <- function (x) {
  (lubridate::wday(x) %in% c(2:6)) %>% sum %>% return
}

#' Number of working days, wrapper
#' Calls \code{work_days_atomic} that calculates number of work days
#'
#' @param x is a list of vector of dates in POSIX-format
#' @return a numeric vector of number of working days in period
#' @keywords internal
work_days_n <-
  function (x)
    lapply(x, work_days_atomic) %>% unlist %>% return

#' Create sequence of days in span
#' Creates a sequence with dates from \code{started} to \code{ended}, indcluding first + last day. Not vectorized.
#'
#' @param started An atomic vector of POSIX time for start time
#' @param ended An atomic vector of POSIX time for end time
#' @return a numeric vector of number of working days in period
#' @keywords internal
dates_span_fun <- function(started, ended)	{
  seq(
    lubridate::floor_date(started, unit = "day"),
    lubridate::floor_date(ended, unit = "day"), by = "days"
  ) %>% return
}
