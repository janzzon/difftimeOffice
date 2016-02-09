# based on: http://stackoverflow.com/questions/34068109/how-to-extract-business-hours-between-two-different-times-values-in-r/
#' @author Stefan Jansson, \email{janzzon@@gmail.com}
#' @title
#' Calculate number of office hours between time stamps
#'
#' @description
#' \code{difftime_office_hours} Calculate number of office hours between time stamps. Office hours is default mon - fri, 8:00 to 16:00
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
difftime_office_hours <-
	function(started, ended, working_hours = c(8, 16)) {
	  # Assert input values is.time
	  assertthat::assert_that(assertthat::is.time(started))
	  assertthat::assert_that(assertthat::is.time(ended))
	  # When does working hours start at day? decimal hours
	  day_start <-
	    lubridate::duration(working_hours[1], units = "hours")
	  # When does working hours end at day? decimal hours
	  day_end <-
	    lubridate::duration(working_hours[2], units = "hours")

	   # Set timestamp to be within working hours. all values as duration
	  timestamp_day <- function(timestamp) {
	    timestamp %>% time_as_duration %>% max(day_start) %>%
	      min(day_end) %>%  lubridate::as.duration(.)
	  }

	  # Hours from start time stamp to end of working hours. Full day if not work day.
	  hours_first_day <-
	    day_end - ifelse(work_days_n(started) == 1, timestamp_day(started),day_start)
	  # Hours from start of working hours to time stamp. Full day if not work day.
	  hours_last_day <-
	    ifelse(work_days_n(ended) == 1, timestamp_day(ended), day_end) - day_start
	  # cat(paste("Hours first day", hours_first_day,"\nHours last day",hours_last_day))

		# All dates in span started to end, indcluding first + last day
		dates_in_span <-
			seq(lubridate::floor_date(started, unit = "day"), lubridate::floor_date(ended, unit = "day"), by = "days")

		# Hours for working days in span, start + stop day is excluded.
		hours_working_days <- (work_days_n(dates_in_span) -2 ) *
			(day_end - day_start)
		# hours_first_day + hours_last_day +
		return(hours_first_day + hours_last_day + hours_working_days )

	}

#' @title POSIX to duration since mid night
# #' @name Transform a POSIX time to duration since mid night.
#' @param x a vector of objects of class POSIX time
#' @return object of class duration
# #' @rdname time_as_duration
#' @rdname internal
#' @keywords internal

time_as_duration <- function(x)  lubridate::as.duration(x - lubridate::floor_date(x, unit = "day"))

#' Number of working days
#' Calculates number of work days, currently just by week days mon-fri
#'
#' @param x is a vector of dates in POSIX-format
#' @return a numeric vector of number of working days in period
# #' @rdname work_days_n
#' @rdname internal
#' @keywords internal
work_days_n <- function (x) {
  work_days_logical <- lubridate::wday(x) %in% c(2:6)
  work_days_logical %>% sum %>% return
}


# #' Pipe operator
# #'
# #' @name %>%
# # #' @rdname pipe
# #' @rdname internal
# #' @keywords internal
# # #' @export
# #' @importFrom magrittr %>%
# #' @usage lhs \%>\% rhs
# NULL

#### slask ####

# difftime_office_hours(Sys.time()+ddays(-1),Sys.time()+dhours(-1))
# difftime_office_hours(Sys.time()-92000,Sys.time())
#
#
#
# started <- Sys.time()-92000
# ended <- Sys.time()-90000
#
# Sys.time()- 90000
#
# working_hours <- c(8, 16)
#
# day_start <- duration(working_hours[1], units = "hours") # When does working hours start at day? decimal hours
# day_end <- duration(working_hours[2], units = "hours") # When does working hours end at day? decimal hours
#
# # starttime_day <- function(starttime) max(time_as_duration(starttime), day_start)
# # endtime_day <- function(endtime) max(time_as_duration(endtime), day_end)
#
# # timestamp_day <- function(timestamp) min(max(time_as_duration(timestamp), day_start), day_end)
#
# timestamp_day(tid)
#
# hours_first_day <- day_end - timestamp_day(tid)
# hours_last_day <- timestamp_day(tid) - day_start
#
#

# vignette("lubridate")
# ddays(1)
# dyears(1)
# Sys.Date() %>% dput
# Sys.time() %>% dput
# tid <- Sys.time() + months(-1)
# tid %>% as.Date()
# as.Date(tid) : as.Date(Sys.time())
# floor_date(tid, unit = "day") %>% dput
# just_nu <- Sys.time()
# just_nu - floor_date(just_nu, unit = "day")
# nu_som_duration <- time_as_duration(just_nu)

#
#
# dates_in_span <- floor_date(seq(tid, Sys.time(), by = "days"), "day")
# floor_date(seq(Sys.time()-92000, Sys.time()-92000, by = "days"), "day")
#
# wday(dates_in_span) %in% c(2:6)
# str(dates_in_span)
