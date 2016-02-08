# based on: http://stackoverflow.com/questions/34068109/how-to-extract-business-hours-between-two-different-times-values-in-r/

library(lubridate)
library(magrittr)
library(dplyr)

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


# Set timestamp to be within working hours. all values as duration
timestamp_day <- function(timestamp){
		timestamp %>% time_as_duration %>% max(day_start) %>% min(day_end) %>% as.duration
}


	# Input is a vector of dates in POSIX-format
	# Calculates number of work days, currently just by week days mon-fri
work_days_n <- function (x){
	work_days_logical <- wday(x) %in% c(2:6)
	work_days_logical %>% sum %>% return
}

work_days_n(dates_in_span)


time_as_duration <- function(x)  as.duration(x - floor_date(x, unit = "day"))

############################### da function ###############

difftime_office_hours <-
	function(started, ended, working_hours = c(8, 16)) {
		# When does working hours start at day? decimal hours
		day_start <-
			duration(working_hours[1], units = "hours")
		# When does working hours end at day? decimal hours
		day_end <-
			duration(working_hours[2], units = "hours")

		# Hours from start time stamp to end of working hours. Full day if not work day.
		hours_first_day <-
			day_end - ifelse(work_days_n(started) == 1,timestamp_day(started),day_start)
		# Hours from start of working hours to time stamp. Full day if not work day.
		hours_last_day <-
			ifelse(work_days_n(ended) == 1, timestamp_day(ended), day_end) - day_start
		# cat(paste("Hours first day", hours_first_day,"\nHours last day",hours_last_day))

		# All dates in span started to end, indcluding first + last day
		dates_in_span <-
			seq(floor_date(started, unit = "day"), floor_date(ended, unit = "day"), by = "days")

		# Hours for working days in span, start + stop day is excluded.
		hours_working_days <- (work_days_n(dates_in_span) -2 ) *
			(day_end - day_start)
		# hours_first_day + hours_last_day +
		return(hours_first_day + hours_last_day + hours_working_days )

	}
difftime_office_hours(Sys.time()+ddays(-1),Sys.time()+dhours(-1))
difftime_office_hours(Sys.time()-92000,Sys.time())



started <- Sys.time()-92000
ended <- Sys.time()-90000

Sys.time()- 90000

working_hours <- c(8, 16)

day_start <- duration(working_hours[1], units = "hours") # When does working hours start at day? decimal hours
day_end <- duration(working_hours[2], units = "hours") # When does working hours end at day? decimal hours

# starttime_day <- function(starttime) max(time_as_duration(starttime), day_start)
# endtime_day <- function(endtime) max(time_as_duration(endtime), day_end)

# timestamp_day <- function(timestamp) min(max(time_as_duration(timestamp), day_start), day_end)

timestamp_day(tid)

hours_first_day <- day_end - timestamp_day(tid)
hours_last_day <- timestamp_day(tid) - day_start


