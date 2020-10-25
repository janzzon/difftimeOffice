# difftimeOffice
Duration within office hours between timestamps.
A package containing one exported function `difftime_office_hours()`, which calculates duration within office hours.  
  
Office hours is default monday to friday, 8:00 to 16:00.
Start and end time of day can be set with argument `working_hours`.
Holidays can be provided to the function with argument `holidays`. (This ensures that any holidays are compatible, no matter the country or office)
 

## Usage
`difftime_office_hours(started, ended, working_hours = c(8, 16))`  
Arguments:  
* `started` Start time of period, as POSIX
* `ended` End time of period, as POSIX
* `working_hours` Vector of length 2, start and end of office day in hours. Default c(8,16)  
* `holidays` Vector of dates in (%m/%d/%Y) format  to calculate as holidays. Default c('01/01/1901', '01/02/1901')


Output is of class Duration from package `lubridate`, units in seconds. 

## Installation
`remotes::install_github("janzzon/difftimeOffice")`
