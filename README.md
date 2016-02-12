# difftimeOffice
Duration within office hours between timestamps.
A package containing one exported function `difftime_office_hours()`, which calculates duration within office hours.  
  
Office hours is default monday to friday, 8:00 to 16:00.
Start and end time of day can be set with argument `working_hours`.
Additional holliday days can probably be added by modifying the internal function `difftimeOffice:::work_days_atomic()`.  

## Usage
`difftime_office_hours(started, ended, working_hours = c(8, 16))`  
Arguments:  
* `started` Start time of period, as POSIX
* `ended` End time of period, as POSIX
* `working_hours` Vector of lenght 2, start and end of office day in hours. Default c(8,16)

## Installation
`devtools::install_github("janzzon/difftimeOffice")`
