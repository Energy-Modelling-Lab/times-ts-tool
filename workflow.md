# Workflow of the time_ts_tool

>date: 2020-11-05
>
>author: [till@energymodellinglab.com](mailto:till@energymodellinglab.com)

## Description
This document describes the workflow of the time_ts_tool. It states the deployed R scripts in a consecutive manner. For each of the script sourced dependency scripts, loaded data, fundamental processing operations and returned objects are described.

## Workflow

### subannual.R

*Main script to execute.*

- sources:
```
source("funcs.R")
source("read_funcs.R") # not necessary here, initially sourced in main_dict.R
source("ts_data.R")
source("timeslices.R")
source("main_dict.R")
source("transform.R")
```
- sets:
	- file locations
	- year
	- calls `"fetch_timeseries()"` defined in `"ts_data.R"`

#### ts_data.R

*Holds only the function `"fetch_timeseries()"` that loads and merges all the time series data.*

- sources:
```
source("calc_funcs.R")
source("read_funcs.R") # unused
```
- loads:
	- heat demand (Ramses)
	- wind data (Ramses)
	- prices (Energinet)
	- power demand (Energinet)
	- soil temperature (DMI)
	- air temperature (DMI)
	- other data
- defines constants for calculating COP
- calls `"HP_COP()"` defined in `"calc_funcs.R"`
- gets heat pump coefficients back
- calulates heat savings based on Ramses data
- combines returns all timeseries to `"ts_data"`

### subannual.R - continued

- selects timesereis to base categorisation on, stored in `"aggr_data"`
- 
