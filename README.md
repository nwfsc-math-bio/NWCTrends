# NWCTrends

This is runs the standardized trends metrics used in the 2015 5-year Status Review Update for listed PNW salmonids.  

To install, install the **devtools** package from CRAN and then use:
```
library(devtools)
devtools::install_github("nwfsc-timeseries/NWCTrends")
```

To run a demo analysis and create a report, type 
```
library(NWCTrends)
NWCTrends_report()
```

Type `?NWCTrends` for instructions for analyzing a data set. The data must be .csv or .xls (eventually) or RData from prior fit. Figures will be saved in the **doc/figures/ESU_figures** folder.

The **doc/tex** files folder are some wrappers that are used to make the tables that go into the ESU reports.  Don't delete.  They call the tex files made by create_reports.R for the tables.

The file `esu_report.Rmd` is the file that makes the report.  Don't delete.

## References

The 2015 Status Review report can be viewed by typing
```
RShowDoc("2015 Status Review Update",package="NWCTrends")
```

A pdf of the methods alone is also available by typing
```
RShowDoc("Methods",package="NWCTrends")
```
at the command line. 

## Example output

![Trend estimates](doc/figures/ESU_figures/Puget Sound Steelhead DPS.html)

