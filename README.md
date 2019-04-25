# NWCTrends

This is runs the standardized trends metrics used in the 2015 5-year Status Review Update for listed PNW salmonids.  

## Installation

To install, install the **devtools** package from CRAN and then use:
```
library(devtools)
devtools::install_github("nwfsc-timeseries/NWCTrends")
```

If you do not have a TeX installation on your machine, then you also need to install *tinytex* with this code in order to create pdf files and tables. The default is to create a png of the figures and an html file with the tables.

```
install.packages('tinytex')
tinytex::install_tinytex()
```

## Instructions to run a demo

Download a set of demo files, run the following code.
```
fpath <- system.file("doc","demodata",package="NWCTrends")
file.copy(fpath, ".",recursive=TRUE)
```
This will create a folder called `demofiles` in your directory.

To run a demo analysis and create a report, type 
```
library(NWCTrends)
NWCTrends_report()
```
You will be asked to select a data file. If you do not have data, navigate to one of the .csv or .RData files in the `demofiles` folder.

Type `?NWCTrends` for instructions for analyzing a data set. The data must be .csv file. Figures will be saved in the **NWCTrends_output** folder.  The default output is a html file. You can also select Word and PDF.

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

A report will be generated with figures and tables. [Example report](inst/doc/example.html)

The main figure shows the estimated trends.

![](inst/doc/main_fig.jpg)

