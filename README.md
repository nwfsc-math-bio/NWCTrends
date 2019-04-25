# NWCTrends

This is runs the standardized trends metrics used in the 2015 5-year Status Review Update for listed PNW salmonids.  

## Installation

To install, install the **devtools** package (if needed) from CRAN and then use:
```
library(devtools)
devtools::install_github("nwfsc-timeseries/NWCTrends")
```

The default is to create an html file with the tables and figures. You can also select Word or PDF output. In order to create PDF files and tables, you also need to install **tinytex** if you do not have a TeX installation. Code to install **tinytex** is below.  

```
install.packages('tinytex')
tinytex::install_tinytex()
```

## Instructions to run a demo

To download a set of demo files, run the following code.
```
fpath <- system.file("doc","demodata",package="NWCTrends")
file.copy(fpath, ".",recursive=TRUE)
```
This will create a folder called `demodata` in your directory.

To run a demo analysis and create a report, type 
```
library(NWCTrends)
NWCTrends_report()
```
You will be asked to select a data file. If you do not have data, navigate to one of the .csv or .RData files in the `demodata` folder.

Type `?NWCTrends` for instructions for analyzing a data set. The data must be .csv file. Figures will be saved in the **NWCTrends_output** folder, created in your working directory.  

## Instructions to run your own data

Download the demo data files and duplicate the format. Do not rename the columns. Missing data are entered with a -99. An ESU name and a unique population name (COMMON_POPULATION_NAME) is required. The tables produced by **NWCTrends** only use the BROOD_YEAR, NUMBER_OF_SPAWNERS, and FRACWILD columns. An entry is required for each year. RUN_TIME, SPECIES and MAJOR_POPULATION_GROUP are used to adding labels to plots and tables.

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

