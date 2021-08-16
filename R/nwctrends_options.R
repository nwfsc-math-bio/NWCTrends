#' @name
#' nwctrends.options
#' @title
#' NWCTrends options and plot variables
#' @description
#' Set up the plot default line widths, types and colors. Pass in as a list, such as `nwctrends.options = list(main.raw.pch = 1)` 
#' with the variables that you wish to set. See details for the names and descriptions of the variables.
#' @details
#' `main.` variables for the main plot with trends for each ESU
#' * `main.title.cex` Size of the main ESU titles in the plot. Default is 1.5.
#' * `main.poptitle.cex` Size of the population titles in the individual panels. Default is 1.
#' * `main.ylabel.cex` Size of the y axis labels in the individual panels. Default is 0.8.
#' * `main.total.lty` Line type for the smoothed total spawners line. Default is 1 (solid).
#' * `main.total.lwd` Line width for the smoothed total spawners line. Default is 3 (thick).
#' * `main.total.col` Line color for the smoothed total spawners line. Default is black.
#' * `main.wild.lty` Line type for the smoothed wild spawners line. Default is 1 (solid).
#' * `main.wild.lwd` Line width for the smoothed wild spawners line. Default is 1 (thin).
#' * `main.wild.col` Line color for the smoothed wild spawners line. Default is red (#D44045) from the NMFS palette.
#' * `main.raw.pch` Point type for the raw spawners data points. Default is 19 (solid circle).
#' * `main.raw.col` Color for the raw spawners data points. Default is blue (#00467F) from the NMFS palette.
#' * `main.ci.col` Color for the confidence polygon around the smoothed total spawners line. Default is "grey75".
#' * `main.ci.border` Border for the confidence polygon around the smoothed total spawners line. Default is NA which is no border. See \code{\link{polygon}()} for the options.
#' * `main.NAtotal.lty` Line type for the smoothed total spawners line  before the first data points. Default is 1 (solid).
#' * `main.NAtotal.lwd` Line width for the smoothed total spawners line  before the first data points. Default is 3 (thick).
#' * `main.NAtotal.col` Line color for the smoothed total spawners line  before the first data points. Default is "grey".
#' * `main.NAci.col` Color for the confidence polygon around the smoothed total spawners line before the first data points. Default is "grey95".
#' * `main.NAci.border` Border for the confidence polygon around the smoothed total spawners line  before the first data points. Default is NA which is no border. See \code{\link{polygon}()} for the options.
#' 
#' `fracwild.` variables for the fracwild plot
#' * `fracwild.title.cex` Size of the main ESU titles in the plot. Default is 1.5.
#' * `fracwild.poptitle.cex` Size of the population titles in the individual panels. Default is 1.
#' * `fracwild.ylabel.cex` Size of the y axis labels in the individual panels. Default is 0.8.
#' * `fracwild.lty` Line type for the smoothed fracwild line. Default is 1 (solid).
#' * `fracwild.lwd` Line width for the smoothed fracwild line. Default is 2 (medium thick).
#' * `fracwild.col` Line color for the smoothed fracwild line. Default is blue (#00467F) from the NMFS palette.
#' * `fracwild.raw.pch` Point type for the fracwild raw data points. Default is 1 (open circle).
#' * `fracwild.raw.col` Color for the fracwild raw data points. Default is black.
#' 
#' `prod.` variables for the productivity plot
#' * `prod.title.cex` Size of the main ESU titles in the plot. Default is 1.5.
#' * `prod.poptitle.cex` Size of the population titles in the individual panels. Default is 1.
#' * `prod.ylabel.cex` Size of the y axis labels in the individual panels. Default is 0.8.
#' * `prod.col.neg` Color of the negative productivity bars. Default is red (#D44045) from the NMFS palette.
#' * `prod.col.pos` Color of the positive productivity bars. Default is green (#007934) from the NMFS palette.
#' 
#' `geomean.` variables for the geomeans plot
#' * `geomean.title.cex` Size of the main titles in the plot. Default is 1.
#' * `geomean.xaxis.cex` Size of the x axis tick labels in the individual panels. Default is 0.9.
#' * `geomean.yaxis.cex` Size of the y axis tick labels in the individual panels. Default is 0.9.
#' * `geomean.pch` Point type for the data points. Default is 19 (solid circle).
#' * `geomean.cex` Point size for the data points. Default is 1.5.
#' * `geomean.col.neg` Color of the negative data points. Default is red (#D44045) from the NMFS palette.
#' * `geomean.col.pos` Color of the positive data points. Default is black.
#' * `geomean.abline.lty` Line type for the horizontal reference lines. Default is 2 (dashed).
#' * `geomean.abline.col` Line color for the horizontal reference lines. Default is "grey".
#' 
#' `trend.` variables for the trends plot showing the x-year (e.g. 15-year) trend values in a plot
#' * `trend.title.cex` Size of the main title in the plot. Default is 1.
#' * `trend.ylabel.cex` Size of the y axis label. Default is 1.
#' * `trend.axis.cex` Size of the axis tick labels. Default is 1.
#' * `trend.pch` Point type for the data points. Default is 19 (solid circle).
#' * `trend.cex` Point size for the data points. Default is 1.5.
#' * `trend.col.neg` Color of the negative data points. Default is red (#D44045) from the NMFS palette.
#' * `trend.col.pos` Color of the positive data points. Default is black.


#' @return
#' Nothing is returned. The variables are set in the internal package environment.
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
#' @seealso `NWCTrends_report()`
#' @keywords utility

NULL