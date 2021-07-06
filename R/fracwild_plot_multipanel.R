#' @name
#' fracwild_multipanel
#' @title
#' Figure of the raw and smoothed fracwild estimates.
#' @description
#' This is the fracwild figure function. Not exported. It is used by \code{\link{NWCTrends_report}} and \code{inst/doc/report_files/esu_report.Rmd}. 
#' 
#' The dots are the raw fracwild values, the black
#' line is the smoothed fracwild estimate.
#' For the smoothed fracwild estimates, there is no data sharing across populations.
#'
#' @param esu The name of the ESU
#' @param pops The population names that will be plotted (populations with too few data are eliminated)
#' @param total.fit total fit returned by `trend_fits()`
#' @param fracwild.fit fracwild fit returned by `trend_fits()`
#' @param min.year The x axis minimum.
#' @param max.year The x axis maximum.
#' @param show.all If there is no fracwild data for a population, should that population still have a fracwild plot, which will be blank.
#' @return
#' A plot
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
#' @keywords report
#' 
fracwild_multipanel <- function(esu, pops, fracwild.fit, min.year = NULL, max.year = NULL, show.all = TRUE) {

  # Set up the min and max years
  years <- as.numeric(colnames(fracwild.fit$fracwild.states))
  if (is.null(min.year)) min.year <- years[1]
  # if (min.year < years[1]) min.year <- years[1]
  if (is.null(max.year)) max.year <- max(years)
  if (max.year > max(years)) max.year <- max(years)
  
  if(!show.all){
    badrows <- apply(fracwild.fit$fracwild.raw, 1, function(x){ all(is.na(x)) })
    badpops <- names(badrows)[badrows]
    pops <- pops[!(pops %in% badpops)]
  }

  n <- length(pops)
  short.pops <- clean.pops(pops)

  if (n == 1) {
    nplotcol <- 1
  }
  if (n == 2) {
    nplotcol <- 2
  }
  if (n > 2 & n < 5) {
    nplotcol <- 2
  }
  if (n > 4) nplotcol <- 4
  nplotrow <- ceiling(n / nplotcol)
  # the.omi=c(max(0,(6-nplotrow)*1.5)+.25,.5,.5,0)
  the.omi <- c(.25, .5, .5, 0)
  par(mfrow = c(nplotrow, nplotcol), omi = the.omi)

  for (pop in 1:n) {
    # set up the data
    popname <- pops[pop]
    fracwild.states <- fracwild.fit$fracwild.states[popname, ]
    fracwild.raw <- fracwild.fit$fracwild.raw[popname, ]

    if (all(is.na(fracwild.raw)) | show.all) {
      min.year <- min.year
      n.start <- which(years == min.year)
      n.end <- which(years == max.year)
    } else {
      n.start <- max(min(which(!is.na(fracwild.raw))), which(years == min.year))
      n.end <- min(max(which(!is.na(fracwild.raw))), which(years == max.year))
    }



    # trim down the data
    fracwild.states <- fracwild.states[n.start:n.end]
    fracwild.raw <- fracwild.raw[n.start:n.end]
    years.trim <- years[n.start:n.end]

    # plot the data
    par(mar = c(2, 2, 2, 2) + 0.1)
    plot(years.trim, fracwild.raw,
      type = "n", bty = "L", xlab = "", ylab = "",
      ylim = c(0, 1), xlim = c(min.year - 1, max.year + 1)
    )
    lines(years.trim, fracwild.states, col = "blue", lwd = 2)
    # if(!all((total.raw-wild.raw)>0.999, na.rm=TRUE))

    points(years.trim, fracwild.raw)

    title(short.pops[pop], cex.main = 1)
  }
  mtext(esu, side = 3, outer = TRUE, line = 0, cex = 1.5)
  mtext("Raw and smoothed fracwild estimates", side = 2, outer = TRUE, line = 0, cex = .8)
}
