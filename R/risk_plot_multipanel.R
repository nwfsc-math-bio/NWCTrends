#' @name
#' Status_trendfigure_multipanel
#' @title
#' Main figure of estimated trends (wild and total spawners)
#' @description
#' This is the main figure function. Not exported. It is used by \code{\link{NWCTrends_report}} and \code{inst/report_files/esu_report.Rmd}. 
#' 
#' The dots are the raw spawner counts, the black
#' line is the smoothed total spawners estimate, and the red line is the 
#' smoothed wild spawners estimate which is 
#' "smoothed total estimate x smoothed fracwild estimate".
#' Note that the wild spawner estimate is only shown from 1 year before and one year after
#' the last actual fracwild estimate (in the data file). This is done so that
#' the wild estimate does not over-extend the fracwild data. Fracwild estimates can
#' be interpolated for missing years, but would not be appropriate to extend much before
#' or past actual observed (or expert) fracwild data.
#' 
#' For the smoothed total estimates, information from all populations (via a non-diagonal
#' year-to-year variance matrix) is used to estimate missing values and to account for
#' observation error in the total spawner count. Because data from all populations are used,
#' estimates can be made even for missing years at the beginning of the time series if there
#' is data for those early years in other populations.
#'
#' @param esu The name of the ESU
#' @param pops The population names that will be plotted (populations with too few data are eliminated)
#' @param total.fit total fit returned by `trend_fits()`
#' @param fracwild.fit fracwild fit returned by `trend_fits()`
#' @param plot.min.year The x axis minimum.
#' @param plot.max.year The x axis maximum.
#' @param silent No output
#' @param CI.method Method sent to \code{\link[MARSS]{MARSSparamCIs}}
#' @param CI.sim If doing bootstrap CI, this is the number of bootstraps sent to \link[MARSS]{MARSSparamCIs}
#' @param log.scale Put plot on log-scale versus the original raw scale
#' @param same.scale Tweak the scale of wild and total in graph. Not used.
#' @param nwctrends.options A list of plot options to change the appearance (colors, line types, line widths, point types, etc)
#' in the plots. See \code{\link{nwctrends.options}} for a description of the options. Note, if `risk_plot_multipanel()` is 
#' called from \code{\link{NWCTrends_report}()} then `nwctrends.options` has already been set and can be left at `NULL` in this call.
#' 
#' @return
#' A plot
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
#' @keywords report
#' @seealso \code{\link{Status_trendfigure_multipanel_csv}}, \code{\link{NWCTrends_report}()}
#' 
Status_trendfigure_multipanel <- function(esu, pops, 
                                          total.fit, fracwild.fit, 
                                          plot.min.year = NULL, plot.max.year = NULL, 
                                          silent = FALSE, CI.method = "hessian", CI.sim = 1000, 
                                          log.scale = FALSE, same.scale = FALSE,
                                          nwctrends.options = NULL) {
  if (!(CI.method %in% c("hessian", "parametric", "innovations", "none"))) {
    stop("Stopped in CSEGriskfigure because allowed CI methods are none, hessian, parametric, and innovations.\n", call. = FALSE)
  }
  
  # Set-up the package globals (for plotting); Normally this has already been done via the NWCTrends_report() call
  nwctrends.options.user <- nwctrends.options
  nwctrends.options <- get("nwctrends.options", envir = pkg_globals) # the defaults
  if(!is.null(nwctrends.options.user)){
    if(!is.list(nwctrends.options.user)) stop("nwctrends.options must be a list.")
    for(i in names(nwctrends.options.user)[names(nwctrends.options.user) %in% names(nwctrends.options)]){
      nwctrends.options[[i]] <- nwctrends.options.user[[i]]
    }
    # nwctrends.options now has any values that the user passed in set.
    # So assign that back to pkg_globals for use in plotting functions
    assign("nwctrends.options", nwctrends.options, pkg_globals)
  }
  
  # Set up the min and max years
  years <- as.numeric(colnames(total.fit$model$data))
  # These are min and max for the fits.
  min.year <- years[1]
  max.year <- max(years)
  # column where spawner data begins and ends (across all populations)
  first.n.spawner.data <- min(which(apply(total.fit$model$data,2,function(x){!all(is.na(x))})))
  last.n.spawner.data <- max(which(apply(total.fit$model$data,2,function(x){!all(is.na(x))})))
  # These are min and max for the plots
  if(is.null(plot.min.year)) plot.min.year <- min.year
  if(is.null(plot.max.year)) plot.max.year <- max.year
  
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

  ylims <- c(
    .9 * min(0, total.fit$model$data, na.rm = TRUE),
    1.1 * max(0.1, total.fit$model$data, na.rm = TRUE)
  )
  if (!log.scale) ylims <- exp(ylims)

  for (pop in 1:n) {
    # set up the data
    popname <- pops[pop]
    total.states <- total.raw <- y.high.total <- y.low.total <- rep(NA, length(years))
    if (popname %in% rownames(total.fit$model$data)) {
      total.states <- total.fit$states[paste("X.", popname, sep = ""), ]
      total.raw <- total.fit$model$data[popname, ]
      y.high.total <- total.states + 1.96 * total.fit$states.se[paste("X.", popname, sep = ""), ]
      y.low.total <- total.states - 1.96 * total.fit$states.se[paste("X.", popname, sep = ""), ]
    }
    # total.states (and total.raw) are logged so total*fracwild = total.states + log(fracwild)
    wild.states <- total.states + log(fracwild.fit$fracwild.states[popname, ])
    wild.raw <- total.raw + log(fracwild.fit$fracwild.raw[popname, ])
    fracwild.raw <- fracwild.fit$fracwild.raw[popname, ]
    if (all(is.na(wild.raw))) {
      wild.states[] <- NA
    } else {
      # Put in NAs for wild.states where there is not fracwild data
      n.start <- max(min(which(!is.na(fracwild.raw))-1), which(years == min.year))
      if (n.start > 1) wild.states[1:(n.start - 1)] <- NA
      n.end <- min(max(which(!is.na(fracwild.raw))+1), which(years == max.year))
      if (n.end < length(wild.states)) wild.states[(n.end + 1):length(wild.states)] <- NA
    }

    if (all(is.na(total.raw))) { # no data for this population, skip
      next
    }
    n.start <- max(first.n.spawner.data, which(years==plot.min.year), 1)
    n.end <- min(last.n.spawner.data, which(years==plot.max.year), length(years))
    total.raw[years<plot.min.year] <- NA
    first.n.spawner.data.pop <- min(which(!is.na(total.raw)))
    last.n.spawner.data.pop <- max(which(!is.na(total.raw)))
    
   # Plot light the full data range
    # trim down the data
    wild.raw.trim <- wild.raw[n.start:n.end]
    total.raw.trim <- total.raw[n.start:n.end]
    total.states.trim <- total.states[n.start:n.end]
    y.high.total.trim <- y.high.total[n.start:n.end]
    y.low.total.trim <- y.low.total[n.start:n.end]
    years.trim <- years[n.start:n.end]
    if (!log.scale) {
      wild.raw.trim <- exp(wild.raw.trim)
      total.raw.trim <- exp(total.raw.trim)
      total.states.trim <- exp(total.states.trim)
      y.high.total.trim <- exp(y.high.total.trim)
      y.low.total.trim <- exp(y.low.total.trim)
    }
    if (!same.scale) {
      ylims <- c(
        0.9 * min(0, wild.raw.trim, total.raw.trim, y.low.total.trim, na.rm = TRUE),
        1.1 * max(.1, wild.raw.trim, total.raw.trim, y.high.total.trim, na.rm = TRUE)
      )
    }
    
    # plot the data
    par(mar = c(2, 2, 2, 2) + 0.1)
    plot(years.trim, total.raw.trim,
         type = "n", bty = "L", xlab = "", ylab = "",
         ylim = ylims,
         xlim = c(plot.min.year - 1, plot.max.year + 1)
    )
    polygon(c(years.trim, rev(years.trim)), c(y.high.total.trim, rev(y.low.total.trim)), 
            col = nwctrends.options$main.NAci.col, border = nwctrends.options$main.NAci.border)
    lines(years.trim, total.states.trim, 
          col = nwctrends.options$main.NAtotal.col, 
          lwd = nwctrends.options$main.NAtotal.lwd,
          lty = nwctrends.options$main.NAtotal.lty)
    
    # Plot dark the range -1 and +1 within the spawner counts
    n.start <- max(first.n.spawner.data.pop-1, which(years==plot.min.year), 1)
    n.end <- min(last.n.spawner.data.pop+1, which(years==plot.max.year), length(years))
    
    # trim down the data
   wild.raw <- wild.raw[n.start:n.end]
   total.raw <- total.raw[n.start:n.end]
   wild.states <- wild.states[n.start:n.end]
   total.states <- total.states[n.start:n.end]
   y.high.total <- y.high.total[n.start:n.end]
   y.low.total <- y.low.total[n.start:n.end]
   years.trim <- years[n.start:n.end]
    if (!log.scale) {
      wild.raw <- exp(wild.raw)
      total.raw <- exp(total.raw)
      total.states <- exp(total.states)
      wild.states <- exp(wild.states)
      y.high.total <- exp(y.high.total)
      y.low.total <- exp(y.low.total)
    }
    polygon(c(years.trim, rev(years.trim), years.trim[1]), c(y.high.total, rev(y.low.total), y.high.total[1]), 
            col = nwctrends.options$main.ci.col, border = nwctrends.options$main.ci.border)
    lines(years.trim, total.states, col = nwctrends.options$main.total.col, 
          lwd = nwctrends.options$main.total.lwd,
          lty = nwctrends.options$main.total.lty)
    points(years.trim, total.raw, pch = nwctrends.options$main.raw.pch, col = nwctrends.options$main.raw.col)
    lines(years.trim, wild.states, col = nwctrends.options$main.wild.col, 
          lwd = nwctrends.options$main.wild.lwd,
          lty = nwctrends.options$main.wild.lty)
    title(short.pops[pop], cex.main = nwctrends.options$main.poptitle.cex)
  }
  mtext(esu, side = 3, outer = TRUE, line = 0, cex = nwctrends.options$main.title.cex)
  if (log.scale) {
    mtext(paste("Predicted log abudance and 95% CIs", sep = ""), side = 2, outer = TRUE, line = 0, 
          cex = nwctrends.options$main.ylabel.cex)
  } else {
    mtext(paste("Predicted abundance and 95% CIs", sep = ""), side = 2, outer = TRUE, line = 0, 
          cex = nwctrends.options$main.ylabel.cex)
  }
}
