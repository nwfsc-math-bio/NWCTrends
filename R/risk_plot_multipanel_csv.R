#' @name
#' Status_trendfigure_multipanel_csv
#' @title
#' data frame of estimated trends (wild and total spawners)
#' @description
#' This returns a data frame that is written to a csv file. Not exported. It is used by 
#' \code{inst/report_files/esu_report.Rmd}. 
#' 
#' It returns the smoothed total spawners estimate and the 
#' smoothed wild spawners estimate which is 
#' "smoothed total estimate x smoothed fracwild estimate".
#' The wild spawner estimate is only shown from 1 year before and one year after
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
#' @param log.scale Return values on log-scale versus the original raw scale

#' @return
#' A dataframe
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
#' @keywords report
#' @seealso \code{\link{Status_trendfigure_multipanel}}
#' 
Status_trendfigure_multipanel_csv <- function(esu, pops, total.fit, fracwild.fit, log.scale = FALSE) {

  # Set up the min and max years
  years <- as.numeric(colnames(total.fit$model$data))
  min.year <- years[1]
  max.year <- max(years)
  # column where spawner data begins and ends (across all populations)
  first.n.spawner.data <- min(which(apply(total.fit$model$data,2,function(x){!all(is.na(x))})))
  last.n.spawner.data <- max(which(apply(total.fit$model$data,2,function(x){!all(is.na(x))})))
  
  n <- length(pops)
  short.pops <- clean.pops(pops)

  df <- data.frame(
    pop = "NA", years = NA, total.raw = NA, total.smoothed = NA,
    total.smoothed.high = NA, total.smoothed.low = NA,
    wild.smoothed = NA
  )

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
    wild.states <- total.states + log(fracwild.fit$fracwild.states[popname, ])
    wild.raw <- total.raw + log(fracwild.fit$fracwild.raw[popname, ])
    fracwild.raw <- fracwild.fit$fracwild.raw[popname, ]
    if (all(is.na(wild.raw))) {
      wild.states[] <- NA
    } else { 
      # Only hindcast 1 year before last fracwild data
      n.start <- max(min(which(!is.na(fracwild.raw))-1), which(years == min.year))
      if (n.start > 1) wild.states[1:(n.start - 1)] <- NA
      # Only forecast 1 year past last fracwild data
      n.end <- min(max(which(!is.na(fracwild.raw))+1), which(years == max.year))
      if (n.end < length(wild.states)) wild.states[(n.end + 1):length(wild.states)] <- NA
    }
    
    # only show estimates within the data for the ESU
    n.start <- first.n.spawner.data
    n.end <- last.n.spawner.data
    
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

    tmp <- data.frame(
      pop = short.pops[pop], years = years,
      total.raw = total.raw, total.smoothed = total.states, total.smoothed.high = y.high.total, total.smoothed.low = y.low.total, wild.smoothed = wild.states
    )

    df <- rbind(df, tmp)
  }
  df <- df[-1, ]
  df$total.smoothed <- round(df$total.smoothed, digits = 2)
  df$total.smoothed.high <- round(df$total.smoothed.high, digits = 2)
  df$total.smoothed.low <- round(df$total.smoothed.low, digits = 2)
  df$wild.smoothed <- round(df$wild.smoothed, digits = 2)

  return(df)
}
