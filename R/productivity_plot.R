#' @name
#' productivity_plot
#' @title
#' Productivity plot
#' @description
#' This uses the smoothed spawner estimates and smoothed fracwild estimates to compute a
#' productivity metric. Type 3: `wild(t+1)/wild(t)`. Type 1: `wild(t+lag)/total(t)`, where `wild`
#' is smoothed total estimate times smoothed fracwild estimate and `total` is the smoothed total 
#' estimate.
#' 
#' In the Viability Report, `type=1` and the lag is set to 3 or 4 (depending on species).
#'
#' @param esu The name of the ESU
#' @param pops The population names that will be plotted.
#' @param total.fit total fit returned by `trend_fits()`
#' @param fracwild.fit fracwild fit returned by `trend_fits()`
#' @param min.year The x axis minimum. First year for numerator.
#' @param max.year The x axis maximum. Last year for numerator.
#' @param type The type of plot. Type 3: wild(t+1)/wild(t). Type 1: wild(t+lag)/total(t)
#' @param lag The number of years prior to use in the denominator, e.g. spawnwers(year-lag). Note not used if type=3.
#' @return
#' A plot
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
#' @keywords report
#' 
productivity_plot <- function(esu, pops, total.fit, fracwild.fit, min.year = NULL, max.year = NULL, type = 1, lag = 4) {
  # Set up the min and max years
  years <- as.numeric(colnames(total.fit$model$data))
  if (is.null(min.year)) min.year <- years[1] + lag
  if (min.year < (years[1]+lag)) min.year <- years[1] + lag
  if (is.null(max.year)) max.year <- max(years)
  if (max.year > max(years)) max.year <- max(years)
  n.start <- which(years == min.year)
  n.end <- which(years == max.year)
  t <- n.start:n.end

  if ((min.year - lag) < years[1] & type == 1) {
    stop(paste0("Type=1 uses the min.year - ", lag, " (", min.year - lag, ") data point and your data do not include that."))
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
  if (n > 4) {
    nplotcol <- 3
    par(mfrow = c(ceiling(n / nplotcol), 4), oma = c(0, 2, 2, 0))
  }
  nplotrow <- ceiling(n / nplotcol)
  # the.omi=c(max(0,(6-nplotrow)*1.5)+.25,.5,.5,0)
  the.omi <- c(.25, .5, .5, 0)
  par(mfrow = c(nplotrow, nplotcol), omi = the.omi)

  ylims <- c(-1, 1)

  for (pop in 1:n) {
    # set up the data
    popname <- pops[pop]
    statename <- paste("X.", popname, sep = "")
    pop.n <- which(rownames(total.fit$model$data) == popname)

    total.states <- total.fit$states[statename, ]
    total.raw <- total.fit$model$data[popname, ] # wild spawners
    # total.states is logged as is wild.states
    wild.states <- total.states + log(fracwild.fit$fracwild.states[popname, ])
    wild.raw <- total.raw + log(fracwild.fit$fracwild.raw[popname, ])


    n.start.data <- max(min(which(!is.na(wild.raw))), which(years == min.year))
    n.end.data <- min(max(which(!is.na(wild.raw))), which(years == max.year))

    par(mar = c(2, 2, 2, 2) + 0.1)

    if (type == 3) {
      ylims <- c(-1, 1)

      #### Productivity Test Plot #2;  wild(t+1)/wild(t)
      vals <- wild.states[t] - wild.states[t - 1]
      if (n.start.data != n.start) vals[1:(n.start.data - n.start)] <- NA
      if (n.end.data != n.end) vals[(length(t) - (n.end - n.end.data) + 1):(length(t))] <- NA

      barplot(vals,
        col = ifelse(vals < 0, nwctrends.palette$red, nwctrends.palette$green),
        ylab = "", xlab = "", ylim = ylims
      )
      par(new = TRUE)

      vals <- wild.states[t]
      if (n.start.data != n.start) vals[1:(n.start.data - n.start)] <- NA
      if (n.end.data != n.end) vals[(length(t) - (n.end - n.end.data) + 1):(length(t))] <- NA

      # make the labels so that year-lag is what appears instead of year[t]
      plot(years[t], vals, axes = FALSE, type = "l", lwd = 2, ylab = "", xlab = "", col = "grey")
      axis(side = 4)
      axis(side = 1)
    }
    if (type == 1) {

      #### Productivity Test Plot #2;  wild spawners(t+lag)/spawners(t)
      vals <- wild.states[t] - total.states[t - lag]
      if (n.start.data != n.start) vals[1:(n.start.data - n.start)] <- NA
      if (n.end.data != n.end) vals[(length(t) - (n.end - n.end.data) + 1):(length(t))] <- NA
      ylims <- c(min(vals - 1, -2, na.rm = TRUE), max(vals + 1, 2, na.rm = TRUE))
      # vals[i] is log(year[i])-log(year[i-lag])
      # but I want vals[i] plotted at [i-lag] not i
      # and I want the xlims to be bigger
      year1 <- (min.year) - ((min.year) %% 5) - 5 # this works if lag is no bigger than 5
      year2 <- ifelse((max.year) %% 5 == 0, max.year, max.year + 5 - (max.year + 5) %% 5)
      xlim1 <- year1 - min.year + lag + 1
      xlim2 <- length(year1:year2) - 1 + xlim1
      xlims <- c(xlim1, xlim2)
      x <- barplot(vals,
        col = ifelse(vals < 0, nwctrends.palette$red, nwctrends.palette$green),
        ylab = "", xlab = "", ylim = ylims, xlim = xlims,
        axisnames = FALSE, width = 1, space = 0
      )
      axis(side = 1, at = seq(xlims[1], xlims[2], 5) - .5, labels = seq(year1, year2, 5))
    }

    title(short.pops[pop], cex.main = 1)
  }
  mtext(esu, side = 3, outer = TRUE, line = 0, cex = 1.5)
  if (type == 1) {
    mtext(paste("log(wild spawner t+", lag, ") - log(total spawner t)", sep = ""), side = 2, outer = TRUE, line = 0, cex = .8)
  }
  if (type == 2) {
    mtext(expression(lambda(t)), side = 2, outer = TRUE, line = 0, cex = .8)
    mtext("smoothed log wild spawners", side = 4, outer = TRUE, line = 0, cex = .8)
  }
  if (type == 3) {
    mtext("log(wild spawner t+1) - log(wild spawner t)", side = 2, outer = TRUE, line = 0, cex = .8)
  }
}
