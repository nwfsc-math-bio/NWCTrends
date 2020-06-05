#' Create the geomean tables
#'
#' Create the tables with the geomeans for different time periods. Two tables are produced: one of the
#' smoothed wild and total (total in parentheses) spawners. This function is called from esu_report.Rmd
#'  and is not exported. The min.year, max.year, lenbands, nbands, and min.band.points control the
#'  look of the table and can be controlled in the call to NWCTrends_report() by passing in
#'  geomean.table.control as list.  For example `list(min.year=1999)` to change the min year shown
#'  from the minimum in the dataset (the default) to 1999.
#'
#'  The code will create bands with lenbands years in each band starting with min.year. If max.year,
#'  would lead to a final band with less than lenbands years, then the last band will not have lenbands
#'  years. If it has fewer than min.band.points, then the last band will be NA.
#'  You will need to properly choose min.year and max.year to get the table to look as you want.
#'
#'  The last column of the tables is the percent change. This can be over the last 2 bands or the first and last
#'  bands. The change.col argument determines which it is.
#'
#' @param pops which populations to include in the table
#' @param mpg Population group. Shown in the table.
#' @param total.fit The matrix of total spawner estimates
#' @param fracwild.fit The matrix of fraction wild associated with each total row.
#' @param min.year The minumum year to include in the tables.
#' @param max.year The maximum year to include in the tables. If this is 'mid-band', the rest of the band will be padded with NAs and the band width of the last band will be less than lenbands.
#' @param lenbands How many years in each band. Default is 5-years.
#' @param min.band.points The minimum data points for the geomean to show in a band.
#' @param change.col Either between last 2 bands or 1st and last.
#'
#' @return A list with the statesgeomean and rawgeomean data frames (tables).
#'
geomean_table <- function(pops, mpg, total.fit, fracwild.fit, min.year = 1990, max.year = 2014,
                          lenbands = 5, min.band.points = 2, change.col = c("last.two", "first.last")) {
  change.col <- match.arg(change.col)
  n <- length(pops)
  short.pops <- clean.pops(pops)

  # set up the years
  if (min.year > max.year) stop("geomean_tables: min.year cannot be greater than max.year.")
  datyears <- as.numeric(colnames(total.fit$model$data))
  nbands <- ceiling((max.year - min.year + 1) / lenbands)
  # re-define max year as the last year in the last band. This will pad the end with NAs
  max.year.passed.in <- max.year
  max.year <- min.year + nbands * lenbands - 1
  nyr <- length(min.year:max.year)
  min.year.data <- max(datyears[1], min.year)
  max.year.data <- min(max(datyears), max.year)
  data.years <- as.character(min.year.data:max.year.data)

  tabgeomean1 <- tabgeomean2 <- data.frame(population = short.pops, mpg = mpg, matrix("", length(pops), nbands + 1), stringsAsFactors = FALSE)
  geo.start <- 3 # col where geo means start
  for (pop in 1:n) {
    # set up the data
    data <- rep(NA, nyr)
    names(data) <- min.year:max.year
    popname <- pops[pop]

    # Part 1. Get the smoothed and raw WILD geomeans
    state.vals <- raw.vals <- ""
    if (popname %in% rownames(total.fit$model$data)) {
      states <- rep(NA, nyr)
      names(states) <- min.year:max.year
      # smoothed estimates; wild = total*fracwild; log wild = log total + log fracwild
      wild.states <- total.fit$states[paste("X.", popname, sep = ""), ] + log(fracwild.fit$fracwild.states[popname, ])
      names(wild.states) <- colnames(total.fit$model$data)
      # raw ie original data * fracwild
      wild.raw <- total.fit$model$data[popname, ] + log(fracwild.fit$fracwild.raw[popname, ])
      names(wild.raw) <- colnames(total.fit$model$data)
      # data is raw wild
      data[data.years] <- wild.raw[data.years]
      # states is smoothed wild
      states[data.years] <- wild.states[data.years]
      # dat is the states with NAs replaced with 0 so the filter works;
      # normally no NAs since states is smoothed, but user might have set min.year or max.year
      # before or after the data
      dat <- dat0 <- states
      dat[is.na(dat)] <- 0
      statesgeomean <- filter(dat, rep(1, lenbands), sides = 1)
      # this sums up the number of non-zeros in each band
      dat0[!is.na(dat0)] <- 1
      dat0[is.na(dat0)] <- 0
      not0 <- filter(dat0, rep(1, lenbands), sides = 1)
      # require min.band.points datapoints
      not0[not0 <= (min.band.points - 1)] <- NA
      statesgeomean <- round(exp(statesgeomean / not0), digits = 0)

      # dat is the raw data with NAs replaced with 0 so the filter works
      dat <- dat0 <- data
      dat[is.na(dat)] <- 0
      rawgeomean <- filter(dat, rep(1, lenbands), sides = 1)
      # this sums up the number of non-zeros in each band
      dat0[!is.na(dat0)] <- 1
      dat0[is.na(dat0)] <- 0
      not0 <- filter(dat0, rep(1, lenbands), sides = 1)
      # require min.band.points datapoints
      not0[not0 <= (min.band.points - 1)] <- NA
      rawgeomean <- round(exp(rawgeomean / not0), digits = 0)

      # The geomean is a filter. We need every lenbands element. Start from end and work back
      # since the filter is sides=1
      state.vals.numeric <- rev(statesgeomean[seq(length(data), lenbands, -1 * lenbands)])
      if (change.col == "last.two") {
        state.vals.numeric <- c(state.vals.numeric, 100 * (state.vals.numeric[nbands] - state.vals.numeric[nbands - 1]) / state.vals.numeric[nbands - 1])
      }
      if (change.col == "first.last") {
        state.vals.numeric <- c(state.vals.numeric, 100 * (state.vals.numeric[nbands] - state.vals.numeric[1]) / state.vals.numeric[1])
      }

      raw.vals.numeric <- rev(rawgeomean[seq(length(data), lenbands, -1 * lenbands)])
      if (change.col == "last.two") {
        raw.vals.numeric <- c(raw.vals.numeric, 100 * (raw.vals.numeric[nbands] - raw.vals.numeric[nbands - 1]) / raw.vals.numeric[nbands - 1])
      }
      if (change.col == "first.last") {
        raw.vals.numeric <- c(raw.vals.numeric, 100 * (raw.vals.numeric[nbands] - raw.vals.numeric[1]) / raw.vals.numeric[1])
      }
      state.vals <- paste(round(state.vals.numeric, digits = 0), sep = "")
      raw.vals <- paste(round(raw.vals.numeric, digits = 0), sep = "")
      raw.vals[raw.vals == "NA"] <- ""
      raw.vals[raw.vals == "NaN"] <- ""
      state.vals[state.vals == "NA"] <- ""
      state.vals[state.vals == "NaN"] <- ""
    }

    # Part 2. Get the smoothed and raw TOTAL geomeans
    # not sure why there is the if statement re total.fit being null.
    data <- rep(NA, nyr)
    names(data) <- min.year:max.year
    states <- rep(NA, nyr)
    names(states) <- min.year:max.year
    if (!is.null(total.fit)) {
      if (popname %in% rownames(total.fit$model$data)) {
        total.states <- total.fit$states[paste("X.", popname, sep = ""), ]
        names(total.states) <- colnames(total.fit$model$data)
        states[data.years] <- total.states[data.years]
        # dat is the states with NAs replaced with 0 so the filter works;
        # normally no NAs since states is smoothed, but user might have set min.year or max.year
        # before or after the data
        dat <- dat0 <- states
        dat[is.na(dat)] <- 0
        total.statesgeomean <- filter(dat, rep(1, lenbands), sides = 1)
        # this sums up the number of non-zeros in each band
        dat0[!is.na(dat0)] <- 1
        dat0[is.na(dat0)] <- 0
        not0 <- filter(dat0, rep(1, lenbands), sides = 1)
        # require min.band.points datapoints
        not0[not0 <= (min.band.points - 1)] <- NA
        total.statesgeomean <- round(exp(total.statesgeomean / not0), digits = 0)

        # dat is the raw data with NAs replaced with 0 so the filter works
        data[data.years] <- total.fit$model$data[popname, data.years]
        dat <- dat0 <- data
        dat[is.na(dat)] <- 0
        total.rawgeomean <- filter(dat, rep(1, lenbands), sides = 1)
        dat0[!is.na(dat0)] <- 1
        dat0[is.na(dat0)] <- 0
        not0 <- filter(dat0, rep(1, lenbands), sides = 1)
        # require min.band.points datapoints
        not0[not0 <= (min.band.points - 1)] <- NA
        total.rawgeomean <- round(exp(total.rawgeomean / not0), digits = 0)

        # The geomean is a filter. We need every lenbands element. Start from end and work back
        # since the filter is sides=1
        state.vals.numeric <- rev(total.statesgeomean[seq(length(data), lenbands, -1 * lenbands)])
        if (change.col == "last.two") {
          state.vals.numeric <- c(state.vals.numeric, 100 * (state.vals.numeric[nbands] - state.vals.numeric[nbands - 1]) / state.vals.numeric[nbands - 1])
        }
        if (change.col == "first.last") {
          state.vals.numeric <- c(state.vals.numeric, 100 * (state.vals.numeric[nbands] - state.vals.numeric[1]) / state.vals.numeric[1])
        }
        # Code here is confusing
        # state.vals is the WILD geomeans. state.vals.numeric is now the TOTAL geomeans
        # Creating a character vector that is Wild (Total) and renaming state.vals
        state.vals <- paste(state.vals, " (", round(state.vals.numeric, digits = 0), ")", sep = "")

        raw.vals.numeric <- rev(total.rawgeomean[seq(length(data), lenbands, -1 * lenbands)])
        if (change.col == "last.two") {
          raw.vals.numeric <- c(raw.vals.numeric, 100 * (raw.vals.numeric[nbands] - raw.vals.numeric[nbands - 1]) / raw.vals.numeric[nbands - 1])
        }
        if (change.col == "first.last") {
          raw.vals.numeric <- c(raw.vals.numeric, 100 * (raw.vals.numeric[nbands] - raw.vals.numeric[1]) / raw.vals.numeric[1])
        }
        # raw.vals is the raw WILD geomeans. raw.vals.numeric is now the raw TOTAL geomeans
        # Creating a character vector that is Wild (Total) and renaming raw.vals
        raw.vals <- paste(raw.vals, " (", round(raw.vals.numeric, digits = 0), ")", sep = "")
      }
    }
    raw.vals[raw.vals == "NA (NaN)"] <- ""
    raw.vals[raw.vals == "NaN (NaN)"] <- ""
    raw.vals[raw.vals == " (NaN)"] <- ""
    raw.vals[raw.vals == " (NA)"] <- ""
    state.vals[state.vals == "NA (NaN)"] <- ""
    state.vals[state.vals == "NaN (NaN)"] <- ""
    state.vals[state.vals == " (NaN)"] <- ""
    state.vals[state.vals == " (NA)"] <- ""
    tabgeomean1[pop, geo.start:(nbands + geo.start)] <- state.vals # smoothed
    tabgeomean2[pop, geo.start:(nbands + geo.start)] <- raw.vals
  }
  # make the year labels
  start1 <- seq(min.year, max.year.passed.in, lenbands)
  end1 <- seq(min.year + lenbands - 1, max.year.passed.in + lenbands - 1, lenbands)
  end1[length(end1)] <- max.year.passed.in
  yrranges <- paste(start1, end1, sep = "-")
  colnames(tabgeomean1) <- c("Population", "MPG", yrranges, "% Change")
  colnames(tabgeomean2) <- c("Population", "MPG", yrranges, "% Change")
  list(statesgeomean = tabgeomean1, rawgeomean = tabgeomean2)
}
