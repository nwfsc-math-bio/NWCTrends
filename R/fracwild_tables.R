#' @title
#' Make average and geomean fraction wild tables 
#' @description
#' Takes the wild and total data and makes tables of fraction wild via wild/total
#' and constructs averages for given number of years (typically 5 or 10).
#' The wild data might be shorter than total since if there is no fracwild info,
#' the wild data might not have that population. The total data will be subsetted
#' to only have the populations in the wild data.
#' 
#' The function wants the matrices where the rownames are the population names
#' and the colnames are the years. In the package output, these are call
#' total: `matdat.spawners` and wild: `matdat.wildspawners`.
#' @param wild The wild count as a matrix. It is up to the user where this comes from. It could come from the raw fracwild data times raw total data or come from the smoothed frac wild times smoothed total estimates.
#' @param total The total count as a matrix. See above notes on the wild count.
#' @param max.year The last year to use when constructing the bands
#' @param lenbands Number of years to average
#' @param nbands Number of averages to show.
fracwild_table <- function(wild, total, max.year = 2014, lenbands = 5, nbands = 5, type=c("mean", "geomean")) {
  type <- match.arg(type)
  pops <- rownames(wild)
  short.pops <- clean.pops(pops)

  n <- length(pops)
  fracwild <- wild / total[pops, ]

  # set up the years
  datyears <- as.numeric(colnames(wild))
  min.year <- max.year - nbands * lenbands + 1
  nyr <- length(min.year:max.year)
  min.year.data <- max(datyears[1], min.year)
  max.year.data <- min(max(datyears), max.year)
  data.years <- as.character(min.year.data:max.year.data)

  tab1 <- data.frame(population = short.pops, matrix(NA, length(pops), nbands))
  for (pop in 1:n) {
    # set up the data
    data <- rep(NA, nyr)
    names(data) <- min.year:max.year
    data[data.years] <- fracwild[pop, data.years]
    popname <- pops[pop]

    # done this way to be general in case I change the band length and number
    dat <- dat0 <- data
    if(type=="geomean") dat <- dat0 <- log(data)
    dat[is.na(dat)] <- 0
    rawmean <- stats::filter(dat, rep(1, lenbands), sides = 1)
    dat0[!is.na(dat0)] <- 1
    dat0[is.na(dat0)] <- 0
    not0 <- stats::filter(dat0, rep(1, 5), sides = 1)
    rawmean <- rawmean / not0
    if(type=="geomean") rawmean <- exp(rawmean)

    tab1[pop, 2:(nbands + 1)] <-
      rawmean[rev(seq(length(data), lenbands, -1 * lenbands))]
  }
  yrranges <- paste(rev(seq(max.year - lenbands + 1, min.year, -1 * lenbands)),
    rev(seq(max.year, min.year + lenbands - 1, -1 * lenbands)),
    sep = "-"
  )
  colnames(tab1) <- c("Population", yrranges)
  tab1
}
