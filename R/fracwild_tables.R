fracwild_table <- function(wild, total, min.year = NULL, max.year = 2014) {
  # this wants the matrices (total=matdat.spawners and wild=matdat.wildspawners)
  lenbands <- 5
  nbands <- 5

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
    dat[is.na(dat)] <- 0
    rawmean <- filter(dat, rep(1, lenbands), sides = 1)
    dat0[!is.na(dat0)] <- 1
    dat0[is.na(dat0)] <- 0
    not0 <- filter(dat0, rep(1, 5), sides = 1)
    rawmean <- rawmean / not0

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
