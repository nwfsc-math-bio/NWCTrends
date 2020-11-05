#' Create the trend tables
#'
#' Create the tables with the trends for different time periods using the smoothed wild
#' spawner estimates. Despite the name of the function, the range of years need not be 15 years.
#' The years to show are specified
#' by list `year.ranges`. The ranges are specified as `begin.year:end.year`, for example
#' `1990:2005`. `year.ranges` can be padded into the NWCTrends_report() call by passing in
#'  trend.table.control as list.  For example `list(year.ranges=list(1990:2000,2000:2010))`.
#'
#' @param pops which populations to include in the table
#' @param mpg Population group. Shown in the table.
#' @param total.fit The matrix of total spawner estimates
#' @param fracwild.fit The matrix of fraction wild associated with each total row.
#' @param year.ranges The columns of years.
#' @param wild Show smoothed wild or smoothed total.
#' @param navalue Value to use for NAs in the table. Default is a blank.
#'
#' @return A data frames with the estimates trend for each year range in a different column.
#'
trend_15_table <- function(pops, mpg, total.fit, fracwild.fit, year.ranges = list(1990:2005, 1999:2014), wild=TRUE, navalue=" ") {
  n <- length(pops)
  short.pops <- clean.pops(pops)
  
  # set up the years
  datyears <- as.numeric(colnames(total.fit$model$data))
  min.year <- min(unlist(year.ranges))
  max.year <- max(unlist(year.ranges))
  
  nyr <- length(min.year:max.year)
  min.year.data <- max(datyears[1], min.year)
  max.year.data <- min(max(datyears), max.year)
  data.years <- as.character(min.year.data:max.year.data)
  
  tabtrend <- data.frame(population = short.pops, mpg = mpg, matrix(NA, length(pops), length(year.ranges)))
  for (pop in 1:n) {
    # set up the data
    popname <- pops[pop]
    
    data <- rep(NA, nyr)
    names(data) <- min.year:max.year
    states <- rep(NA, nyr)
    names(states) <- min.year:max.year
    if(wild){
      wild.states <- total.fit$states[paste("X.", popname, sep = ""), ] + log(fracwild.fit$fracwild.states[popname, ])
      names(wild.states) <- colnames(total.fit$model$data)
      wild.raw <- total.fit$model$data[popname, ] + log(fracwild.fit$fracwild.raw[popname, ])
      names(wild.raw) <- colnames(total.fit$model$data)
      
      data[data.years] <- wild.raw[data.years]
      states[data.years] <- wild.states[data.years]
    }else{
      total.states <- total.fit$states[paste("X.", popname, sep = ""), ]
      names(total.states) <- colnames(total.fit$model$data)
      total.raw <- total.fit$model$data[popname, ]
      names(total.raw) <- colnames(total.fit$model$data)
      
      data[data.years] <- total.raw[data.years]
      states[data.years] <- total.states[data.years]
    }
    
    for (i in 1:length(year.ranges)) {
      years <- year.ranges[[i]]
      tmp.raw <- data[as.character(years)]
      tmp <- states[as.character(years)]
      if (all(is.na(tmp)) | !all(years %in% names(states))) {
        trend <- NA
        tabtrend[pop, (i + 2)] <- navalue
      } else {
        tmp.lm <- lm(tmp ~ years)
        trend <- coef(tmp.lm)[2]
        tabtrend[pop, (i + 2)] <- paste(round(trend, digits = 2), " (",
                                        round(confint(tmp.lm)[2, 1], digits = 2), ", ",
                                        round(confint(tmp.lm)[2, 2], digits = 2), ")",
                                        sep = ""
        )
      }
      if (length(tmp) > 4 && (sum(!is.na(tmp.raw[1:5])) < 2 | sum(!is.na(tmp.raw[(length(tmp) - 4):length(tmp)])) < 2)) tabtrend[pop, (i + 2)] <- navalue
      if (length(tmp) <= 4) tabtrend[pop, (i + 2)] <- navalue
    }
  }
  yrranges <- paste(unlist(lapply(year.ranges, min)),
                    unlist(lapply(year.ranges, max)),
                    sep = "-"
  )
  tmp.col <- as.vector(sapply(yrranges, c, c("lowCI", "upCI")))
  colnames(tabtrend) <- c("Population", "MPG", yrranges)
  if(all(is.na(tabtrend$MPG)) || all(tabtrend$MPG=="")) tabtrend <- tabtrend[,-2]
  tabtrend
}
