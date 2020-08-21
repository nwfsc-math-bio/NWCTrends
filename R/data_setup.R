#' Read in the inputfile
#'
#' Read in the csv inputfile and create the data frames and matrices needed for
#' the fitting, plots and tables: matdat.spawners, matdat.wildspawners, and metadata.
#' Some clean up of names and runtiming is done.
#'
#' NAs are specified with -99, -99.00 or -99.0.
#'
#' @param inputfile .csv file. See demofiles for the proper format.
#' @param min.year The minimum year for the returned data frames. If left off, it will use the minimum year in the data set. You can set later to exclude data or set before to hindcast.
#' @param max.year The maximum year for the returned data frames. If left off, it will use the maximum year in the data set. You can set earlier to exclude data or set later to forecast.
#'
#' @return A list with four items:
#' \describe{
#'   \item{dat}{The raw data for the selected ESUs.}
#'   \item{matdat.spawners}{A matrix of the total spawners with NAs for missing years. Each column is a year from min.year to max.year and each row is a population.}
#'   \item{matdat.wildspawners}{A matrix of the the wildspawners using the fracwild data if included. NAs for years with either missing fracwild or missing spawner count. Each column is a year from min.year to max.year and each row is a population.}
#'   \item{metadat}{A data.frame with all the metadata for each population: name = population name, ESU = ESU name, Species, Run = runtiming for population, PopGroup = name of the Major Population Group (within ESU).}
#' }
#'
data_setup <- function(inputfile, min.year, max.year) {

  # toproper function; make column names nice
  toproper <- function(x) {
    x <- tolower(x)
    x <- stringr::str_replace_all(x, "[.]", " ")
    x <- stringr::str_replace_all(x, "_", " ")
    out <- c()
    for (i in x) {
      s <- strsplit(i, " ")[[1]]
      s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " "
      )
      out <- c(out, s)
    }
    out <- stringr::str_replace_all(out, " ", ".")
    out <- stringr::str_replace_all(out, "Nwr", "NWR")
    out <- stringr::str_replace_all(out, "Esu", "ESU")

    out
  }

  # read in the data
  if (stringr::str_sub(inputfile, -4) == ".xls" | stringr::str_sub(inputfile, -5) == ".xlsx") {
    dat <- gdata::read.xls(inputfile, na.strings = c("-99", "-99.00", "-99.0", "-99.000"), stringsAsFactors = FALSE)
    dat <- dat[,!stringr::str_detect(colnames(dat), "X[.]")]
  }
  if (stringr::str_sub(inputfile, -4) == ".csv"){
    dat <- read.csv(inputfile, header = TRUE, na.strings = c("-99", "-99.00", "-99.0", "-99.000"), stringsAsFactors = FALSE)
  }

  # Check that all required columns are present
  colnames(dat)[colnames(dat)=="BROOD_YEAR" | colnames(dat)=="Brood_Year"] <- "YEAR"
  colnames(dat)[colnames(dat)=="MPG"] <- "MAJOR_POPULATION_GROUP"
  colnames(dat) <- toupper(colnames(dat))
  
  required <- c(
    "YEAR", "NUMBER_OF_SPAWNERS", "SPECIES", "FRACWILD", "COMMON_POPULATION_NAME",
    "RUN_TIMING", "ESU", "MAJOR_POPULATION_GROUP"
  )
  if (!all(
    required %in% colnames(dat) |
    toproper(required) %in% colnames(dat) |
    required %in% toproper(colnames(dat)) |
    toproper(required) %in% toproper(colnames(dat)))) {
    cat(paste0("\nYour data file must have the following columns: ", paste(required, collapse = ", "), "."))
    cat(paste0("\n\nData file is missing: ", paste(required[!(required %in% colnames(dat))], collapse = ", "), "."))
    stop()
  }
  if (any(duplicated(colnames(dat)))) {
    cat("Duplicated colnames in", inputfile, "\n")
    stop()
  }
  
  # clean up some column names
  names(dat) <- toproper(names(dat))
  names(dat)[names(dat) == "Brood.Year"] <- "Year"
  names(dat)[names(dat) == "Number.Of.Spawners"] <- "Spawners"
  names(dat)[names(dat) == "Catch"] <- "Effective.Catch"
  names(dat)[names(dat) == "ESU.Name"] <- "ESU"
  if (!("Run.Timing" %in% names(dat))) dat$Run.Timing <- NA

  ## Derived Datasets
  dat$wildspawners <- dat$Spawners * dat$Fracwild

  ## Do clean up on names to get rid of duplicated run timing info
  dat$Common.Population.Name <- stringr::str_trim(dat$Common.Population.Name)
  for (i in c(
    "Fall-run", "Winter-run", "Spring-run", "Summer-run", "Late-run", "Early-run", "Early-late-run",
    "Spring", "Fall", "Winter", "Summer", "SpR", "WR"
  )) {
    bad <- stringr::str_detect(dat$Common.Population.Name, i)
    if(i=="Fall") bad <- stringr::str_detect(dat$Common.Population.Name, i) & 
        !stringr::str_detect(dat$Common.Population.Name, "Falls")
    tmp <- dat$Common.Population.Name[bad]
    tmp2 <- stringr::str_replace_all(tmp, i, "")
    tmp2 <- stringr::str_replace_all(tmp2, tolower(i), "")
    dat$Common.Population.Name[bad] <- tmp2
  }
  dat$Common.Population.Name <- stringr::str_trim(dat$Common.Population.Name)

  ## replace 0s with NAs
  dat$Spawners[dat$Spawners == 0] <- NA
  dat$wildspawners[dat$wildspawners == 0] <- NA

  dat$unique.name <- paste(dat$ESU, dat$Common.Population.Name, dat$Run.Timing, sep = "|")

  # Set up the size of matrices
  if(is.null(min.year)){ 
    min.yr <- min(dat[!is.na(dat$Spawners), "Year"])
  }else{
    if(min.year > max(dat[!is.na(dat$Spawners), "Year"])) 
      stop("fit.min.year is greater than max year in the data set.\n")
    min.yr <- min.year
  }
  if(is.null(max.year)){ 
    max.yr <- max(dat[!is.na(dat$Spawners), "Year"])
  }else{
    if(max.year < min(dat[!is.na(dat$Spawners), "Year"])) 
      stop("fit.max.year is less than min year in the data set.\n")
    max.yr <- max.year
  }
  if(min.yr > max.yr) stop("fit.max.year is less than fit.min.year.\n")
  years <- min.yr:max.yr
  nyr <- length(years)
  # clear out year before or after fit.min.year and fit.max.year
  dat <- dat[dat$Year>=min.yr & dat$Year<=max.yr,]
  

  ####### Replace with shiny app #################
  esu.names <- unique(dat$ESU)
  esu.choice <- choose.esu(esu.names)
  dat <- dat[dat$ESU %in% esu.names[esu.choice], ]
  ##############################################

  pops <- unique(dat$unique.name)
  esus <- dat$ESU[match(pops, dat$unique.name)]
  runtimings <- dat$Run.Timing[match(pops, dat$unique.name)]
  species <- dat$Species[match(pops, dat$unique.name)]
  majorpopgroup <- dat$Major.Population.Group[match(pops, dat$unique.name)]
  metadat <- data.frame(
    name = pops, ESU = esus, Run = runtimings,
    Species = species, PopGroup = majorpopgroup,
    min.year = min.yr, max.year = max.yr,
    stringsAsFactors = FALSE
  )

  npops <- length(pops)

  # create data matrices
  matdat.spawners <- matdat.wildspawners <- matdat.fracwild <- matrix(NA, npops, nyr, dimnames = list(pops, years))

  # check that there are no problems
  for (i in pops) {
    if (any(duplicated(match(dat$Year[dat$Common.Population.Name == i], years)))) {
      cat("Problem ", i, "\n")
    }
  }

  # fill the matrices
  for (i in pops) {
    matdat.spawners[i, match(dat$Year[dat$unique.name == i], years)] <- dat$Spawners[dat$unique.name == i]
    matdat.wildspawners[i, match(dat$Year[dat$unique.name == i], years)] <- dat$wildspawners[dat$unique.name == i]
    matdat.fracwild[i, match(dat$Year[dat$unique.name == i], years)] <- dat$Fracwild[dat$unique.name == i]
  }
  yr1 <- which(colnames(matdat.spawners) == min.yr)
  yr2 <- which(colnames(matdat.spawners) == max.yr)
  matdat.spawners <- matdat.spawners[, yr1:yr2, drop = FALSE]
  matdat.wildspawners <- matdat.wildspawners[, yr1:yr2, drop = FALSE]
  matdat.fracwild <- matdat.fracwild[, yr1:yr2, drop = FALSE]
  
  return(
    list(
      dat = dat,
      matdat.spawners = matdat.spawners,
      matdat.wildspawners = matdat.wildspawners,
      matdat.fracwild = matdat.fracwild,
      metadat = metadat
    )
  )
}
