#' Read in the inputfile
#'
#' Read in the csv inputfile and create the data frames and matrices needed for 
#' the fitting, plots and tables: matdat.spawners, matdat.wildspawners, and metadata. 
#' Some clean up of names and runtiming is done.
#' 
#' NAs are specified with -99, -99.00 or -99.0.
#'
#' @param inputfile .csv file. See demofiles for the proper format.
#' @param min.year The minumum year for the returned data frames.
#' @param max.year The maximum year for the returned data frames.
#'
#' @return A list with four items:
#' \describe{
#'   \item{dat}{The raw data for the selected ESUs.}
#'   \item{matdat.spawners}{A matrix of the total spawners with NAs for missing years. Each column is a year from min.year to max.year and each row is a population.}
#'   \item{matdat.wildspawners}{A matrix of the the wildspawners using the fracwild data if included. NAs for years with either missing fracwild or missing spawner count. Each column is a year from min.year to max.year and each row is a population.}
#'   \item{metadat}{A data.frame with all the metadata for each population: name = population name, ESU = ESU name, Species, Run = runtiming for population, PopGroup = name of the Major Population Group (within ESU).}
#' } 
#'
data_setup=function(inputfile, min.year, max.year){
  require(stringr)
  
  if(str_split(inputfile,"[.]")[[1]][2]!="csv") stop("Inputfile must be a .csv file.")
  
  ####### Replace with shiny app #################
  # This code is specific to my machine.  Replace 
  if(stringr::str_sub(inputfile,-4)==".xls" | stringr::str_sub(inputfile,-5)==".xlsx"){
    require(XLConnect)    
    inputfile = xls2csv(inputfile, sheet=1, perl="C:/Program Files (x86)/Perl/bin/perl.exe")
  }
  ####### Replace with shiny app #################
  
  #toproper function; make column names nice
  toproper = function(x){
    require(stringr)
    x=tolower(x)
    x=stringr::str_replace_all(x, "[.]", " ")
    x=stringr::str_replace_all(x, "_", " ")
    out=c()
    for(i in x){
      s = strsplit(i, " ")[[1]]
      s = paste(toupper(substring(s, 1, 1)), substring(s, 2),
                sep = "", collapse = " ")
      out = c(out, s)
    }
    out=stringr::str_replace_all(out, " ", ".")
    out=stringr::str_replace_all(out, "Nwr", "NWR")
    out=stringr::str_replace_all(out, "Esu", "ESU")
    
    out
  }
  
  #read in the data
  dat = read.csv(inputfile,header=TRUE,na.strings=c('-99','-99.00','-99.0'),stringsAsFactors=FALSE)
    
  #clean up some column names
  names(dat) = toproper(names(dat))
  names(dat)[names(dat)=="Brood.Year"]="Year"
  names(dat)[names(dat)=="Number.Of.Spawners"]="Spawners"
  names(dat)[names(dat)=="Catch"]="Effective.Catch"
  names(dat)[names(dat)=="ESU.Name"]="ESU"
  if(!("Run.Timing"%in%names(dat))) dat$Run.Timing=NA
  
  #Check that all required columns are present
  required <- c("BROOD_YEAR", "NUMBER_OF_SPAWNERS", "Species", "FRACWILD", "COMMON_POPULATION_NAME", 
                "RUN_TIMING", "ESU", "MAJOR_POPULATION_GROUP")
  if(!all(required %in% colnames(dat))) stop(paste("Your data file must have the following columns:", required,"."))
  
  ## Derived Datasets
  dat$wildspawners = dat$Spawners*dat$Fracwild
  
  # append run time to common name to make common names unique 
  for(i in 1:nrow(dat)){
    if(is.na(dat$Run.Timing[i])==FALSE){
      dat$Common.Population.Name[i] = paste(dat$Common.Population.Name[i],dat$Run.Timing[i],sep=" ")}
  }
  
  ## Do clean up on names
  ## Do not do this. It gets rid of everything past the run-timing in the name
  # dat$Common.Population.Name = stringr::str_trim(dat$Common.Population.Name)
  # #clean up the common names with run timing
  # for(i in c("Fall-run", "Winter-run", "Spring-run", "Summer-run", "Late-run", "Early-run", "Early-late-run")){
  #   tmp = dat$Common.Population.Name[stringr::str_detect(dat$Common.Population.Name, i)]
  #   tmp2 = stringr::str_sub(tmp, 1, stringr::str_locate(tmp, i)[,1]-1)
  #   dat$Common.Population.Name[stringr::str_detect(dat$Common.Population.Name, i)] = tmp2
  # }
  # dat$Common.Population.Name = stringr::str_trim(dat$Common.Population.Name)
  
  ## replace 0s with NAs
  dat$Spawners[dat$Spawners==0]=NA
  dat$wildspawners[dat$wildspawners==0]=NA
  
  dat$unique.name = paste(dat$ESU,dat$Common.Population.Name, dat$Run.Timing,sep="|")
  
  # Set up the size of matricesx
  min.yr = min(dat[,"Year"], min.year)
  max.yr = max(dat[,"Year"], max.year)
  years = min.yr:max.yr
  nyr = length(years)

  ####### Replace with shiny app #################
  esu.names = unique(dat$ESU)
  esu.choice=choose.esu(esu.names)
  dat=dat[dat$ESU%in%esu.names[esu.choice], ]
  ##############################################
    
  pops = unique(dat$unique.name)
  esus=dat$ESU[match(pops,dat$unique.name)]
  runtimings=dat$Run.Timing[match(pops,dat$unique.name)]
  species=dat$Species[match(pops,dat$unique.name)]
  majorpopgroup=dat$Major.Population.Group[match(pops,dat$unique.name)]
  metadat=data.frame(
    name=pops, ESU=esus, Run=runtimings, 
    Species=species, PopGroup=majorpopgroup,
    stringsAsFactors = FALSE
    )
  
  npops = length(pops)
  
  #create data matrices
  matdat.spawners = matdat.wildspawners = recr = recr2 = matrix(NA, npops, nyr, dimnames=list(pops, years))
  
  #check that there are no problems
  for(i in pops){
    if(any(duplicated(match(dat$Year[dat$Common.Population.Name==i],years))))
      cat("Problem ", i, "\n")
  }
  
  #fill the matrices
  for(i in pops){
    matdat.spawners[i,match(dat$Year[dat$unique.name==i],years)]=dat$Spawners[dat$unique.name==i]
    matdat.wildspawners[i,match(dat$Year[dat$unique.name==i],years)]=dat$wildspawners[dat$unique.name==i]
  }
  yr1=which(colnames(matdat.spawners)==min.year)
  yr2=which(colnames(matdat.spawners)==max.year)
  matdat.spawners=matdat.spawners[,yr1:yr2,drop=FALSE]
  matdat.wildspawners=matdat.wildspawners[,yr1:yr2,drop=FALSE]
  
  return(
    list(
      dat=dat, 
      matdat.spawners=matdat.spawners, 
      matdat.wildspawners=matdat.wildspawners, 
      metadat=metadat
    )
  )
}