#' @name 
#' NWCTrends_report
#' @title
#' NWFSC Salmonid Status Review Table and Figures
#' @description 
#' This is the main function in the NWCTrends package which
#' creates the ESU tables and figures from the 
#' Northwest Fisheries Science Center's report: "2015 Status review 
#' update for Pacific salmon and steelhead listed under the Endangered 
#' Species Act: Pacific Northwest".

#' The 2015 Status Review report can be viewed by typing
#' \code{RShowDoc("2015 Status Review Update",package="NWCTrends")}
#' at the command line. The report has a description of the 
#' methods used for computing the smoothed trend lines and the status metrics. 
#' A pdf of the methods is also available by typing
#' \code{RShowDoc("Methods",package="NWCTrends")}
#' at the command line.
#' 
#' @details
#' The default model used to fit the data is that used in the 
#' 2015 Status Update. This model uses information across the 
#' populations in an ESU to estimate the process variance, 
#' non-process variance (residuals 
#' between smoothed fits and observed spawners), covariance in 
#' process errors (good and bad year correlation). However it 
#' allows each population to have a different trend.  This model is 
#' specified as \code{ model=list(Z="identity", R="diagonal and equal", U="unequal", Q="equalvarcov") }.
#' 
#' This function does all the steps to create the trend plots and figures
#' \enumerate{
#'   \item Load data (.csv)
#'   \item Fit model(s)
#'   \item Make plots and tables and save as report
#' }
#' By default, the plots and tables are saved in a directory
#'  named (and created if necessary) NWCTrend_output in your
#'  working directory. 
#'  
#' The look of the tables can be adjusted by passing in geomean.tables.control. See ?geomean_tables
#' for the elements that can be controlled. Note that if the defaults for
#' geomean.table.control are changed, they must be also changed in geomean_tables.R
#' 
#' @param inputfile comma-delimited data file (see demo files for the format). 
#' demofiles are in inst/doc/demodata.
#' @param fit.min.year The earliest year to use when fitting the models.
#' @param fit.max.year The latest year to use when fitting the models.
#' @param model The structure of the MARSS model to use. Entered as a list specified as a \link[MARSS]{MARSS} model.
#' @param logit.fw TRUE/FALSE whether to estimate the smoothed fraction wild from the logit of the fractions or from the raw (0,1) fractions.
#' @param plot.min.year The earliest year to use when plotting the data.
#' @param plot.max.year The latest year to use when plotting the data.
#' @param min.data.points The minimum data points to require from a population (for fitting and plotting).
#' @param geomean.table.control A list with the adjustable variables for geomean_table(). See ?geomean_table
#' @param trend.table.control A list with the adjustable variables for trend_15_table(). See ?trend_15_table
#' @param output.type "html", "pdf", or "word" Format to produce the report in.
#' @param output.dir Directory (in the working directory) where the output will be saved. Defaults to "NWCTrend_output". The directory will be created if it does not exist.

#' @return
#' Plots and tables that are saved to doc/figures/ESU_figures.
#' @author 
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
  #' @references
#' Ford, M. J., K. Barnas, T. Cooney, L. G. Crozier, M. Diaz, J. J. Hard, E. E. Holmes, D. M. Holzer, R. G. Kope, P. W. Lawson, M. Liermann, J. M. Myers, M. Rowse, D. J. Teel, D. M. Van Doornik, T. C. Wainwright, L. A. Weitkamp, M. Williams. 2015. Status Review Update for Pacific Salmon and Steelhead Listed under the Endangered Species Act:  Pacific Northwest. Nationa Marine Fisheries Service, Northwest Fisheries Science Center.
#' Available from the NWFSC Publications page.

NWCTrends_report=function(
  inputfile=NULL, 
  fit.min.year=1975, fit.max.year=2014,
  model=list(Z="identity", R="diagonal and equal", Q="equalvarcov", U="unequal"), 
  logit.fw=FALSE,
  plot.min.year=1980, plot.max.year=2014,
  min.data.points=5,
  geomean.table.control=list(min.year=1990, max.year=2014, lenbands=5, min.band.points=2, change.col="last.two"),
  trend.table.control=list(year.ranges=list(1990:2005,1999:2014)),
  output.type = c("html", "pdf", "word"),
  output.dir="NWCTrend_output"
){
  output.type = tolower(output.type)
  output.type = match.arg(output.type)
  if(!is.logical(logit.fw)) stop("logit.fw must TRUE or FALSE")
  # Set up the directory locations
  if(!dir.exists(output.dir)) dir.create(output.dir)
  instdocpath = system.file("doc", package="NWCTrends")
  texdir=system.file("doc", "report_files", package="NWCTrends") #where the tex wrappers are
  figdir=paste0(file.path(getwd(),output.dir),"/")
  
  # Get the input data
  if(missing(inputfile)){
    cat("Select a data file (.csv or .RData).\n")
    inputfile=file.choose()
  }
  filetype <- stringr::str_split(inputfile,"[.]")[[1]]
  filetype <- filetype[length(filetype)]

  # Read in the data
  if(filetype=="rdata" | filetype=="RData"){ #load fits and data
    inputnames <- load(inputfile)
    if(!all(c("datalist","fits")%in%inputnames))
      stop("The RData must contain datalist and fitslist.  Normally output from trend_fits().")
    
    ####### Replace with shiny app #################
    if(length(fits)>1){
      esu.choice <- choose.esu( names(fits) )
      fits <- fits[esu.choice]
    }
    ##############################################
    
  }else{ #need to read in data and fit  
    # This reads in the data file and creates the needed data objects
    if(filetype=="csv" | filetype==".xls" | filetype=="xlsx"){
      datalist <- data_setup(inputfile=inputfile, min.year=fit.min.year, max.year=fit.max.year)
    }else{
      stop("The inputfile should be data (.csv or .xls) or an RData file from a fit.")
    }
    
    #outputfile name
    #the outputfile is saved debugging purposes
    fits.file <- paste0(figdir,"NWCTrends_debug_file.RData")
    
    #then do multivariate analysis where
    fitslist <- trend_fits(datalist, fits.file, wild=FALSE, 
                          model=model, logit.fw=logit.fw)
    
    #the below code to makes the ESU figures 
    fits = fitslist$fits
  }
  metadat=datalist$metadat
  
  if(output.type=="pdf") output.type="latex" #this is what R wants for pdf
  if(output.type=="latex") render.type="pdf_document"
  if(output.type=="html") render.type="html_document"
  if(output.type=="word") render.type="word_document"
  
  #In the code for the Status Review, multiple models could be tested.
  # For this code only one model is used
  modn=1
  
  for(this.esu in 1:length(fits)){
    esuname=names(fits)[this.esu]
    ifit.total <- fits[[esuname]][[modn]]$fit
    ifit.fracwild <- fits[[esuname]][["fwlogitfit"]]
    esuname=names(fits)[this.esu]
    #Set up the min and max years
    years = as.numeric(colnames(ifit.total$model$data))
    if(is.null(plot.min.year)) plot.min.year <- years[1]
    if(plot.min.year<years[1]) plot.min.year <- years[1]
    if(is.null(plot.max.year)) plot.max.year <- max(years)
    if(plot.max.year>max(years)) plot.max.year <- max(years)
    
    # Figure out the names of the populations to plot
    # it's the row in total.fit where there are enough data points for the min to max year range
    pops = rownames(ifit.total$model$data)
    mpg=metadat$PopGroup[metadat$name%in%pops]
    pops.to.plot <-  pops[
      apply(ifit.total$model$data[,which(years==plot.min.year):which(years==plot.max.year),drop=FALSE],1,
            function(x){sum(!is.na(x))>=min.data.points})]
    # mpg.to.plot used for mgp col in tables
    mpg.to.plot <- sort(metadat$PopGroup[metadat$name%in%pops.to.plot])
    mpg.to.plot <- clean.mpg(mpg.to.plot)
    pops.to.plot <- pops.to.plot[order( metadat$PopGroup[metadat$name%in%pops.to.plot] )]
    if(esuname=="Salmon, Chinook (Puget Sound ESU)"){
      ord <- sort.PSChinook(pops.to.plot)
      pops.to.plot <- pops.to.plot[ord]
      mpg.to.plot <- mpg.to.plot[ord]
    }
    
    pops.to.plot.wild <-  rownames(ifit.fracwild$fracwild.raw)[
      apply(ifit.fracwild$fracwild.raw[,which(years==plot.min.year):which(years==plot.max.year),drop=FALSE],1,
            function(x){sum(!is.na(x))>=min.data.points})]
    # mpg.to.plot.wild used for mgp col in tables
    mpg.to.plot.wild <- sort(metadat$PopGroup[metadat$name%in%pops.to.plot.wild])
    mpg.to.plot.wild <- clean.mpg(mpg.to.plot.wild)
    pops.to.plot.wild <- pops.to.plot.wild[order( metadat$PopGroup[metadat$name%in%pops.to.plot.wild] )]
    if(esuname=="Salmon, Chinook (Puget Sound ESU)"){
      ord <- sort.PSChinook(pops.to.plot.wild)
      pops.to.plot.wild <- pops.to.plot.wild[ord]
      mpg.to.plot.wild <- mpg.to.plot.wild[ord]
    }
    outputfile=stringr::str_replace_all(esuname,"/","-")
    if(output.type=="latex") outputfile.ext <- ".pdf" 
    if(output.type=="html") outputfile.ext <- ".html" 
    if(output.type=="word") outputfile.ext <- ".docx" 
    outputfile=paste0(figdir, outputfile, outputfile.ext)
    
    #this Rmd file will make all the figures with a default name
    render(paste0(instdocpath,"/report_files/esu_report.Rmd"), render.type, 
           output_options=list(fig_caption=TRUE), quiet=TRUE)
    
    #this will rename the figures made to the ESU specific name
    file.rename(paste0(paste0(instdocpath,"/report_files/esu_report"), outputfile.ext), outputfile)
    outnames <- paste(figdir, stringr::str_replace_all(esuname,"/","-"),"-",
                   c("summary_fig.pdf","fracwild_fig.pdf","main_fig.pdf","productivity_fig.pdf", "main_fig.csv"), sep="")
    innames <- paste(figdir, c("summary_fig-1.pdf","fracwild_fig-1.pdf","main_fig-1.pdf","productivity_fig-1.pdf", "main_fig.csv"),sep="")
    tabnames <- c("trend_15_table", "geomean_wild_table", "geomean_total_table", "fracwild_table")
    tabinnames <- paste0(texdir,"/wrapper_", tabnames, ".tex", sep="")
    #oddly pdf created at base level not in folder where tex is
    taboutnames.tmp <- paste("wrapper_", tabnames, ".pdf", sep="")
    taboutnames <- paste0(figdir, stringr::str_replace_all(esuname,"/","-"),"-",
                      tabnames, ".pdf")
    
    if(output.type=="latex"){
      for(i in 1:length(outnames))
        file.rename(innames[i], outnames[i]) #rename the tmp fig to fig with ESU
      for(i in 1:length(taboutnames)){
        texi2pdf(tabinnames[i], clean=TRUE) #create tables from tex
        file.remove(paste(figdir, tabnames[i], ".tex", sep="")) #remove the tex file (only wrapper needed it)
        file.rename(taboutnames.tmp[i], taboutnames[i]) #rename table pdf
      }
    }
    
    if(output.type=="html" | output.type=="word"){
      #rename the tmp fig to fig with ESU
      innames <- paste(figdir, c("summary_fig-1.png","fracwild_fig-1.png","main_fig-1.png","productivity_fig-1.png", "main_fig.csv"),sep="")
      outnames <- paste(figdir, stringr::str_replace_all(esuname,"/","-"),"-",
                     c("summary_fig.png","fracwild_fig.png","main_fig.png","productivity_fig.png","main_fig.csv"), sep="")
      for(i in 1:length(outnames)) file.rename(innames[i], outnames[i]) 
    }
  }
  
}
