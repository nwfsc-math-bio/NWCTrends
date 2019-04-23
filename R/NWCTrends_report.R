#######################################################################################
## This function does all the steps to create the trend plots and figures
## 1. Load data (.csv)
## 2. Fit model(s)
## 3. Make plots and tables and save as report
# The default model is that used in the 2016 status update
# This uses information across the populations in an ESU to estimate process variance, 
# non-process variance (residuals about the black lines), covarianc in process errors 
# (good and bad year correlation) but allows each population to have a different trend.
# Z=identity; R=diag and equal; U=unequal; Q=equalvarcov
# AIC selected U=equal over this, however for the purpose of the status review, U needed to
# be 'unequal' to show the estimate of the trend for each population
#######################################################################################
NWCTrends_report=function(
  inputfile=file.choose(), 
  fit.min.year=1975, fit.max.year=2014,
  model=list(Z="identity", R="diagonal and equal", Q="equalvarcov", U="unequal"), 
  logit.fw=FALSE,
  plot.min.year=1980, plot.max.year=2014,
  min.data.points=5,
  output.type = "pdf"
){ 
  #logit.fw is a flag for whether to smooth the raw fracwild data (0 to 1)
  # or to smooth the logit of the fracwild; they give similar results
  
  ## For shiny app ##
  # User will choose an input file
  # Choose fit.min.year and fit.max.year (based on min and max in data)
  # Choose plot.min.year and plot.max.year  (based on min and max in data)
  # min.data.points (a number between 2 and 10)
  # other arguments can be left at defaults for now
  
  ## Output
  # The user will want to be able to download the files produced for each ESU
  # There are 9 produced.  See inst/doc/figures/ESU_figures
  # where I have left in results from running one ESU.
  # inst/doc/figures/ESU_figures
  #
  # But we will also want to show the output in tabs.  Since the user might want
  # to run multiple ESUs, I'm not sure how best to do this.
  # Run this NWCTrends_report(output.type="html")
  # An html file will appear in inst/doc/figures/ESU_figures
  #  Look at that.  I could create a nicer version of that.
  #  Maybe a tab for each ESU and show that html file in each?
  #  Note, the .html file will use the .png files in inst/doc/figure in that case
  
  ####### Replace with shiny app #################
  # In the function argument, inputfile is defined as file.choose()
  #demo file here
  # "inst/appdir/demofiles/PNW_Listed_Salmonids_2016.csv"
  filetype=str_split(inputfile,"[.]")[[1]]
  filetype=filetype[length(filetype)]
  ##############################################
  
  #Set equal to NA until it gets set
  
  if(filetype=="rdata" | filetype=="RData"){ #load fits and data
    inputnames=load(inputfile)
    if(!all(c("datalist","fits")%in%inputnames))
      stop("The RData must contain datalist and fitslist.  Normally output from trend_fits().")
    
    ####### Replace with shiny app #################
    if(length(fits)>1){
      esu.choice=choose.esu( names(fits) )
      fits=fits[esu.choice]
    }
    ##############################################
    
  }else{ #need to read in data and fit  
    # This reads in the data file and creates the needed data objects
    if(filetype=="csv" | filetype==".xls" | filetype=="xlsx"){
      datalist=data_setup(inputfile=inputfile, min.year=fit.min.year, max.year=fit.max.year)
    }else{
      stop("The inputfile should be data (.csv or .xls) or an RData file from a fit.")
    }
    
    #tmp; need to get this from input file
    #the file is mostly for debugging actually
    fits.file = "inst/doc/fits_debug_file_ok_to_delete.RData"
    
    #then do multivariate analysis where
    fitslist = trend_fits(datalist, fits.file, wild=FALSE, 
                          model=model, logit.fw=FALSE)
    
    #the below code to makes the ESU figures 
    fits = fitslist$fits
  }
  metadat=datalist$metadat
  
  if(output.type=="pdf") output.type="latex" #this is what R wants for pdf
  if(output.type=="latex") render.type="pdf_document"
  if(output.type=="html") render.type="html_document"
  
  modn=1
  
  for(this.esu in 1:length(fits)){
    esuname=names(fits)[this.esu]
    ifit.total = fits[[esuname]][[modn]]$fit
    ifit.fracwild = fits[[esuname]][["fwlogitfit"]]
    esuname=names(fits)[this.esu]
    #Set up the min and max years
    years = as.numeric(colnames(ifit.total$model$data))
    if(is.null(plot.min.year)) plot.min.year = years[1]
    if(plot.min.year<years[1]) plot.min.year = years[1]
    if(is.null(plot.max.year)) plot.max.year = max(years)
    if(plot.max.year>max(years)) plot.max.year = max(years)
    
    #Figure out the names of the populations to plot
    #it's the row in total.fit where there are enough data points for the min to max year range
    pops = rownames(ifit.total$model$data)
    mpg=metadat$PopGroup[metadat$name%in%pops]
    pops.to.plot =  pops[
      apply(ifit.total$model$data[,which(years==plot.min.year):which(years==plot.max.year),drop=FALSE],1,
            function(x){sum(!is.na(x))>=min.data.points})]
    #mpg.to.plot used for mgp col in tables
    mpg.to.plot = sort(metadat$PopGroup[metadat$name%in%pops.to.plot])
    mpg.to.plot = clean.mpg(mpg.to.plot)
    pops.to.plot = pops.to.plot[order( metadat$PopGroup[metadat$name%in%pops.to.plot] )]
    if(esuname=="Salmon, Chinook (Puget Sound ESU)"){
      ord=sort.PSChinook(pops.to.plot)
      pops.to.plot=pops.to.plot[ord]
      mpg.to.plot=mpg.to.plot[ord]
    }
    
    pops.to.plot.wild =  rownames(ifit.fracwild$fracwild.raw)[
      apply(ifit.fracwild$fracwild.raw[,which(years==plot.min.year):which(years==plot.max.year),drop=FALSE],1,
            function(x){sum(!is.na(x))>=min.data.points})]
    #mpg.to.plot.wild used for mgp col in tables
    mpg.to.plot.wild = sort(metadat$PopGroup[metadat$name%in%pops.to.plot.wild])
    mpg.to.plot.wild = clean.mpg(mpg.to.plot.wild)
    pops.to.plot.wild = pops.to.plot.wild[order( metadat$PopGroup[metadat$name%in%pops.to.plot.wild] )]
    if(esuname=="Salmon, Chinook (Puget Sound ESU)"){
      ord=sort.PSChinook(pops.to.plot.wild)
      pops.to.plot.wild=pops.to.plot.wild[ord]
      mpg.to.plot.wild=mpg.to.plot.wild[ord]
    }
    texdir="inst/doc/figures/" #where the tex wrappers are
    figdir="inst/doc/figures/ESU_figures/" #where the pdfs are put
    outputfile=str_replace_all(esuname,"/","-")
    if(output.type=="latex") outputfile.ext = ".pdf" 
    if(output.type=="html") outputfile.ext = ".html" 
    outputfile=paste(figdir, outputfile,outputfile.ext, sep="")
    #this Rmd file will make all the figures with a default name
    render("inst/doc/esu_report.Rmd", render.type, 
           output_options=list(fig_caption=TRUE), quiet=TRUE)
    
    #this will rename the figures made to the ESU specific name
    file.rename(paste("inst/doc/esu_report",outputfile.ext,sep=""), outputfile)
    outnames=paste(figdir, str_replace_all(esuname,"/","-"),"-",
                   c("summary_fig.pdf","fracwild_fig.pdf","main_fig.pdf","productivity_fig.pdf"), sep="")
    innames = paste(texdir, c("summary_fig-1.pdf","fracwild_fig-1.pdf","main_fig-1.pdf","productivity_fig-1.pdf"),sep="")
    tabnames=c("trend_15_table", "geomean_wild_table", "geomean_total_table", "fracwild_table")
    tabinnames=paste(texdir,"wrapper_", tabnames, ".tex", sep="")
    #oddly pdf created at package level not inst/doc/figures where tex is
    taboutnames.tmp=paste("wrapper_", tabnames, ".pdf", sep="")
    taboutnames=paste(figdir,str_replace_all(esuname,"/","-"),"-",
                      tabnames, ".pdf", sep="")
    
    if(output.type=="latex"){
      for(i in 1:4){
        file.rename(innames[i], outnames[i]) #rename the tmp fig to fig with ESU
        texi2pdf(tabinnames[i], clean=TRUE) #create tables from tex
        file.remove(paste(texdir, tabnames[i], ".tex", sep="")) #remove the tex file (only wrapper needed it)
        file.rename(taboutnames.tmp[i], taboutnames[i]) #rename table pdf
      }
    }
    
    if(output.type=="html"){
      pngnames = paste(texdir, c("summary_fig-1.png","fracwild_fig-1.png","main_fig-1.png","productivity_fig-1.png"),sep="")
      file.remove(pngnames) #remove the png files
      file.remove(paste(texdir, tabnames, ".tex", sep="")) 
    }
  }
  
}
