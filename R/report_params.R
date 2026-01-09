#' @name
#' report_params
#' @title
#' Parameters for ESU_report.Rmd
#' @description
#' This returns a list of the needed parameters for the report Rmd.
#'
#' @param figdir Where output is saved.
#' @param fitslist A list of fits returned from `trend_fits()`
#' @param datalist A list of input returned from `data_setup()`
#' @param plot.min.year Optional. The earliest year to use when plotting the data if different than the first year in the data set.
#' @param plot.max.year Optional. The last year to use when plotting the data if different than the last year in the data set.
#' @param output.type "html", "pdf", or "word" Format to produce the report in.
#' @param this.esu If there there are multiple ESUs in the fitslist$fits object (a list), then this is the one to use. An integer.
#' @param min.data.points The minimum data points to require from a population (for fitting and plotting).
#' 
#' @return
#' A list of parameters.
#' @keywords internal
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
report_params <- function(figdir, fits, datalist, this.esu=1, plot.min.year=NULL, plot.max.year=NULL, output.type="word", min.data.points=5){
  metadat <- datalist$metadat
  modn <- 1
  
  rparams <- list()
  esuname <- names(fits)[this.esu]
  ifit.total <- fits[[esuname]][[modn]][["fit"]]
  ifit.fracwild <- fits[[esuname]][["fwlogitfit"]]

  # Set up the min and max years
  years <- as.numeric(colnames(ifit.total$model$data))
  if (is.null(plot.min.year)) plot.min.year <- years[1]
  if (is.null(plot.max.year)) plot.max.year <- max(years)
  
  # Figure out the names of the populations to plot
  # it's the row in total.fit where there are enough data points for the min to max year range
  pops <- rownames(ifit.total$model$data)
  mpg <- metadat$PopGroup[metadat$name %in% pops]
  pops.to.plot <- pops[
    apply(
      ifit.total$model$data, 1,
      function(x) {
        sum(!is.na(x)) >= min.data.points
      }
    )
  ]
  # mpg.to.plot used for mgp col in tables
  mpg.to.plot <- metadat$PopGroup[metadat$name %in% pops.to.plot]
  mpg.to.plot <- clean.mpg(mpg.to.plot)
  # popid.to.plot used for popid col in tables
  popid.to.plot <- metadat$PopID[metadat$name %in% pops.to.plot]
  
  pops.to.plot.wild <- rownames(ifit.fracwild$fracwild.raw)[
    apply(
      ifit.fracwild$fracwild.raw, 1,
      function(x) {
        sum(!is.na(x)) >= min.data.points
      }
    )
  ]
  if(length(pops.to.plot.wild)==0){
    cat(paste("No populations in", esuname, "have fracwild info. ESU is being skipped.\n"))
    return(list(ok=FALSE))
  }
  # mpg.to.plot.wild used for mgp col in tables
  mpg.to.plot.wild <- metadat$PopGroup[metadat$name %in% pops.to.plot.wild]
  mpg.to.plot.wild <- clean.mpg(mpg.to.plot.wild)
  # mpg.to.plot.wild used for mgp col in tables
  popid.to.plot.wild <- metadat$PopID[metadat$name %in% pops.to.plot.wild]
  
  outputfile <- stringr::str_replace_all(esuname, "/", "-")
  if (output.type == "latex") outputfile.ext <- ".pdf"
  if (output.type == "html") outputfile.ext <- ".html"
  if (output.type == "word") outputfile.ext <- ".docx"
  outputfile <- paste0(figdir, outputfile, outputfile.ext)
  
  rparams$esuname <- esuname
  rparams$ifit.total <- ifit.total
  rparams$ifit.fracwild <- ifit.fracwild
  rparams$popid.to.plot <- popid.to.plot
  rparams$popid.to.plot.wild <- popid.to.plot.wild
  rparams$plot.min.year <- plot.min.year
  rparams$plot.max.year <- plot.max.year
  rparams$pops.to.plot <- pops.to.plot
  rparams$pops.to.plot.wild <- pops.to.plot.wild
  rparams$mpg.to.plot <- mpg.to.plot
  rparams$mpg.to.plot.wild <- mpg.to.plot.wild
  rparams$output.type <- output.type
  rparams$outputfile <- outputfile
  rparams$figdir <- figdir
  rparams$outputfile.ext <- outputfile.ext
  rparams$ok <- TRUE
  
  return(rparams) 
}