.onLoad <- function(libname, pkgname) {
  assign("pkg_globals", new.env(), envir = parent.env(environment()))
  
  nmfs.palette = list(red="#D44045", white="#FFFFFF", green="#007934", blue="#00467F", black="#000000")
  
  # Set up the generic defaults for plots
  nwctrends.options <- list()
  nwctrends.options$main.title.cex <- 1.5
  nwctrends.options$main.subtitle.cex <- 1
  nwctrends.options$main.ylabel.cex <- 0.8
  nwctrends.options$main.raw.pch <- 19
  nwctrends.options$main.raw.col <- nmfs.palette$blue
  nwctrends.options$main.total.col <- nmfs.palette$black
  nwctrends.options$main.total.lwd <- 3
  nwctrends.options$main.total.lty <- 1
  nwctrends.options$main.wild.col <- nmfs.palette$red
  nwctrends.options$main.wild.lwd <- 1
  nwctrends.options$main.wild.lty <- 1
  nwctrends.options$main.ci.col <- "grey75"
  nwctrends.options$main.ci.border <- NA
  nwctrends.options$main.NAtotal.col <- "grey"
  nwctrends.options$main.NAtotal.lwd <- 3
  nwctrends.options$main.NAtotal.lty <- 1
  nwctrends.options$main.NAci.col <- "grey95"
  nwctrends.options$main.NAci.border <- NA
  
  nwctrends.options$fracwild.title.cex <- 1.5
  nwctrends.options$fracwild.subtitle.cex <- 1
  nwctrends.options$fracwild.ylabel.cex <- 0.8
  nwctrends.options$fracwild.raw.pch <- 1
  nwctrends.options$fracwild.raw.col <- nmfs.palette$black
  nwctrends.options$fracwild.col <- nmfs.palette$blue
  nwctrends.options$fracwild.lwd <- 2
  nwctrends.options$fracwild.lty <- 1
  
  nwctrends.options$prod.title.cex <- 1.5
  nwctrends.options$prod.poptitle.cex <- 1
  nwctrends.options$prod.ylabel.cex <- 0.8
  nwctrends.options$prod.col.neg <- nmfs.palette$red
  nwctrends.options$prod.col.pos <- nmfs.palette$green

  nwctrends.options$geomean.title.cex <- 1.5
  nwctrends.options$geomean.ylabel.cex <- 1
  nwctrends.options$geomean.pch <- 19
  nwctrends.options$geomean.cex <- 1.5
  nwctrends.options$geomean.col.neg <- nmfs.palette$red
  nwctrends.options$geomean.col.pos <- nmfs.palette$black
  nwctrends.options$geomean.abline.lty <- 2
  nwctrends.options$geomean.abline.col <- "grey"
 
  nwctrends.options$trend.title.cex <- 1.5
  nwctrends.options$trend.ylabel.cex <- 1
  nwctrends.options$trend.axis.cex <- 1
  nwctrends.options$trend.pch <- 19
  nwctrends.options$trend.cex <- 1.5
  nwctrends.options$trend.col.neg <- nmfs.palette$red
  nwctrends.options$trend.col.pos <- nmfs.palette$black
  
  assign("nwctrends.options", nwctrends.options, pkg_globals)
  assign("nmfs.palette", nmfs.palette, pkg_globals)
}
