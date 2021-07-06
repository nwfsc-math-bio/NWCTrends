#' Clean up population names
#'
#' Utility function to clean up the population names and strip run timing
#' in the population names in the input file. 
#' Change this file if the abbreviations used need to be changed.
#'
#' @param pops A vector of the population names from the input csv file.
#'
#' @return A vector of cleaned population names.
#' @keywords utility
#' @author
#' Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
#' 
clean.pops <- function(pops) {
  pops.trim <- sapply(pops, function(x) {
    stringr::str_split(x, "[|]")[[1]][2]
  })

  pops.trim <- stringr::str_replace_all(pops.trim, "Spring[/]", "")
  pops.trim <- stringr::str_replace_all(pops.trim, "Rivers", "R.")
  pops.trim <- stringr::str_replace_all(pops.trim, "River", "R.")
  pops.trim <- stringr::str_replace_all(pops.trim, "Creeks", "Cr.")
  pops.trim <- stringr::str_replace_all(pops.trim, "Creek", "Cr.")
  pops.trim <- stringr::str_replace(pops.trim, "Tributaries", "Tribs.")
  run.trim <- sapply(pops, function(x) {
    stringr::str_split(x, "[|]")[[1]][3]
  })
  run.trim <- stringr::str_replace(run.trim, "Fall-run", "FR")
  run.trim <- stringr::str_replace(run.trim, "Spring[/]Summer-run", "SSR")
  run.trim <- stringr::str_replace(run.trim, "Summer[/]Winter-run", "SWR")
  run.trim <- stringr::str_replace(run.trim, "Winter-run", "WR")
  run.trim <- stringr::str_replace(run.trim, "Early-run", "ER")
  run.trim <- stringr::str_replace(run.trim, "Early-late-run", "ELR")
  run.trim <- stringr::str_replace(run.trim, "Spring-run", "SpR")
  run.trim <- stringr::str_replace(run.trim, "Summer-run", "SuR")
  run.trim <- stringr::str_replace(run.trim, "Late-run", "LR")
  run.trim <- stringr::str_replace(run.trim, "Late fall", "LFR")
  run.trim <- stringr::str_replace(run.trim, "NA", "")
  pops.trim <- paste(pops.trim, run.trim)
  pops.trim <- stringr::str_replace(pops.trim, "Spring SSR", "SSR")
  pops.trim <- stringr::str_replace(pops.trim, "Washington", "WA")
  pops.trim <- stringr::str_replace(pops.trim, "Oregon", "OR")
  pops.trim <- stringr::str_replace(pops.trim, "North Fork", "NF")
  pops.trim <- stringr::str_replace(pops.trim, "North", "N.")
  pops.trim <- stringr::str_replace(pops.trim, "South Fork", "SF")
  pops.trim <- stringr::str_replace(pops.trim, "Middle Fork", "MF")
  pops.trim <- stringr::str_replace(pops.trim, "East Fork", "EF")
  pops.trim <- stringr::str_replace(pops.trim, "West Fork", "WF")
  pops.trim <- stringr::str_replace(pops.trim, "South", "S.")
  pops.trim <- stringr::str_replace(pops.trim, "Upper", "Up.")
  pops.trim <- stringr::str_replace(pops.trim, "Lower", "Low.")
  pops.trim <- stringr::str_replace(pops.trim, ", and ", "/")
  pops.trim <- stringr::str_replace(pops.trim, " and ", "/")
  pops.trim <- stringr::str_replace(pops.trim, ", ", "/")

  pops.trim <- stringr::str_trim(pops.trim)
  pops.trim
}
