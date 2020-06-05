# Utility function to clean up population names to make shorter
clean.mpg <- function(pops) {
  pops <- as.character(pops)
  pops.trim <- stringr::str_trim(pops)
  pops.trim <- stringr::str_replace_all(pops.trim, " [/] ", "/")
  pops.trim <- stringr::str_replace_all(pops.trim, " [-] ", "-")
  pops.trim <- stringr::str_replace_all(pops.trim, "Rivers", "R.")
  pops.trim <- stringr::str_replace_all(pops.trim, "River", "R.")
  pops.trim <- stringr::str_replace_all(pops.trim, "Puget Sound", "PS")
  pops.trim <- stringr::str_replace_all(pops.trim, "Middle Fork", "MF")
  pops.trim <- stringr::str_replace_all(pops.trim, "South Fork", "SF")
  pops.trim <- stringr::str_replace_all(pops.trim, "Strait of Juan de Fuca", "SJF")
  pops.trim <- stringr::str_replace_all(pops.trim, "Upper", "Up.")
  pops.trim <- stringr::str_replace_all(pops.trim, "Lower", "Low.")
  pops.trim <- stringr::str_replace_all(pops.trim, "Creeks", "Cr.")
  pops.trim <- stringr::str_replace_all(pops.trim, "Creek", "Cr.")
  pops.trim <- stringr::str_replace(pops.trim, "Tributaries", "Tribs.")
  pops.trim <- stringr::str_replace(pops.trim, " And ", "/")
  pops.trim <- stringr::str_replace(pops.trim, " and ", "/")
  pops.trim <- stringr::str_replace(pops.trim, "Eastern", "E.")
  pops.trim
}
