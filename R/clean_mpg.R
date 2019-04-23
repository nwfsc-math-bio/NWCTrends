# Utility function to clean up population names to make shorter
clean.mpg=function(pops){
  pops=as.character(pops)
  pops.trim=str_trim(pops)
  pops.trim=str_replace_all(pops.trim, " [/] ", "/")
  pops.trim=str_replace_all(pops.trim, " [-] ", "-")
  pops.trim=str_replace_all(pops.trim, "Rivers", "R.")
  pops.trim=str_replace_all(pops.trim, "River", "R.")
  pops.trim=str_replace_all(pops.trim, "Puget Sound", "PS")
  pops.trim=str_replace_all(pops.trim, "Middle Fork", "MF")
  pops.trim=str_replace_all(pops.trim, "South Fork", "SF")
  pops.trim=str_replace_all(pops.trim, "Strait of Juan de Fuca", "SJF")
  pops.trim=str_replace_all(pops.trim, "Upper", "Up.")
  pops.trim=str_replace_all(pops.trim, "Lower", "Low.")
  pops.trim=str_replace_all(pops.trim, "Creeks", "Cr.")
  pops.trim=str_replace_all(pops.trim, "Creek", "Cr.")
  pops.trim=str_replace(pops.trim, "Tributaries", "Tribs.")
  pops.trim=str_replace(pops.trim, " And ", "/")
  pops.trim=str_replace(pops.trim, " and ", "/")
  pops.trim=str_replace(pops.trim, "Eastern", "E.")
  pops.trim
}
