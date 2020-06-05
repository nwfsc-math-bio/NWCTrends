# custom fun to fix PS Chinook name ordering
sort.PSChinook <- function(x) {
  pop.ord <- c(
    "North Fork Nooksack",
    "South Fork Nooksack",
    "Elwha",
    "Dungeness",
    "Skokomish",
    "Mid-Hood Canal",
    "Skykomish",
    "Snoqualmie",
    "North Fork Stillaguamish",
    "South Fork Stillaguamish",
    "Upper Skagit",
    "Lower Skagit",
    "Upper Sauk",
    "Lower Sauk",
    "Suiattle",
    "Cascade",
    "Sammamish",
    "Cedar",
    "Green",
    "Puyallup",
    "White",
    "Nisqually"
  )
  if (length(x) != length(pop.ord)) stop("Something wrong with sort names for PS Chinook")

  ord <- 1:length(pop.ord)
  for (i in 1:length(pop.ord)) ord[i] <- which(stringr::str_detect(x, pop.ord[i]))
  return(ord)
}
