## ---- include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----demo, echo=FALSE-------------------------------------------------
fil <- system.file("extdata", "Demo_NWCTrends.csv", package="NWCTrends")
cat(readLines(fil, n=5), sep="\n")

