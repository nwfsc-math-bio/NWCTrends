# Utility function to read in a esu choice from a list
choose.esu <- function(esu.names) {
  done <- FALSE
  while (!done) {
    nesus <- length(esu.names)
    cat(
      paste(
        "All ESUs in the data:\n",
        paste(1:nesus, ". ", esu.names, collapse = "\n ", sep = "")
      ), "\n"
    )
    esu.choice <- readline("Choose the ESU(s) by number: \n Enter single number or\n separate numbers by commas or 0 for all. ")
    esu.choice <- as.numeric(stringr::str_split(esu.choice, ",")[[1]])
    if (all(esu.choice %in% (0:nesus))) {
      if (identical(esu.choice, 0)) esu.choice <- 1:nesus
      done <- TRUE
    }
  }
  return(esu.choice)
}
