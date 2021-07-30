# NWCTrends

# Version 1.27

I was overloading the `inst/doc` folder with non-vignette material. I moved the needed report files (Rmd and tex) to `inst/report_files` and the demo data to `inst/extdata`. The `inst/doc` folder now only has the two vignettes and the documentation PDFs. 

I also updated the `vignettes` folder such that if `install_local()` or `install_github()` with `build_vignettes=TRUE` is used, the `inst/doc` created will match that on GitHub. Note, the user would have to accept `inst/doc` will be delected. It doesn't delete the files in the repo, just re-creates `inst/doc` in the installed version in the user library.

I added a file `local_build_instructions.txt` to the repo to remind me how to build the vignettes and how to deal with `devtools::check()` deleting the `inst/doc` folder.

# Version 1.26

Added a data object `nwctrends.pallete` with red, blue, black, green and white defined (in a list). To change the colors, the user a new palette list to `NWCTrends()`

Added `show.all.fracwild` to `NWCTrends_report()` so that that can be changed from the main function.

# Version 1.25
This is the version used in the 2020 NWFSC PNW Salmonid Viability Report. 

Add more meta data: Method (Survey, Model), Citation, and Contributor. A `metadata.csv` file is added to each Output folder.

## Minor

* Added more info to `pkgname.R` Roxygen2 code.
* Added a `NEWS.md` file

# Version 1.0
This is the version used in the 2015 NWFSC PNW Salmonid Viability Report. 


