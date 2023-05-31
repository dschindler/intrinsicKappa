## set working directory
in_folder = TRUE

if(!in_folder){
  setwd("C:\\Users\\dschind2\\Desktop\\David\\Git\\intrinsicKappa")
} else {
  setwd("C:\\Users\\dschind2\\Desktop\\David\\Git\\intrinsicKappa\\intrinsicKappa")
}

## required packages
library(knitr)
library(devtools)
library(testthat)
require(rmarkdown)
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

if(in_folder) {
  devtools::build_vignettes()
}
  

# 
# library(pkgdown)
# pkgdown::build_site()

# for new packages use this
# create_package("intrinsicKappatest")

## specifications of the package
#as.package("./intrinsicKappa")

# sources all R code in R/, removes and reloads S4 class and method definitions,
if(!in_folder){
  load_all("./intrinsicKappa")
} else {
  load_all()
}  

## testing the package
#test("./intrinsicKappa")
##  converts inline roxygen document blocks to R's standard Rd files in the man/ directory
if(!in_folder){
  document("./intrinsicKappa")
} else {
  document()
}

## automative test suite for packages 
## if you want to release the package, you must pass this check (!!!)
if(!in_folder){
  check("./intrinsicKappa")
} else {
  check()
}


## building the package
if(!in_folder){
  build("./intrinsicKappa")
} else {
    build()
}

# generate manual
if(!in_folder){
  if (file.exists("./intrinsicKappa.pdf")) file.remove("./intrinsicKappa.pdf")
  system(paste('R CMD Rd2pdf ',  './intrinsicKappa'))
} else {
  if (file.exists("./../intrinsicKappa.pdf")) file.remove("./../intrinsicKappa.pdf")
  system(paste('R CMD Rd2pdf ',  './../intrinsicKappa'))
}


# Removing package
detach("package:intrinsicKappa", unload = TRUE)
## install the package
install.packages("C:/Users/DSCHIND2/Desktop/David/Git/intrinsicKappa/intrinsicKappa_0.1.tar.gz", repos = NULL, type = "source")

library(intrinsicKappa)

