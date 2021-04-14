Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Rtools", "C:\\Rtools\\mingw64\\bin", "C:\\Rtools\\usr\\bin", sep = ";"))

usethis::use_mit_license()

usethis::use_package("utils")
usethis::use_package("rlang")
usethis::use_package("httr")
usethis::use_package("openssl")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("jsonlite")
usethis::use_package("XML")

usethis::use_build_ignore("devtools_history.R")
usethis::use_build_ignore("sandbox")

usethis::use_testthat()
