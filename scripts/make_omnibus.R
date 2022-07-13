## --------------------------------
##
## Script name: make_omnibus.R
##
## Purpose of script: Make omnibus csv used for omnibus analysis
## and bl correction
##
## Author: Shanaa Modchalingam
##
## Date created: 2022-07-12
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: Uses complete files from the 3 object-to-box experiments
## These can be found in data/raw/
##
## --------------------------------

## ----
## Load packages
# Packages and required scripts
rm(list = ls()) # clean environment

source("src/helper_funcs.R")
source("src/df_mod_funcs.R")

library(data.table)
library(tidyverse)

## ----
## Functions
# function for making the omnibus thing
makeNoCurOmnibus <- function() {
    path <- "data/omnibus/"
    datalist <- list()
    i <- 1

    # make a list of files to use
    all_reaches_csvs <- list.files(
        path = path,
        pattern = "all_reaches_*",
        full.names = TRUE
    )

    # load and concatenate all the files listed in all_reaches_csvs
    for (file in all_reaches_csvs) {
        datalist[[i]] <- read.csv(file) %>%
            mutate(exp = gsub("_pl.csv", "", gsub("data/omnibus/all_reaches_", "", file)))

        i <- i + 1
    }

    # bind the dataframes together
    omnibus_obj_to_box <- do.call(rbind, datalist)

    # save the omnibus df
    fwrite(omnibus_obj_to_box, file = "data/omnibus/omnibus_obj_to_box.csv")
}

makeNoCurOmnibus()