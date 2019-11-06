## --------------------------------
##
## Script name: XXX.R
##
## Purpose of script: Use "complete" CSVs to get per trial data
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-11-06
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: Use complete files
##
## --------------------------------

## Load packages
library(data.table)
library(tidyverse)

## make into a function
# setup
path <- "data/raw/complete"


for (expVersion in list.files(path = path)){
  
  for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
    
    for (session in list.files(path = paste(path, expVersion, ppt, sep = '/'))){
      
      for(trackerTag in c("cursorobjecttracker")){
        
        # make a vector of filenames to load (these are entire paths)       
        fileToLoad <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                  pattern = glob2rx(paste("*",trackerTag,"*", sep = "")), 
                                  full.names = TRUE)
        
        # read the file
        allReaches <- fread(fileToLoad, stringsAsFactors = FALSE)
        
        
      }
    }
  }
}
        