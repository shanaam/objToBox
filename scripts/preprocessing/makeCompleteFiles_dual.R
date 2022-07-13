## --------------------------------
##
## Script name: makeCompleteFiles.R
##
## Purpose of script: Make stitched CSVs -- one for trackerholder (the real hand), 
## and one for cursorObject (the object)
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-10-29
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: For each participant, make a "complete" file for hand, and one for object
##
## --------------------------------

## Load packages
library(data.table)
library(tidyverse)

## make into a function
# setup
# path <- "D:/shanaa/repos/unity-reachToTarget-remake/UFile-reachToTarget-remake/data"
path <- "data/raw/moveObject_dual"

for (expVersion in list.files(path = path)){
  try({dir.create(paste(path, "complete", sep = '/'))})
  dir.create(paste(path, "complete", expVersion, sep = '/'))
  
  for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
    
    dir.create(paste(path, "complete", expVersion, ppt, sep = '/'))
    
    for (session in list.files(path = paste(path, expVersion, ppt, sep = '/'))){
      dir.create(paste(path, "complete", expVersion, ppt, session, sep = '/'))
      
      for(trackerTag in c("cursorobjecttracker", "trackerholderobject")){
        
        # make a vector of filenames to load (these are entire paths)       
        filesToLoad <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                  pattern = glob2rx(paste("*",trackerTag,"*", sep = "")), 
                                  full.names = TRUE)
        
        datalist <- list()
        i <- 1
        
        # fill up datalist
        for (eachFilePath in filesToLoad){
          eachFile <- fread(eachFilePath, stringsAsFactors = FALSE)
          
          # save this one df to datalist
          datalist[[i]] <- eachFile
          
          i <- i+1
          
        }
        
        # save this
        complete_df <- do.call(rbind, datalist)
        
        
        fileName <- paste(trackerTag, "_complete.csv", sep = "")
        
        # print(head(complete_df))
        fwrite(complete_df, file = paste(path, "complete", expVersion, ppt, session, fileName, sep = '/'))
        
      }
      
      #copy over the trial results file
      trialResultPath <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                               pattern = glob2rx("*_results*"), 
                                               full.names = TRUE) 
      trialResult <- fread(trialResultPath, stringsAsFactors = FALSE)
      
      fwrite(trialResult, file = paste(path, "complete", expVersion, ppt, session, "trial_results.csv", sep = '/'))
    }
  }
}
