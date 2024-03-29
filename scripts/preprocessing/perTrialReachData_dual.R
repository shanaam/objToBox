## --------------------------------
##
## Script name: perTrialReachData_dual.R
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
rm(list = ls())
library(data.table)
library(tidyverse)

## setup
path <- "data/raw/moveObject_dual/complete"

## functions
# given a vector, find the euclidian normal (magnitude)
NormVec <- function(vec) sqrt(sum(vec^2))

# given a row of data (as a df), returns a vector with x,y,z positions that are adjusted for hompePosition
PositionVec <- function(dfRow, homePositionLoc)
  return (c(dfRow$pos_x - homePositionLoc$x, dfRow$pos_y - homePositionLoc$y, dfRow$pos_z - homePositionLoc$z))

# given a 3-D vector, find spherical (magnitude, theta, phi)
SphericalVec <- function(vec){
  magnitude <- NormVec(vec)
  
  # theta <- acos(vec[1] / magnitude) * 180/pi                # the 180/pi is to turn radians to degrees
  theta <- (atan2(vec[3], vec[1]) * 180/pi) %% 360             # abs is a bad way to do it because sometimes, the direction is negative... This is a problem with the coordinate frames unity uses vs what I am expecting. Work out the math on paper
  
  # phi <- acos(vec[1] / NormVec(vec[1:2])) * 180/pi
  phi <- acos(vec[2] / magnitude) * 180/pi
  
  return(c(magnitude, theta, phi))
}

# given a row, find the distance from home position
DistanceFromHome <- function (dfRow, homePositionLoc){
  
  # first, subtract the home position to get a vector relative to the origin
  locationVector <- PositionVec(dfRow, homePositionLoc)
  
  # ONLY USE X + Z plane
  locationVector <- c(locationVector[1], locationVector[3])
  
  # find the length (magnitude, or euclidian norm) of the vector
  distance <- NormVec(locationVector)
  
  return(distance)
}

find3cmTheta <- function(reachDF, startPoint){
  for(i in 1:nrow(reachDF)) {
    row <- reachDF[i,]
    
    # do stuff with row
    # if distance of the row (minus home) is greather than 3cm...
    if (DistanceFromHome(row, startPoint) >= 0.03){
      # get position vector from the row
      positionVec <- PositionVec(row, startPoint)
      
      # get the spherical of the vector
      trialSpherical <- SphericalVec(positionVec)
      
      return(trialSpherical[2])
    }
  }
}

findcmTime <- function(reachDF, startPoint){
  for(i in 1:nrow(reachDF)) {
    row <- reachDF[i,]
    
    # do stuff with row
    # if distance of the row (minus home) is greather than 1cm...
    if (DistanceFromHome(row, startPoint) >= 0.01){
      # get time from the row
      eventTime <- as.numeric(row[1, 1])
      
      return(eventTime)
    }
  }
}

applyAngDev <- function(trialDFRow, pptAllReaches){
  # important columns
  # 21 = stepTime, 9 = endTime,
  # 23 = distractor_loc, 10 = targetAngle
  
  relevantReach <-
    pptAllReaches %>%
    filter(time >= as.numeric(trialDFRow[21]) + 0.015, time <= as.numeric(trialDFRow[9]))
  # the +0.015 removes the first frame where the obj tracker is at 0,0,0
  
  # find the "correct angle"
  correctAngle <- as.numeric(trialDFRow[10])
  
  
  startPoint <- list('x' = as.numeric(trialDFRow[24]), 'y' = as.numeric(trialDFRow[27]), 'z' = as.numeric(trialDFRow[25]))
  
  # find angle 3cm away.
  reachAngle_3cm <- find3cmTheta(relevantReach, startPoint)
  
  # get angular dev
  angular_dev <- reachAngle_3cm - correctAngle
  
  # if (as.numeric(trialDFRow[12]) < 0){
  #   angular_dev <- angular_dev * -1
  # }
  
  return(angular_dev)
}

#This should be the point at which the hand crosses a certain threshold for each trial
applyStartRT <- function(trialDFRow, pptAllReaches){
  # important columns
  # 8 = startTime, 21 = stepTime, 9 = endTime
  # 18, 19, 20 = dock x, y, z
  # 23 = distractor_loc, 10 = targetAngle
  
  relevantReach <-
    pptAllReaches %>%
    filter(time >= as.numeric(trialDFRow[8]) + 0.015, time <= as.numeric(trialDFRow[21]))
  # the +0.015 removes the first frame where the obj tracker is at 0,0,0
  
  # the starting position (dock)
  startPoint <- list('x' = as.numeric(relevantReach$pos_x[1]), 
                     'y' = as.numeric(relevantReach$pos_y[1]), 
                     'z' = as.numeric(relevantReach$pos_z[1]))
  
  # find time at 1cm away.
  time_cm <- findcmTime(relevantReach, startPoint)
  
  # get the relative time
  startRT <- time_cm - as.numeric(as.numeric(relevantReach$time[1]))
  
  return(startRT)
}

applyStepRT <- function(trialDFRow, pptAllReaches){
  # important columns
  # 8 = startTime, 21 = stepTime, 9 = endTime
  # 18, 19, 20 = dock x, y, z
  # 23 = distractor_loc, 10 = targetAngle
  
  relevantReach <-
    pptAllReaches %>%
    filter(time >= as.numeric(trialDFRow[21]), time <= as.numeric(trialDFRow[9]))
  
  # the starting position (dock)
  startPoint <- list('x' = as.numeric(relevantReach$pos_x[1]), 
                     'y' = as.numeric(relevantReach$pos_y[1]), 
                     'z' = as.numeric(relevantReach$pos_z[1]))
  
  # find time at 1cm away.
  time_cm <- findcmTime(relevantReach, startPoint)
  
  # get the relative time
  stepRT <- time_cm - as.numeric(as.numeric(relevantReach$time[1]))
  
  return(stepRT)
}

applyPathLength <- function(trialDFRow, pptAllReaches){
  # important columns
  # 8 = startTime, 21 = stepTime, 9 = endTime
  # 18, 19, 20 = dock x, y, z
  # 23 = distractor_loc, 10 = targetAngle
  
  relevantReach <-
    pptAllReaches %>%
    filter(time >= as.numeric(trialDFRow[21]), time <= as.numeric(trialDFRow[9]))
  
  #make sure we start with a fresh pathLength
  pathLength <- 0
  
  #iterate over rows, starting at second
  for (i in 2:nrow(relevantReach)){
    point1 <- c(as.numeric(relevantReach[i-1, 2]), 
                as.numeric(relevantReach[i-1, 3]), 
                as.numeric(relevantReach[i-1, 4]))
    point2 <- c(as.numeric(relevantReach[i, 2]), 
                as.numeric(relevantReach[i, 3]), 
                as.numeric(relevantReach[i, 4]))
    mini_vector <- point2 - point1
    
    mini_distance <- NormVec(mini_vector)
    
    # add the small distance to pathlength
    pathLength = pathLength + mini_distance
  }
  
  return(pathLength)
}

# testing

# trialDF$theta <- apply(trialDF, MARGIN = 1, applyAngDev, pptAllReaches = allReaches)
## trialResultThetas
##----
makeTrialResultThetas <- function(){
  for (expVersion in list.files(path = path)){
    
    for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
      
      for (session in list.files(path = paste(path, expVersion, ppt, sep = '/'))){
        
        for(trackerTag in c("trackerholder")){
          
          # make a vector of filenames to load (these are entire paths)       
          fileToLoad <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                   pattern = glob2rx(paste("*",trackerTag,"*", sep = "")), 
                                   full.names = TRUE)
          
          # read the file
          allReaches <- fread(fileToLoad, stringsAsFactors = FALSE)
          
          trialDF <- fread(paste(path, expVersion, ppt, session, "trial_results.csv", sep = '/'))
          
          # remove instruction trials
          trialDF <- trialDF %>%
            filter(type != "instruction")
          
          # add an angular dev column to trialDF
          trialDF$theta <- apply(trialDF, MARGIN = 1, applyAngDev, pptAllReaches = allReaches)
          
          # add a start reaction time (startRT) column to trialDF
          trialDF$startRT <- apply(trialDF, MARGIN = 1, applyStartRT, pptAllReaches = allReaches)
          
          # add a step reaction time (startRT) column to trialDF
          trialDF$stepRT <- apply(trialDF, MARGIN = 1, applyStepRT, pptAllReaches = allReaches)
          
          # add a path length column to trialDF
          # trialDF$pathLength <- apply(trialDF, MARGIN = 1, applyPathLength, pptAllReaches = allReaches)
          
          
          # some basic outlier removal
          trialDF$theta[trialDF$theta >= 120 | trialDF$theta <= -120] <- NA
          
          fwrite(trialDF, file = paste(path, expVersion, ppt, session, "trial_results_theta.csv", sep = '/'))
        }
      }
      
      print(ppt)
    }
  }
}


## merge into one file
##----
makeAllReachesCSV <- function(){
  for (expVersion in list.dirs(path = path, recursive = FALSE)){
    
    allReachList <- list()
    i <- 1
    
    for (ppt in list.dirs(path = expVersion, recursive = FALSE)){
      
      for (session in list.dirs(ppt, recursive = FALSE)){
        fileToLoad <- paste(session, "trial_results_theta.csv", sep = '/')
        
        # read the file
        trialDF_theta <- fread(fileToLoad, stringsAsFactors = FALSE)
        
        # remove instruction trials
        trialDF_theta <- trialDF_theta %>%
          filter(type != "instruction")
        
        allReachList[[i]] <- trialDF_theta
        
        i <- i + 1
      }
    }
    
    complete_df <- do.call(rbind, allReachList)
    
    # remove unused columns
    complete_df <- complete_df %>%
      select(-ends_with("filename"),
             -starts_with("target_"),
             -starts_with("loc_"),
             -plane_setting,
             -experiment_mode,
             -directory,
             -experiment,
             -session_num,
             -cursor_rotation)
    
    fwrite(complete_df, file = paste(path, "all_reaches.csv", sep = '/'))
    
  }
}

## do
##----
makeTrialResultThetas()
makeAllReachesCSV()


## test plots
##----
# trial <- 125
# relevantReach <-
#   allReaches %>%
#   filter(time >= as.numeric(trialDF$start_time[trial]), 
#          time <= as.numeric(trialDF$end_time[trial]))
# 
# library(plotly)
# 
# plot_ly(x = relevantReach$pos_x, y = relevantReach$pos_z, z = relevantReach$pos_y, type = "scatter3d")



