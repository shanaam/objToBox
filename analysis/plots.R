## --------------------------------
##
## Script name: perTrialReachData_dual.R
##
## Purpose of script: Use "complete" CSVs AND the big reach DFs to plot
##
## Author: Shanaa Modchalingam
##
## Date created: 2021-06-22
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
source("analysis/analysisFunctions.R")

library("plot3D")
library("plot3Drgl")
library("ggpubr")

##### 
# First, we plot paths
#####
# group <- "dual_60"
group <- "dual"
expVersion <- "A"
ppt <- "17"
trial <- 345
# load in trial data
#####
plot3Dreach <- function(group, expVersion, ppt, trial){
  complete_path <- paste("data/moveObject_", group,"/complete/all_reaches_", group,"_pl.csv", sep = "")
  
  trialDF <- loadData(complete_path) %>%
    filter(ppid == ppt)
  
  # load in the reach for ONE participant
  
  fileToLoad <- paste("data/moveObject_", group, "/complete/", expVersion,"/", ppt,"/S001/trackerholderobject_complete.csv", sep = "")
  
  # read the file
  pptReaches <- fread(fileToLoad, stringsAsFactors = FALSE) %>%
    select(time, pos_x, pos_y, pos_z)
  
  # isolate one reach
  relevantReach <-
    pptReaches %>%
    filter(time >= as.numeric(trialDF$start_time[trial]), time <= as.numeric(trialDF$end_time[trial]))
  # the +0.015 removes the first frame where the obj tracker is at 0,0,0
  
  # the starting position (dock)
  startPoint <- c(as.numeric(relevantReach$pos_x[1]), as.numeric(relevantReach$pos_y[1]), as.numeric(relevantReach$pos_z[1]))
  
  
  relevantReach$pos_x <- lapply(relevantReach$pos_x, apply_subtraction, homePos = startPoint[1])
  relevantReach$pos_y <- lapply(relevantReach$pos_y, apply_subtraction, homePos = startPoint[2])
  relevantReach$pos_z <- lapply(relevantReach$pos_z, apply_subtraction, homePos = startPoint[3])
  
  relevantReach$dist_on_plane <- apply(relevantReach, MARGIN = 1, FUN = apply_reachDF_norm_vec)
  
  relevantReach <- relevantReach %>%
    mutate(across(as.double()))
  
  #####
  # plot
  #####
  # first, the 3D plot
  
  scatter3D(as.numeric(relevantReach$pos_x) * 100, 
            as.numeric(relevantReach$pos_z) * 100, 
            as.numeric(relevantReach$pos_y) * 100, 
            phi = 0, bty = "g",  type = "h", 
            ticktype = "detailed", pch = 19, cex = 0.5,
            main = "3D reach", xlab = "x (cm)",
            ylab ="y (cm)", zlab = "z (cm)", 
            col = ramp.col(c("blue", "red")), colkey = FALSE, scale = FALSE)
  
  # show 3D plot in new window
  plotrgl()
}

# make some plots Note: 73 is the first rotated row

for (i in 70:72){
  plot3Dreach(group = "dual", expVersion = "A", ppt = "17", trial = i)
}


# a gg plot thing
# p <- relevantReach %>%
#   ggplot (aes(as.numeric(pos_x) * 100, 
#               as.numeric(pos_z) * 100, 
#               colour = as.numeric(pos_y) * 100)) +
#   geom_point(size = 1, alpha = 1) + 
#   scale_x_continuous(name = "x (cm)") +
#   scale_y_continuous(name = "y (cm)") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major.y = element_line(colour = "#CCCCCC")) +
#   NULL
# 
# p

# using the ggpubr package
x_y <- ggscatter(relevantReach, 
                x = "pos_x", 
                y = "pos_z")

dist_height <- ggscatter(relevantReach, 
                x = "dist_on_plane", 
                y = "pos_y")
  
ggarrange(x_y, dist_height + font("x.text", size = 10),
          labels = c("A", "B"),
          ncol = 2, nrow = 1)




# for looping through participants
for (expVersion in list.files(path = path)){
  
  for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
    
    for (session in list.files(path = paste(path, expVersion, ppt, sep = '/'))){
      
      for(trackerTag in c("trackerholder")){
        
        # make a vector of filenames to load (these are entire paths)       
        fileToLoad <- list.files(path = paste(path, expVersion, ppt, session, sep = '/'), 
                                 pattern = glob2rx(paste("*",trackerTag,"*", sep = "")), 
                                 full.names = TRUE)
        
        # read the file
        pptReaches <- fread(fileToLoad, stringsAsFactors = FALSE)
      }
    }
  }
}

# Testing
#####
ppt <- 1
expVersion <- "A"
