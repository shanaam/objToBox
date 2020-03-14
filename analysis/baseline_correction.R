## --------------------------------
##
## Script name: baseline_correction_dual.R
##
## Purpose of script: Use "complete" CSVs to get per trial data
##
## Author: Shanaa Modchalingam
##
## Date created: 2020-03-09
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

##----
## make a baseline file
make_blsummaries <- function(){ 
  
  baseline_summarries <- omnibus_data %>%
    filter(block_num < 6) %>%
    group_by(ppid, type, obj_shape, targetAngle) %>%
    summarise(mean_theta = mean(theta, na.rm = TRUE), 
              sd = sd(theta, na.rm = TRUE), 
              ci = vector_confint(theta), 
              n = n(), 
              median_theta = median(theta, na.rm = TRUE)) 
  
  baseline_summarries$type <- baseline_summarries$type %>%
    recode(aligned = "reach")
  
  return(baseline_summarries)
}

# apply bl correction
apply_blcorrection <- function(rot_data_row, bl_df){
  
  # make sure input is in format: ppt, type, object, target, theta
  
  bl <- filter(bl_df,
               ppid == as.numeric(rot_data_row[1]),
               type == rot_data_row[2],
               obj_shape == rot_data_row[3],
               targetAngle == as.numeric(rot_data_row[4]))$mean_theta %>% 
    mean(na.rm = TRUE)
  
  corrected_dev <- as.numeric(rot_data_row[5]) - bl
  
  return(corrected_dev)
}

apply_normalize_theta <- function(rot_data_row){
  if (rot_data_row[1] == "sphere"){
    theta <- as.numeric(rot_data_row[2]) * -1
  }
  else
    theta <- as.numeric(rot_data_row[2])
  
  return (theta)
}

apply_dummy <- function(rot_data_row){
  ## Input: ppid, trial_num, type, norm_theta
  if(rot_data_row[3] == "rotated"){
    if(as.numeric(rot_data_row[2]) == 76)
      return(as.factor(1))
    else if(as.numeric(rot_data_row[2]) <= 79)
      return(as.factor(2))
    else if(as.numeric(rot_data_row[2]) >= 440 & as.numeric(rot_data_row[2]) <= 442)
      return(as.factor(3))
    else 
      return(as.factor("N"))
  }
  else {
    if(as.numeric(rot_data_row[2]) >= 468)
      return(as.factor("E"))
    else if(as.numeric(rot_data_row[2]) >= 443)
      return(as.factor("I"))
    else
      return(as.factor("X"))
  }
}

# do
omnibus_path <- "data/moveObject_dual/complete/all_reaches.csv"

omnibus_data <- loadData(omnibus_path)

# make functions and move up if possible
# this should have summary stats (per ppt, type, object, target)
baseline_summarries <- make_blsummaries()

rot_data <- omnibus_data %>%
  filter(block_num >= 6)

rot_data$type_new <- rot_data$type %>%
  recode(aligned = "reach", rotated = "reach")

rot_data$temp <- select(rot_data, ppid, type_new, obj_shape, targetAngle, theta) %>%
  apply(1, FUN = apply_blcorrection, bl_df = baseline_summarries)

rot_data$type_new <- NULL

# rename some columns
rot_data <- 
  rot_data %>% 
  rename(raw_theta = theta) %>%
  rename(theta = temp)

# normalize the data
rot_data$norm_theta <- select(rot_data, obj_shape, theta) %>%
  apply(1, FUN = apply_normalize_theta)

# REMOVE PPT 1 and 2

rot_data <- rot_data %>%
  filter(ppid != 1, ppid != 2)

# SAVE HERE

## Make JASPABLE (this doesn't seem to work)
# Add dummies for JASP
rot_data$dummy <- rot_data %>%
  select(ppid, trial_num, type, norm_theta) %>%
  apply(1, FUN = apply_dummy)

rot_data_JASP <- rot_data %>%
  group_by(ppid, dummy) %>%
  summarise(mean_dev = mean(norm_theta, na.rm = TRUE), 
            sd = sd(norm_theta, na.rm = TRUE), 
            ci = vector_confint(norm_theta), 
            n = n(), 
            median_dev = median(norm_theta, na.rm = TRUE)) 

rot_data_JASP <- rot_data_JASP %>%
  select(ppid,dummy, mean_dev) %>%
  pivot_wider(names_from = dummy, values_from = mean_dev) %>%
  select(-N, -X)

fwrite(rot_data_JASP, file = paste("data/wide_format", "blocked_JASP_broken.csv", sep = '/'))

# make excellable
# make column data (pivot)

rot_data_wide_reaches <- rot_data %>%
  select(ppid, trial_num, block_num, type, norm_theta) %>%
  filter(type == "rotated") %>%
  pivot_wider(names_from = ppid, values_from = norm_theta)

# SAVE HERE
fwrite(rot_data_wide_reaches, file = paste("data/wide_format", "reaches.csv", sep = '/'))

# do the same for instructed, and non-instructed data
rot_data_wide_clamped <- rot_data %>%
  select(ppid, block_num, trial_num_in_block, trial_num, type, obj_shape, norm_theta) %>%
  filter(type == "clamped") %>%
  pivot_wider(names_from = ppid, values_from = norm_theta) %>%
  arrange(block_num, obj_shape, trial_num_in_block)

fwrite(rot_data_wide_clamped, file = paste("data/wide_format", "clamped.csv", sep = '/'))

