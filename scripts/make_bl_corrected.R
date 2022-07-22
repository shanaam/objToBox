## --------------------------------
##
## Script name: make_bl_corrected.R
##
## Purpose of script: Make a baseline corrected csv for data analysis
##
## Author: Shanaa Modchalingam
##
## Date created: 2022-07-13
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: Uses omnibus csv from make_omnibus.R
## found in data/omnibus/
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


## ----
## Main script
# load in the omnibus csv
# Cursor data
omnibus_df <- read_delim("data/omnibus/omnibus_obj_to_box.csv",
    delim = ",",
    col_types = cols(
        .default = col_double(),
        ppid = col_factor(),
        targetAngle = col_factor(),
        type = col_factor(),
        hand = col_factor(),
        obj_shape = col_factor(),
        distractor_loc = col_factor(),
        exp = col_factor()
    )
)

# filter out unused experiment groups
# emppty for now

# Note: group names are single, dual, and dual_60

# make unique ids for participants by combining ppid and exp
# remove the ppid column from the dataframe
omnibus_df <- omnibus_df %>%
    mutate(ppt = paste0(ppid, "_", exp)) %>%
    select(-ppid)

# separate rotated and bl reaches
# rotated starts at block 7 for dual, at 16 for single
bl_df <- omnibus_df %>%
    filter(((exp == "dual" | exp == "dual_60") & block_num < 7) |
        (exp == "single" & block_num < 16)) %>%
    filter(trial_num > 7)

rot_df <- omnibus_df %>%
    filter(((exp == "dual" | exp == "dual_60") & block_num >= 7) |
        (exp == "single" & block_num >= 16))

# apply baseline correction
# make bl_summary first (faster than doing this in apply)

bl_summary <- bl_df %>%
    group_by(ppt, targetAngle, type, hand, obj_shape) %>%
    summarise(bl_per_target = mean(theta), .groups = "drop") %>%
    mutate(type = recode(type,
        "aligned" = "rotated",
        .default = "clamped"
    ))

# add summary to the rotated dataframe
rot_df <- rot_df %>%
    left_join(bl_summary, by = c(
        "ppt", "targetAngle",
        "type", "hand", "obj_shape"
    )) %>%
    mutate(temp = theta - bl_per_target) %>%
    select(-bl_per_target)

# rename some columns
rot_df <- rot_df %>%
    rename(raw_angular_dev = theta) %>%
    rename(angular_dev = temp)

## ----
## Post-processing (e.g. trial sets)


## ----
## Save the data
fwrite(rot_df, file = "data/bl_corrected/bl_corrected_data.csv")
