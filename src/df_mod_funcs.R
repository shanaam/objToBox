##### Functions for modifyind dataframe



#####
# No Cursors

# THIS FUNCTION IS DEPRECATED (way too slow)
apply_blcorrection <- function(rot_df_row, bl_df) {
  # no cursor baseline correction
  # make sure input is in format: targetangle_deg, ppt, angular_dev

  bl <- filter(
    bl_df,
    targetangle_deg == rot_df_row[1] &
      ppt == rot_df_row[2]
  )$angular_dev %>%
    mean(na.rm = TRUE)



  corrected_dev <- as.numeric(rot_df_row[3]) - bl

  return(corrected_dev)
}

#
add_nocur_block_num <- function(df) {
  # input: df,
  # returns: int
  if (df[2] == "ramped") {
    if (df[1] < 20) {
      return(1)
    } else if (df[1] < 34) {
      return(2)
    } else if (df[1] < 46) {
      return(3)
    } else {
      return(4)
    }
  } else if (df[2] == "reintroExp") {
    if (df[1] < 25) {
      return(1)
    } else if (df[1] < 38) {
      return(2)
    } else if (df[1] < 51) {
      return(3)
    } else {
      return(4)
    }
  } else {
    if (df[1] < 25) {
      return(1)
    } else if (df[1] < 37) {
      return(2)
    } else if (df[1] < 49) {
      return(3)
    } else {
      return(4)
    }
  }
}

add_training_block_num <- function(df) {
  # given a df with experiment and block number
  # these if statements are repetitive at the moment.
  # Kept in case different experiments need different trials
  # input: df,
  # returns: int
  x <- as.double(df[1])
  if (df[2] == "ramped") {
    if (x <= 69) {
      return(0)
    } else if (x >= 127 & x <= 132) {
      return(1)
    } else if (x >= 193 & x <= 198) {
      return(2)
    } else if (x >= 259 & x <= 264) {
      return(3)
    } else if (x >= 325 & x <= 330) {
      return(4)
    } else {
      return(10)
    }
  } else if (df[2] == "stepped") {
    if (x <= 69) {
      return(0)
    } else if (x >= 127 & x <= 132) {
      return(1)
    } else if (x >= 193 & x <= 198) {
      return(2)
    } else if (x >= 259 & x <= 264) {
      return(3)
    } else if (x >= 325 & x <= 330) {
      return(4)
    } else {
      return(10)
    }
  } else { # for longAbrupt
    if (x <= 69) {
      return(0)
    } else if (x >= 127 & x <= 132) {
      return(1)
    } else if (x >= 193 & x <= 198) {
      return(2)
    } else if (x >= 259 & x <= 264) {
      return(3)
    } else if (x >= 325 & x <= 330) {
      return(4)
    } else {
      return(10)
    }
  }
}

add_trial_set <- function(df) {
  # given a df with experiment and trial sets
  # these if statements are repetitive at the moment. I've kept them in case different trials need to be isolated for different experiment protocols
  # input: df,
  # returns: int
  x <- as.double(df[1])
  if (df[2] == "ramped") {
    if (x <= 21) {
      return(1)
    } # first block
    else if (x >= 79 & x <= 84) {
      return(2)
    } # the end of training
    else if (x >= 385) {
      return(3)
    } else {
      return(10)
    }
  } else if (df[2] == "stepped") {
    if (x <= 3) {
      return(1)
    } else if (x >= 385) {
      return(3)
    } else {
      return(10)
    }
  } else { # for longAbrupt
    if (x <= 3) {
      return(1)
    } else if (x >= 79 & x <= 84) {
      return(2)
    } else if (x >= 385) {
      return(3)
    } else {
      return(10)
    }
  }
}

detailed_block_w_trial_num <- function(trial_num) {
  # input: int (trial_num),
  # returns: float (detailed block num)
  if (trial_num <= 63) {
    return(1.1)
  } else if (trial_num <= 102) {
    return(1.2)
  } else if (trial_num <= 165) {
    return(2.1)
  } else if (trial_num <= 204) {
    return(2.2)
  } else if (trial_num <= 267) {
    return(3.1)
  } else if (trial_num <= 306) {
    return(3.2)
  } else if (trial_num <= 369) {
    return(4.1)
  } else if (trial_num <= 408) {
    return(4.2)
  }
}