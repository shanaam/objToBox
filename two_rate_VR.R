library(tidyverse)
library(ggbeeswarm)

vector_confint <- function(vector, interval = 0.95) {
  # Standard deviation of sample
  vec_sd <- sd(vector, na.rm = TRUE)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector, na.rm = TRUE)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  # result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(error)
}


allDataPath <- "data/TwoRateVR/complete/all_reaches_abrupt.csv"

allReaches <- read.csv(allDataPath)

means <- allReaches[ , 8:39] %>%
  apply( 1, mean, na.rm = TRUE)

ci_95 <- allReaches[ , 8:39] %>%
  apply( 1, vector_confint)

allReaches <- cbind(allReaches, means, ci_95)

allReaches$trial_num <- 1:164

#plot
lc <- ggplot(allReaches, aes(x = trial_num, y = means)) +
  theme_minimal() +
  geom_point(stat = "identity", colour = "#e51636", fill = "#e51636", size = 4) +
  geom_smooth(aes(ymin = means - ci_95, ymax = means + ci_95), 
              stat = "identity", colour = "none", fill = "#e51636", size = 1) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(-40, 40), breaks = c(-30, -15, 0, 15, 30), name = "hand deviation (°)") +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB"))

lc
ggsave(lc, height = 14, width = 24, device = "svg", filename = "data/abrupt_2rate_plot.svg")


#GRADUAL
allDataPath <- "data/TwoRateVR/complete/all_reaches_gradual.csv"

allReaches <- read.csv(allDataPath)

means <- allReaches[ , 8:39] %>%
  apply( 1, mean, na.rm = TRUE)

ci_95 <- allReaches[ , 8:39] %>%
  apply( 1, vector_confint)

allReaches <- cbind(allReaches, means, ci_95)

allReaches$trial_num <- 1:164

#plot
lc <- ggplot(allReaches, aes(x = trial_num, y = means)) +
  theme_minimal() +
  geom_point(stat = "identity", colour = "#e51636", fill = "#e51636", size = 4) +
  geom_smooth(aes(ymin = means - ci_95, ymax = means + ci_95), 
              stat = "identity", colour = "none", fill = "#e51636", size = 1) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(-40, 40), breaks = c(-30, -15, 0, 15, 30), name = "hand deviation (°)") +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB"))

lc
ggsave(lc, height = 14, width = 24, device = "svg", filename = "data/gradual_2rate_plot.svg")


