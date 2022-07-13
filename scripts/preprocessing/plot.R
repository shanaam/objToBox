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

# setup

allDataPath <- "data/raw/complete/all_reaches.csv"

allReaches <- read.csv(allDataPath)


means <- allReaches[ , 7:38] %>%
  apply( 1, mean, na.rm = TRUE)

ci_95 <- allReaches[ , 7:38] %>%
  apply( 1, vector_confint)

allReaches <- cbind(allReaches, means, ci_95) %>%
  filter(block_num > 5)

allReaches$trial_num <- allReaches$trial_num - 47
allReaches$means<- allReaches$means * -1

ncReaches <- allReaches %>%
  filter(type == "clamped", block_num > 15)


nc_1R <- ncReaches %>%
  filter(hand == "r", obj_shape == 1)

nc_1L <- ncReaches %>%
  filter(hand == "l", obj_shape == 1)

nc_2R <- ncReaches %>%
  filter(hand == "r", obj_shape == 2)

nc_2L <- ncReaches %>%
  filter(hand == "l", obj_shape == 2)

nc_df <- data.frame(ppt = 1:32)


nc_df$o1r <- nc_1R[ , 7:38] %>%
  apply( 2, mean, na.rm = TRUE)
nc_df$o2r <- nc_2R[ , 7:38] %>%
  apply( 2, mean, na.rm = TRUE)
nc_df$o1l <- nc_1L[ , 7:38] %>%
  apply( 2, mean, na.rm = TRUE)
nc_df$o2l <- nc_2L[ , 7:38] %>%
  apply( 2, mean, na.rm = TRUE)

nc_df$o1r <- nc_df$o1r * -1
nc_df$o2r <- nc_df$o2r * -1
nc_df$o1l <- nc_df$o1l * -1
nc_df$o2l <- nc_df$o2l * -1

nc_summary <- data.frame(objhand = c("R Obj1", "R Obj2", "L Obj1", "L Obj2"),
                         dummy = 1:4,
                         mean = c(mean(nc_df$o1r), mean(nc_df$o2r), mean(nc_df$o1l), mean(nc_df$o2l)),
                         ci = c(vector_confint(nc_df$o1r), vector_confint(nc_df$o2r), vector_confint(nc_df$o1l), vector_confint(nc_df$o2l)))




#plot
#LC
s <- ggplot(filter(allReaches, type != "clamped"), aes(x = trial_num, y = means)) +
  theme_minimal() +
  geom_point(stat = "identity", colour = "#e51636", fill = "#e51636", size = 4) +
  geom_smooth(aes(ymin = means - ci_95, ymax = means + ci_95), 
             stat = "identity", colour = "none", fill = "#e51636", size = 1) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(-10, 35), breaks = c(0, 15, 30, 45), name = "hand deviation (°)") +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB"))

s

# ggsave(s, height = 14, width = 24, device = "svg", filename = "data/lc_plot.svg")

#clamped
p <- ggplot(nc_summary, aes(x <- dummy, y = mean, colour = objhand)) +
  theme_minimal() +
  geom_point(stat = "identity", size = 8) +
  geom_linerange(aes(ymin =mean - ci, ymax = mean + ci), lwd = 20, alpha = 0.4) +
  geom_beeswarm(data = nc_df, 
                aes(x = 1.3, y = o1r), size = 4, alpha = 0.6, colour ="#e51636") +
  geom_beeswarm(data = nc_df, 
                aes(x = 2.3, y = o2r), size = 4, alpha = 0.6, colour = "#ff8000") +
  geom_beeswarm(data = nc_df, 
                aes(x = 3.3, y = o1l), size = 4, alpha = 0.6, colour = "#0500a0") +
  geom_beeswarm(data = nc_df, 
                aes(x = 4.3, y = o2l), size = 4, alpha = 0.6, colour = "#037fc4") +
  scale_x_continuous(name = NULL, limits = c(0.5, 4.5), breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-10, 35), breaks = c(0, 15, 30, 45), name = "hand deviation (°)") +
  scale_color_manual(values=c("#0500a0", "#037fc4", "#e51636", "#ff8000")) +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=40), panel.grid.major.y = element_line(colour = "#ABABAB"))

p

# ggsave(p, height = 14, width = 24, device = "svg", filename = "data/clamp_plot.svg")

