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

allDataPath <- "data/moveObject_dual/complete/A_all_reaches.csv"

allReaches <- fread(allDataPath)

aligned <- allReaches %>%
  filter(type == "aligned") %>%
  group_by(trial_num) %>%
  summarise(mean_devs = mean(theta, na.rm = TRUE), 
            sd = sd(theta, na.rm = TRUE), 
            ci = vector_confint(theta), 
            n = n())

neg_rot <- allReaches %>%
  filter(dual_rotation < 0) %>%
  group_by(trial_num) %>%
  summarise(mean_devs = mean(theta, na.rm = TRUE), 
            sd = sd(theta, na.rm = TRUE), 
            ci = vector_confint(theta), 
            n = n())

pos_rot <- allReaches %>%
  filter(dual_rotation > 0) %>%
  group_by(trial_num) %>%
  summarise(mean_devs = mean(theta, na.rm = TRUE), 
            sd = sd(theta, na.rm = TRUE), 
            ci = vector_confint(theta), 
            n = n())

clamped <- allReaches %>%
  filter(type == "clamped") %>%
  group_by(obj_shape, ppid) %>%
  summarise(mean_devs = mean(theta, na.rm = TRUE), 
            sd = sd(theta, na.rm = TRUE), 
            ci = vector_confint(theta), 
            n = n())

clamped_summary <- clamped %>%
  group_by(obj_shape) %>%
  summarise(mean = mean(mean_devs, na.rm = TRUE), 
            sd = sd(mean_devs, na.rm = TRUE), 
            ci = vector_confint(mean_devs), 
            n = n())




# make summary files (by trial)

#plot
#LC
s <- ggplot(data = aligned, aes(x = trial_num, y = mean_devs)) +
  theme_minimal() +
  geom_point(data = aligned, stat = "identity", size = 4) +
  geom_smooth(data = aligned, aes(ymin = mean_devs - ci, ymax = mean_devs + ci), 
             stat = "identity", colour = "none", fill = "#e51636", size = 1) +
  geom_point(data = neg_rot, stat = "identity", size = 4, colour = "#e51636") +
  geom_smooth(data = neg_rot, aes(ymin = mean_devs - ci, ymax = mean_devs + ci), 
              stat = "identity", colour = "none", fill = "#e51636", size = 1) +
  geom_point(data = pos_rot, stat = "identity", size = 4, colour = "#ff8000") +
  geom_smooth(data = pos_rot, aes(ymin = mean_devs - ci, ymax = mean_devs + ci), 
              stat = "identity", colour = "none", fill = "#ff8000", size = 1) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(-60, 60), 
                     breaks = c(-45, -30, -15, 0, 15, 30, 45), name = "hand deviation (°)") +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB"))

s

ggsave(s, height = 14, width = 24, device = "svg", filename = "data/dual_lc_plot.svg")

#clamped
p <- ggplot(clamped, aes(x = obj_shape, y = mean_devs, colour = obj_shape)) +
  theme_minimal() +
  geom_point(data = clamped_summary, aes(y = mean), stat = "identity", size = 8) +
  geom_linerange(data = clamped_summary, 
                 aes(y = mean, ymin = mean - ci, ymax = mean + ci), lwd = 20, alpha = 0.4) +
  geom_beeswarm(size = 4, alpha = 0.6) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "hand deviation (°)") +
  #scale_color_manual(values=c("#0500a0", "#037fc4", "#e51636", "#ff8000")) +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=40), panel.grid.major.y = element_line(colour = "#ABABAB"))

p

ggsave(p, height = 14, width = 24, device = "svg", filename = "data/dual_clamp_plot.svg")

