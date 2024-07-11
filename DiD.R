#importing and cleaning data
#loading packages
library(tidyverse)
library(broom)
install.packages("modelsummary")
library(scales)
library(modelsummary)
library(dplyr)

injury_raw <- read_csv("C:/Users/HP/Desktop/Ecotrix Projects/injury.csv")

injury <- injury_raw %>%
  filter(ky == 1) %>%
  rename(after_1980 = afchnge, 
         duration = durat, 
         log_duration = ldurat)

#EXPLORING DATA ANALYSIS
ggplot(injury, mapping = aes(x = log_duration)) + 
  geom_histogram(binwidth = 0.5, color = "white", boundary = 0) +
  scale_x_continuous(labels = trans_format("exp", format = round)) +
  facet_wrap(vars(highearn))


ggplot(injury, mapping = aes(x = highearn, y = log_duration)) + 
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(vars(after_1980))
#there is quite substantial difference between highearn and lowearns
#we can clearly see the diff in diff plot
#join the lower dots and the upper dots
#the diff in what actually happened and what happened is the causal effect


#MANUAL DIFF IN DIFF
#we need to find four numbers before treatment and after treatment, before control and after control
diffs <- injury %>% 
  group_by(after_1980, highearn) %>% 
  summarize(mean_duration = mean(log_duration))

#BEFORE 1980 HIGH EARNERS = 1.38
#We are going to extract these numbers from diff dataset

before_treatment <- diffs %>% 
  filter(after_1980 == 0, highearn == 1) %>% 
pull(mean_duration)

before_control <- diffs %>% 
  filter(after_1980 == 0, highearn == 0) %>% 
  pull(mean_duration)

after_treatment <- diffs %>% 
  filter(after_1980 == 1, highearn == 1) %>% 
  pull(mean_duration)

after_control <- diffs %>% 
  filter(after_1980 == 1, highearn == 0) %>% 
  pull(mean_duration)
  
diff_treatment_before_after <- after_treatment - before_treatment
diff_control_before_after <- after_control - before_control

diff_treatment_before_after - diff_control_before_after
diff_diff <- diff_treatment_before_after - diff_control_before_after

#so, this policy change that happened in 1980 caused increase in log numbers of weeks of unemployment for highers of 0.19
#this is the causal effect
# the policy change caused an increase of 19% in the duration of unemployment for highearners

#PLOTTING THE SAME USING GGPLOT
ggplot(data = diffs, aes(x =after_1980, y = mean_duration, color = highearn)) +
  geom_point() +
  geom_line(aes(group = highearn)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = before_treatment, yend = after_treatment - diff_diff, 
           linetype = "dotted") +
  annotate(geom = "segment", x =1, xend = 1, 
           y = after_treatment - diff_diff, yend = after_treatment, color = "red")

#treatment group did not follow the same trend as control group did
#red is the causal effect

#DIFF IN DIFF WITH REGRESSION
#include a column for group, time, and interaction term
model_simple <- lm(log_duration ~ highearn + after_1980 +
                     (highearn * after_1980),
                   data = injury)
tidy(model_simple)

model_complex <- lm(log_duration ~ highearn + after_1980 +
                     (highearn * after_1980) + male + married + age + hosp,
                     data = injury)
tidy(model_complex)

modelsummary(list(model_simple, model_complex))


#
