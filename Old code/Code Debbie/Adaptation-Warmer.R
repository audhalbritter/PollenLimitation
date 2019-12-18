### ANALYSIS ###

# load the data
source("Merging 2015 and 2017 data.R")

library("broom")
library("writexl")

#***************************************************************************************
#### ADAPTATION ####
## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit simple glm
dfPolli <- Pollination %>% 
  #filter(Species == "LEO", Variable == "Flower") %>% 
  filter(Variable != "Ripe Seed") %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(!is.na(value)) %>%  # remove NAs

  filter(Variable %in% c("Bud", "Flower", "Seed", "Ripe Seed")) %>% 
  group_by(Species, Variable) %>% 
  do(fit = glm(value ~ Treatment * OrigPLevel, data = ., family = "poisson"))

# get the coefficients by group in a tidy data_frame
sink("Warmer_Adaptation1.txt")
Adaptation_warm <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adaptation_warm, path = "Output/Adaptation_warm.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame
sink("Warmer_Adaptation2.txt")
Adaptation_warm1 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
sink()
write_xlsx(Adaptation_warm1, path = "Output/Adaptation_warm2.xlsx", col_names = TRUE)
###**********************************************************************££££
### IMPORTANT ###
# - you need to check if model assumptions are met for each of these models
# - because these are poisson model, you need to check if they are over disperesd.

