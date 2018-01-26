### ANALYSIS ###

# load the data
source("Merging 2015 and 2017 data.R")

library("broom")
library("writexl")

#***************************************************************************************
#### Plasticity ####
## WETTER- Bud, Flower, Seed and Ripe Seed date for both species ##
# fit simple glm
dfPolli <- Pollination %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate")) %>% 
  filter(!is.na(value)) %>%  # remove NAs
  
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  group_by(Species, Variable) %>% 
  do(fit = glm(value ~ Treatment * OrigPLevel, data = ., family = "poisson"))

# get the coefficients by group in a tidy data_frame

Plasticity_wetter1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_wetter1, path = "Output/Plasticity_warmer_and_wetter.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame

Plasticity_wetter1 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)

write_xlsx(Plasticity_wetter1, path = "Output/Plasticity_wetter2.xlsx", col_names = TRUE)
###**********************************************************************££££
### IMPORTANT ###
# - you need to check if model assumptions are met for each of these models
# - because these are poisson model, you need to check if they are over disperesd.