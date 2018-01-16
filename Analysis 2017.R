### ANALYSIS ###

# load the data
source("Merging 2015 and 2017 data.R")

library("broom")

#***************************************************************************************
#### PLASTICITY ####
## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit simple glm
dfPolli <- Pollination %>% 
  # remove Control plants at Veskre, because they are not needed for the plasticity question
  filter(Origin != c("VES") | Treatment != "Control") %>%  
  filter(Origin != c("RAM") | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(!is.na(value)) %>%  # remove NAs
  filter(Variable %in% c("Bud", "Flower", "Seed", "Ripe Seed")) %>% 
  group_by(Species, Variable) %>% 
  do(fit = glm(value ~ Treatment * OrigPLevel, data = ., family = "poisson"))
  
# get the coefficients by group in a tidy data_frame
tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3)) %>% pn

# get the summary statistics by group in a tidy data_frame
glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)


### IMPORTANT ###
# - you need to check if model assumptions are met for each of these models
# - because these are poisson model, you need to check if they are over disperesd.

  
## WETTER - Flowering date for Leontodon ##
dat <- Pollination %>% 
  filter(Species == "LEO", Variable == "Flower") %>% 
  # remove Control plants at Veskre and Skj
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(!is.na(value)) # remove NAs

# fit dimple glm
fit <- glm(value ~ Treatment * OrigTLevel, data = dat, family = "poisson")
summary(fit)
  
## WARM AND WET - Flowering date for Leontodon ##
dat <- Pollination %>% 
  filter(Species == "LEO", Variable == "Flower") %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate")) %>% 
  filter(!is.na(value)) # remove NAs

# fit dimple glm
fit <- glm(value ~ Treatment, data = dat, family = "poisson")
summary(fit)


#***************************************************************************************
#### ADAPTATION ####
## WARMER - Flowering date for Leontodon ##
dat <- Pollination %>% 
  filter(Species == "LEO", Variable == "Flower") %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(!is.na(value)) # remove NAs

# fit dimple glm
fit <- glm(value ~ Treatment * OrigPLevel, data = dat, family = "poisson")
summary(fit)



## WETTER - Flowering date for Leontodon ##
dat <- Pollination %>% 
  filter(Species == "LEO", Variable == "Flower") %>% 
  # remove Control plants at Gudmedalen and Ram
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(!is.na(value)) # remove NAs

# fit dimple glm
fit <- glm(value ~ Treatment * OrigTLevel, data = dat, family = "poisson")
summary(fit)


## WARM AND WET - Flowering date for Leontodon ##
dat <- Pollination %>% 
  filter(Species == "LEO", Variable == "Flower") %>% 
  # remove Control plants at Gudmedalen, Skj and Ram
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate")) %>%
  filter(!is.na(value)) # remove NAs

# fit dimple glm
fit <- glm(value ~ Treatment, data = dat, family = "poisson")
summary(fit)




#***************************************************************************************

#### MORE COMPLICATED MODELS; NOT NEEDED FOR NOW
library("lme4")
library("MuMIn")

dat <- Pollination %>% 
  left_join(climate, by = c("Year" = "year", "Site" = "site", "value" = "doy")) %>% 
  filter(Year == 2017) %>% 
  mutate(OrigPLevelRescale = (OrigPLevel - min(OrigPLevel))/(max(OrigPLevel) - min(OrigPLevel))) %>% 
  mutate(OrigTLevelRescale = (OrigTLevel - min(OrigTLevel))/(max(OrigTLevel) - min(OrigTLevel)))


dat2 <- dat %>% 
  filter(Species == "LEO", pheno.stage == "Flower") %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  # remove Control at Veskre
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(!is.na(value))
  

fit <- glm(value ~ OrigPLevelRescale * Treatment, data = dat2, family = "poisson")
summary(fit)


new.dat <- data.frame(expand.grid(OrigPLevel = c(2000, 2700),
                      Treatment = c("Control", "Warmer")))
new.dat$pred <- exp(predict(fit, new.dat))
new.dat

dat2 <- dat %>% 
  filter(Species == "LEO", pheno.stage == "Flower")

fit <- glmer(value ~ Treatment + OrigPLevelRescale + OrigTLevelRescale + Treatment:OrigPLevelRescale + Treatment:OrigTLevelRescale + (1|NewBlock), data = dat2, family = "poisson")
summary(fit)





