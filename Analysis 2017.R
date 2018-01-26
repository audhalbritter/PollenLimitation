### ANALYSIS ###

# load the data
source("Merging 2015 and 2017 data.R")

library("broom")

#***************************************************************************************
#### PLASTICITY ####
## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination %>% 
  # centre Plevel
  mutate(OrigPLevel.cen = scale(OrigPLevel, scale = FALSE)) %>% 
  # we only want Control plants at Gudmedalen and SKJ
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = glm.nb(value ~ Treatment * OrigPLevel.cen, data = .))

# get the coefficients by group in a tidy data_frame

Plasticity_warmer1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_warmer1, path = "Output/Plasticity_warmer.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame

Plasticity_warmer2 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
#write_xlsx(Plasticity_warmer2, path = "Output/Plasticity_warmer.xlsx", col_names = TRUE)


### IMPORTANT ###
# - you need to check if model assumptions are met for each of these models
# - because these are poisson model, you need to check if they are over disperesd.


#***************************************************************************************
## WETTER- Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination %>% 
  # centre Plevel
  mutate(OrigTLevel.cen = scale(OrigTLevel, scale = FALSE)) %>% 
  # we only want Control plants at GUD and RAM
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = glm.nb(value ~ Treatment * OrigTLevel.cen, data = .))

# get the coefficients by group in a tidy data_frame
Plasticity_wetter1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(exp(estimate), 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_wetter1, path = "Output/Plasticity_wetter.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame
Plasticity_wetter2 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
#write_xlsx(Plasticity_wetter2, path = "Output/Plasticity_wetter.xlsx", col_names = TRUE)
  


#***************************************************************************************
## WARM AND WET - Flowering date for both species and all pheno stages##
# fit nb. glm
dfPolli <- Pollination %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate")) %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = glm.nb(value ~ Treatment, data = .))

# get the coefficients by group in a tidy data_frame
Plasticity_warmwet1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(exp(estimate), 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_warmwet1, path = "Output/Plasticity_warmwet.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame
Plasticity_warmwet2 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
#write_xlsx(Plasticity_warmwet2, path = "Output/Plasticity_warmwet.xlsx", col_names = TRUE)


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





