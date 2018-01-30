### ANALYSIS ###

# load the data
source("Merging 2015 and 2017 data.R")
library("MASS")
library("broom")
library("writexl")

# FUNCTION
#### CHECK MODEL ASSUMPTIONS ####
#(Generalized) Linear models make some strong assumptions concerning the data structure:

# Normality of Residuals (QQ plot, density plot of residuals)
# Correct specification of the variance structure; Evaluate homoscedasticity (plot residuals vs. fitted values)

fix.check <- function(mod){    #function to produce model-checking plots for the fixed effects of an lmer model
  par(mfrow = c(2,2))
  plot(fitted(mod),resid(mod))  #should have no pattern
  abline(h=0)
  print(anova(lm(fitted(mod)~resid(mod))))	#should be non-significant
  qqnorm(resid(mod), ylab="Residuals")		#should be approximately straight line
  qqline(resid(mod))
  plot(density(resid(mod)))					#should be roughly normally distributed
  rug(resid(mod))}

Pollination17 <- Pollination %>% 
  filter(Year == 2017) %>% 
  # remove Second Flowers
  filter(!Pollination == "")

#***************************************************************************************

####################
#### PLASTICITY ####
####################

#### PHENOLOGY ####

## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination17 %>% 
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


### Exceptions only testing GUD plants (control and warmer)
dfPolli <- Pollination17 %>% 
  # we only want plants from Gudmedalen
  filter(Species == "LEO") %>% 
  filter(Origin == "SKJ") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable %in% c("Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = glm.nb(value ~ Treatment, data = .))

tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))


#***************************************************************************************
## WETTER- Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination17 %>% 
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
dfPolli <- Pollination17 %>% 
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
#***************************************************************************************
#### GROWTH and REP OUTPUT ####

## WARM
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  mutate(OrigPLevel.cen = scale(OrigPLevel, scale = FALSE)) %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lm(value ~ Treatment * OrigPLevel.cen, data = .))
           
# get the coefficients by group in a tidy data_frame
Plasticity_warmGrowth1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_warmGrowth1, path = "Output/Plasticity_warmGrowth.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame
Plasticity_warmGrowth2 <- glance(dfPolli, fit)
#write_xlsx(Plasticity_warmGrowth2, path = "Output/Plasticity_warmGrowth.xlsx", col_names = TRUE)


### Exceptions only testing GUD plants (control and warmer)
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  # we only want plants from Gudmedalen
  filter(Species == "LEO") %>% 
  filter(Origin == "SKJ") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable == "RepOutput") %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lm(value ~ Treatment, data = .))

tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(estimate, 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))


## WETTER
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  mutate(OrigTLevel.cen = scale(OrigTLevel, scale = FALSE)) %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lm(value ~ Treatment * OrigTLevel.cen, data = .))

# get the coefficients by group in a tidy data_frame
Plasticity_wetGrowth1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_wetGrowth1, path = "Output/Plasticity_wetGrowth.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame
Plasticity_warmGrowth2 <- glance(dfPolli, fit)
#write_xlsx(Plasticity_warmGrowth2, path = "Output/Plasticity_warmGrowth.xlsx", col_names = TRUE)



### Exceptions only testing GUD plants (control and warmer)
dfPolli <- Pollination %>% 
  filter(Pollination == "control") %>% 
  # we only want plants from Gudmedalen
  filter(Species == "RAN") %>% 
  filter(Origin == "GUD") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% # select treatments
  filter(Variable == "RepOutput") %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lm(value ~ Treatment, data = .))

tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(estimate, 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))



## WARMER AND WETTER
dfPolli <- Pollination17 %>%
  filter(Pollination == "control") %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "WarmLate")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lm(value ~ Treatment, data = .))

# get the coefficients by group in a tidy data_frame
Plasticity_warmwetGrowth1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Plasticity_warmwetGrowth1, path = "Output/Plasticity_warmwetGrowth.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame
Plasticity_warmGrowth2 <- glance(dfPolli, fit)
#write_xlsx(Plasticity_warmGrowth2, path = "Output/Plasticity_warmGrowth.xlsx", col_names = TRUE)



#***************************************************************************************
#***************************************************************************************
#***************************************************************************************

####################
#### ADAPTATION ####
####################

#### PHENOLOGY ####

## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination17 %>% 
  # centre Plevel
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = glm.nb(value ~ Treatment * DestPLevel.cen, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_warmer1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adapt_warmer1, path = "Output/Adapt_warmer1.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame

Adapt_warmer2 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
#write_xlsx(Adapt_warmer2, path = "Output/Adapt_warmer2.xlsx", col_names = TRUE)



# Exception
dfPolli <- Pollination17 %>% 
  # For flowers and seed in Leontodon, only test plants at RAM, not enough at VES
  filter(Species == "LEO") %>% 
  filter(Site == "VES") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable %in% c("Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = glm.nb(value ~ Treatment, data = .))

# get the coefficients by group in a tidy data_frame

tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))

#***************************************************************************************

## WETTER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination17 %>% 
  # centre Tlevel
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  # remove Control plants at Gudmedalen and Rambera, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "LaterSM")) %>% # select treatments
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = glm.nb(value ~ Treatment * DestTLevel.cen, data = .))


# get the coefficients by group in a tidy data_frame

Adapt_wetter1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adapt_wetter1, path = "Output/Adapt_wetter1.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame

Adapt_wetter2 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
#write_xlsx(Adapt_wetter2, path = "Output/Adapt_wetter2.xlsx", col_names = TRUE)



#***************************************************************************************

## WARM AND WETTER - Bud, Flower, Seed and Ripe Seed date for both species ##
# fit nb. glm
dfPolli <- Pollination17 %>% 
  # only keep Control plants at VES
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warm and wet
  filter(Treatment %in% c("Control", "WarmLate")) %>% # select treatments
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = glm.nb(value ~ Treatment, data = .))


# get the coefficients by group in a tidy data_frame

Adapt_warmwet1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adapt_warmwet1, path = "Output/Adapt_warmwet1.xlsx", col_names = TRUE)

# get the summary statistics by group in a tidy data_frame

Adapt_warmwet2 <- glance(dfPolli, fit) %>% 
  # check for overdispersion: ratio of residual deviance to degrees of freedom, overdispersion if ratio > 1
  mutate(ratio = deviance / df.residual)
#write_xlsx(Adapt_warmwet2, path = "Output/Adapt_warmwet2.xlsx", col_names = TRUE)



#***************************************************************************************
#***************************************************************************************
#### GROWTH and REP OUTPUT ####

## WARM
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lm(value ~ Treatment * DestPLevel.cen, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_warmGrowth1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adapt_warmGrowth1, path = "Output/Adapt_warmGrowth1.xlsx", col_names = TRUE)


# Exception: only plants at VES
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  filter(Species == "LEO") %>% 
  filter(Site == "VES") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Variable %in% c("RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Variable) %>% 
  do(fit = lm(value ~ Treatment, data = .))

tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))


## WETTER
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lm(value ~ Treatment * DestTLevel.cen, data = .))

# get the coefficients by group in a tidy data_frame
Adapt_wetterGrowth1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adapt_wetterGrowth1, path = "Output/Adapt_wetterGrowth1.xlsx", col_names = TRUE)


# Exception: only plants at SKJ
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  filter(Species == "RAN") %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Site == "SKJ") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(Variable %in% c("RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Variable) %>% 
  do(fit = lm(value ~ Treatment, data = .))

tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))



## WARM AND WETTER
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  # only keep Control plants at VES
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warm and wet
  filter(Treatment %in% c("Control", "WarmLate")) %>% # select treatments
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lm(value ~ Treatment, data = .))

# get the coefficients by group in a tidy data_frame
Adapt_warmwetGrowth1 <- tidy(dfPolli, fit) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))
write_xlsx(Adapt_wetterGrowth1, path = "Output/Adapt_wetterGrowth1.xlsx", col_names = TRUE)

