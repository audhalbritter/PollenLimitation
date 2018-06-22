### ANALYSIS ###

library("MASS")
library("broom")
library("writexl")
library("nlme")

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


#There are 4 main assumptions to check for a basic linear model like this.
# Model check
ModelCheck <- function(fit){
  # Normality (of residuals)
  p1 <- ggplot(NULL) +
    aes(x = residuals(fit)) +
    geom_bar()
  
  # Constant Error Variance (of residuals)
  p2 <- ggplot(NULL) +
    aes(x = fitted(fit), y = residuals(fit)) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_point()
  
  # Independence (of residuals)
  p3 <- ggplot(NULL) +
    aes(y = residuals(fit), x = 1:length(residuals(fit)) ) +
    geom_point()
  check.plot <- plot_grid(p1, p2, p3, nrow = 2)
  check.plot
}


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
dfPolli <- CumulativeTemperature %>% 
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
  do(fit = lme(cumTemp ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .))
  
### Check model assumptions (add first line to dfPolli)
do(model.check = ModelCheck(lme(cumTemp ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .)))
pdf()
dfPolli$model.check
dev.off()


# get the summary statistics by group in a tidy data_frame
PlasticityCumT_warmer <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Warmer")


### Exceptions only testing GUD plants (control and warmer)
dfPolli <- CumulativeTemperature %>% 
  # we only want plants from Gudmedalen
  filter(Species == "LEO") %>% 
  filter(Origin == "SKJ") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable %in% c("Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))


tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3))


#***************************************************************************************
## WETTER- Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
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
  do(fit = lme(cumTemp ~ Treatment * OrigTLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
# get the summary statistics by group in a tidy data_frame
PlasticityCumT_wetter <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Later SM")




#***************************************************************************************
## WARM AND WET - Flowering date for both species and all pheno stages##
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate")) %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Species, Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
PlasticityCumT_warmwet <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Warm & later SM")


#### ALL PLASTICITY SUMMARIES ####
Plasticity_cumTPhenology <- PlasticityCumT_warmer %>% 
  bind_rows(PlasticityCumT_wetter, PlasticityCumT_warmwet)
write_xlsx(Plasticity_cumTPhenology, path = "Output/Plasticity_cumTPhenologyh.xlsx", col_names = TRUE)


#***************************************************************************************
#***************************************************************************************
#### GROWTH and REPRODUCTIVE OUTPUT ####

## WARM
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% # Only control plants, not pollinated plants
  mutate(OrigPLevel.cen = scale(OrigPLevel, scale = FALSE)) %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  filter(!(Species == "LEO" & Variable == "RepOutput")) %>% # remove RepOutput for LEO, too few observations, see below
  group_by(Species, Variable) %>% 
  do(fit = lme(value ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .))

# get the coefficients by group in a tidy data_frame
Plasticity_warmGrowth <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Warmer")


### Exceptions only testing GUD plants for LEO and Rep Output (control and warmer)
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
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))


LeoWarm <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(estimate, 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Warmer") %>% 
  mutate(Species = "LEO")

Plasticity_warmGrowth <- Plasticity_warmGrowth %>% 
  rbind(LeoWarm)

#***************************************************************************************
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
  do(fit = lme(value ~ Treatment * OrigTLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
Plasticity_wetterGrowth <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Later SM") %>% 
  # remove RAN and Late SM
  filter(!(Species == "RAN" & Variable == "RepOutput" & term == "(Intercept)")) %>% 
  filter(!(Species == "RAN" & Variable == "RepOutput" & term == "TreatmentLaterSM"))


### Exceptions only testing Ranunculus GUD plants  (control and warmer)
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
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))

RanLate <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Later SM") %>% 
  mutate(Species = "RAN")


Plasticity_wetterGrowth <- Plasticity_wetterGrowth %>% 
  rbind(RanLate)



#***************************************************************************************
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
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))



# get the coefficients by group in a tidy data_frame
Plasticity_warmwetGrowth <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparison = "Warm & later SM")


#### ALL PLASTICITY SUMMARIES ####
Plasticity_Growth <- Plasticity_warmGrowth %>% 
  bind_rows(Plasticity_wetterGrowth, Plasticity_warmwetGrowth)
write_xlsx(Plasticity_Growth, path = "Output/Plasticity_Growth.xlsx", col_names = TRUE)



#***************************************************************************************

####################
#### ADAPTATION ####
####################

#### PHENOLOGY ####

## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
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
  do(fit = lme(cumTemp ~ Treatment * DestPLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_cumTwarmer <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Warmer")



# Exception
dfPolli <- CumulativeTemperature %>% 
  # For flowers and seed in Leontodon, only test plants at RAM, not enough at VES
  filter(Species == "LEO") %>% 
  filter(Site == "VES") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% # select treatments
  filter(Variable %in% c("Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3))


#***************************************************************************************

## WETTER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
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
  do(fit = lme(cumTemp ~ Treatment * DestTLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_cumTwetter <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Later SM")


#***************************************************************************************

## WARM AND WETTER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
  # only keep Control plants at VES
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warm and wet
  filter(Treatment %in% c("Control", "WarmLate")) %>% # select treatments
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% # select pheno stages
  filter(!is.na(value)) %>%  # remove NAs
  group_by(Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the coefficients by group in a tidy data_frame
Adapt_cumTwarmwet <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Warm & later SM")


#### ALL ADAPTATION SUMMARIES ####
Adapt_cumTPhenology <- Adapt_cumTwarmer %>% 
  bind_rows(Adapt_cumTwetter, Adapt_cumTwarmwet)
write_xlsx(Adapt_cumTPhenology, path = "Output/Adapt_cumTPhenology.xlsx", col_names = TRUE)


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
  filter(!(Species == "LEO" & Variable == "RepOutput")) %>% # remove RepOutput for LEO, too few observations, see below
  group_by(Species, Variable) %>% 
  do(fit = lme(value ~ Treatment * DestPLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_warmerGrowth <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Warmer")


# Exception: only LEO plants at VES
dfPolli <- Pollination17 %>% 
  filter(Pollination == "control") %>% 
  filter(Species == "LEO") %>% 
  filter(Site == "VES") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Variable %in% c("RepOutput")) %>% 
  filter(!is.na(value)) %>% # remove NAs
  group_by(Variable) %>% 
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))

LeoWarmAdapt <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Warmer") %>% 
  mutate(Species = "LEO")

Adapt_warmerGrowth <- Adapt_warmerGrowth %>% 
  rbind(LeoWarmAdapt)


#***************************************************************************************
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
  do(fit = lme(value ~ Treatment * DestTLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_wetterGrowth <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Later SM") %>% 
  # remove RAN and Late SM
  filter(!(Species == "RAN" & Variable == "RepOutput" & term == "(Intercept)")) %>% 
  filter(!(Species == "RAN" & Variable == "RepOutput" & term == "TreatmentLaterSM"))


# Exception: only RAN plants at SKJ
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
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))

RanLateAdapt <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Later SM") %>% 
  mutate(Species = "RAN")

Adapt_wetterGrowth <- Adapt_wetterGrowth %>% 
  rbind(RanLateAdapt)


#***************************************************************************************
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
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))


# get the coefficients by group in a tidy data_frame
Adapt_warmwetGrowth <- tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  mutate(Comparision = "Warmer & later SM")


#### SUMMARY ADDAPTATION PHENOLOGY ####
Adapt_Growth <- Adapt_warmerGrowth %>% 
  bind_rows(Adapt_wetterGrowth, Adapt_warmwetGrowth)
write_xlsx(Adapt_Growth, path = "Output/Adapt_Growth.xlsx", col_names = TRUE)
