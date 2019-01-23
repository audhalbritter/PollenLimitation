### ANALYSIS ###

library("MASS")
library("writexl")
library("nlme")

source("Merging 2015 and 2017 data.R")
source("MyFunctions.R")
source("ClimateData.R")


#***************************************************************************************
# Prepare cum temp
CumulativeTemp <- DailyAndCumulativeTemp %>% 
  filter(variable == "cumulativeTemperature") %>% 
  rename("cumTemp" = "value")

# Biomass and RepOutput
Production <- Pollination %>% 
  # remove Second Flowers
  filter(!Pollination == "") %>% 
  filter(Variable %in% c("EndSize", "RepOutput"))

### Join cumulative temperature with phenology data
CumulativeTemperature <- Pollination %>% 
  # remove Second Flowers
  filter(!Pollination == "") %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>%
  left_join(CumulativeTemp, by = c("Site" = "Site", "value" = "dssm", "Year", "SM")) %>% 
  bind_rows(Production)

CumulativeTemperature <- CumulativeTemperature %>% 
  # centre Plevel
  mutate(OrigPLevel.cen = scale(OrigPLevel, scale = FALSE),
         OrigTLevel.cen = scale(OrigTLevel, scale = FALSE),
         DestPLevel.cen = scale(DestPLevel, scale = FALSE),
         DestTLevel.cen = scale(DestTLevel, scale = FALSE))

####################
#### PLASTICITY ####
####################

#### PHENOLOGY ####

## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>%
  # Only want Control plants at Gudmedalen and SKJ
  filter(Origin != "VES" | Treatment != "Control",
         Origin != "RAM" | Treatment != "Control") %>% 
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"), # select treatments
         Variable %in% c("Bud", "Flower", "Seed"),
         !is.na(cumTemp)) %>% # remove NAs
  filter(!(Year == 2015 & Species == "LEO")) %>% 
  # use group by to do analysis for each year, species and pheno stage
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .))
  
### Check model assumptions (add first line to dfPolli)
# do(model.check = ModelCheck(lme(cumTemp ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .)))
# pdf()
# dfPolli$model.check
# dev.off()

# get the summary statistics by group in a tidy data_frame
PlasticCTWarm <- TidyResults(dfPolli, "Warmer")


### Only Leo 2015
dfPolli <- CumulativeTemperature %>%
  # we only want Control plants at Gudmedalen and SKJ
  filter(Origin != "VES" | Treatment != "Control",
         Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"), # select treatments
         !is.na(cumTemp)) %>% # remove NAs
  filter(Year == 2015 & Species == "LEO", Variable == "Bud") %>% 
  # use group by to do analysis for each species and pheno stage
  group_by() %>% 
  do(fit = lme(cumTemp ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticCTWarmLeo15 <- TidyResults(dfPolli, "Warmer") %>% 
  mutate(Year = 2015,
         Species = "LEO",
         Variable = "Bud")
  

### Exceptions only testing SKJ plants (control and warmer)
dfPolli <- CumulativeTemperature %>% 
  # we only want plants from SKJ
  filter(Species == "LEO",
         Origin == "SKJ",
         Year == 2017,
         Treatment %in% c("Control", "Warmer"),
         Variable %in% c("Flower", "Seed"),
         !is.na(cumTemp)) %>%
  group_by(Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

PlasticCTWarmLeo17 <- TidyResults(dfPolli, "Warmer") %>%
  mutate(Year = 2017,
         Species = "LEO")

ResultsPlasticWarmer <- PlasticCTWarm %>% 
  filter(!(Year == 2017 & Species == "LEO" & Variable %in% c("Flower", "Seed"))) %>% 
  bind_rows(PlasticCTWarmLeo15, PlasticCTWarmLeo17)


#***************************************************************************************
## WETTER- Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at GUD and RAM
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM"),
         Variable %in% c("Bud", "Flower", "Seed"),
         !is.na(cumTemp),
         Year == 2017) %>%
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment * OrigTLevel.cen, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticityCTWetter <- TidyResults(dfPolli, "LaterSM")


### Leo 2015, only origin RAM has enough
dfPolli <- CumulativeTemperature %>%
  # select only controls and warmer
  filter(Year == 2015,
         Species == "LEO",
         Origin == "RAM",
         Treatment %in% c("Control", "LaterSM"),
         Variable == "Bud",
         !is.na(cumTemp)) %>% 
  group_by() %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticCTWetterLeo15 <- TidyResults(dfPolli, "LaterSM") %>% 
  mutate(Year = 2015,
         Species = "LEO",
         Variable = "Bud")


### Ran 2015, only origin GUD has enough
dfPolli <- CumulativeTemperature %>%
  # select only controls and warmer
  filter(Year == 2015,
         Species == "RAN",
         Origin == "GUD",
         Treatment %in% c("Control", "LaterSM"),
         Variable %in% c("Bud", "Flower"),
         !is.na(cumTemp)) %>% 
  group_by(Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticCTWetterRan15 <- TidyResults(dfPolli, "LaterSM") %>% 
  mutate(Year = 2015,
         Species = "RAN")

ResultsPlasticWetter <- PlasticityCTWetter %>% 
  bind_rows(PlasticCTWetterLeo15, PlasticCTWetterRan15)


#***************************************************************************************
## WARM AND WET - Flowering date for both species and all pheno stages##
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate"),
         Variable %in% c("Bud", "Flower", "Seed"),
         !is.na(cumTemp)) %>% 
  filter(!(Year == 2015 & Species == "LEO")) %>% 
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticityCTWarmWet <- TidyResults(dfPolli, "WarmLateSM")


### Only Leo 2015
dfPolli <- CumulativeTemperature %>%
  # we only want Control plants at Gudmedalen and SKJ
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate"),
         !is.na(cumTemp)) %>% # remove NAs
  filter(Year == 2015,
         Species == "LEO",
         Variable == "Bud") %>% 
  group_by() %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticCTWarmWetLeo15 <- TidyResults(dfPolli, "WarmLateSM") %>% 
  mutate(Year = 2015,
         Species = "LEO",
         Variable = "Bud")


ResultsPlasticWarmWet <- PlasticityCTWarmWet %>% 
  bind_rows(PlasticCTWarmWetLeo15)


#### ALL PLASTICITY SUMMARIES ####
Plasticity_cumTPhenology <- ResultsPlasticWarmer %>% 
  bind_rows(ResultsPlasticWetter, ResultsPlasticWarmWet) %>% 
  mutate(Comparison = factor(Comparison, levels = c("Warmer", "LaterSM", "WarmLateSM"))) %>% 
  arrange(Species, Year, Comparison)
write_xlsx(Plasticity_cumTPhenology, path = "Plasticity_cumTPhenology.xlsx")

#***************************************************************************************
#***************************************************************************************
#### GROWTH and REPRODUCTIVE OUTPUT ####

## WARM
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"),
         Variable %in% c("EndSize", "RepOutput"),
         !is.na(value)) %>% # remove NAs
  filter(!(Variable == "RepOutput" & Species == "LEO")) %>%
  filter(!(Variable == "RepOutput" & Species == "RAN" & Year == 2015)) %>%
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(value ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .))


### Check model assumptions (add first line to dfPolli)
# do(model.check = ModelCheck(lme(cumTemp ~ Treatment * OrigPLevel.cen, random = ~ 1 | NewBlock, data = .)))
# pdf()
# dfPolli$model.check
# dev.off()

# get the summary statistics by group in a tidy data_frame
PlasticWarmProd1 <- TidyResults(dfPolli, "Warmer")

# RepOutput only Ran 2015
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"),
         Variable %in% c("RepOutput"),
         Species == "RAN",
         Year == 2015,
         !is.na(value)) %>% # remove NAs
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))
PlasticWarmProd2 <- TidyResults(dfPolli, "Warmer")


# RepOutput only LEO 2017
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"),
         Variable %in% c("RepOutput"),
         Species == "LEO",
         Year == 2017,
         !is.na(value)) %>% # remove NAs
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))
PlasticWarmProd3 <- TidyResults(dfPolli, "Warmer")


Plasticity_warmGrowth <- PlasticWarmProd1 %>% 
  rbind(PlasticWarmProd2, PlasticWarmProd3)

#***************************************************************************************
## WETTER
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "LaterSM"),
         Variable %in% c("EndSize", "RepOutput"),
         !is.na(value)) %>% # remove NAs
  filter(!(Variable == "RepOutput" & Year == 2015 & Species == "RAN")) %>%
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(value ~ Treatment * OrigTLevel.cen, random = ~ 1 | NewBlock, data = .))


# get the summary statistics by group in a tidy data_frame
PlasticWetProd1 <- TidyResults(dfPolli, "LaterSM")


# RepOutput only Ran 2017
dfPolli <- CumulativeTemperature %>% 
  # we only want Control plants at Gudmedalen
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>% 
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM"),
         Variable %in% c("RepOutput"),
         Species == "RAN",
         Year == 2017,
         !is.na(value)) %>% # remove NAs
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))
PlasticWetProd2 <- TidyResults(dfPolli, "LaterSM")


Plasticity_wetGrowth <- PlasticWetProd1 %>% 
  filter(!(Year == 2017 & Species == "RAN" & Variable == "RepOutput")) %>%
  rbind(PlasticWetProd2)



#***************************************************************************************
## WARMER AND WETTER
dfPolli <- CumulativeTemperature %>%
  # we only want Control plants at Gudmedalen
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "WarmLate"),
         Variable %in% c("EndSize", "RepOutput"),
         !is.na(value)) %>% # remove NAs
  group_by(Year, Species, Variable) %>% 
  do(fit = lme(value ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
PlasticWarmWetProd <- TidyResults(dfPolli, "WarmLateSM")


#### ALL PLASTICITY SUMMARIES ####
Plasticity_Growth <- Plasticity_warmGrowth %>% 
  bind_rows(Plasticity_wetGrowth, PlasticWarmWetProd) %>% 
  mutate(Comparison = factor(Comparison, levels = c("Warmer", "LaterSM", "WarmLateSM"))) %>% 
  arrange(Species, Year, Comparison)
write_xlsx(Plasticity_Growth, path = "Plasticity_Growth.xlsx")



#***************************************************************************************

####################
#### ADAPTATION ####
####################

#### PHENOLOGY ####

## WARMER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"),
         !is.na(cumTemp)) %>%  # remove NAs
  filter(!(Year == 2015 & Species == "LEO")) %>% 
  group_by(Year, Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lme(cumTemp ~ Treatment * DestPLevel.cen, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
AdaptCTWarm <- TidyResults(dfPolli, "Warmer")


### Only Leo 2015
dfPolli <- CumulativeTemperature %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer"),
         !is.na(cumTemp),
         Year == 2015,
         Species == "LEO",
         Variable == "Bud") %>%  
  group_by() %>% 
  do(fit = lme(cumTemp ~ Treatment * DestPLevel.cen, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
AdaptCTWarmLeo15 <- TidyResults(dfPolli, "Warmer") %>% 
  mutate(Year = 2015,
         Species = "LEO",
         Variable = "Bud")


# Exception LEO 2017
dfPolli <- CumulativeTemperature %>% 
  # For flowers and seed in Leontodon, only test plants at VES, not enough at RAM
  filter(Species == "LEO",
         Site == "VES",
         Year == 2017,
         # select only controls and warmer
         Treatment %in% c("Control", "Warmer"),
         # select pheno stages
         Variable %in% c("Flower", "Seed")) %>% 
  filter(!is.na(cumTemp)) %>% # remove NAs
  group_by(Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

AdaptCTWarmLeo17 <- TidyResults(dfPolli, "Warmer") %>% 
  mutate(Year = 2017,
         Species = "LEO")


ResultsAdaptWarmer <- AdaptCTWarm %>% 
  filter(!(Year == 2017 & Species == "LEO" & Variable %in% c("Flower", "Seed"))) %>% 
  bind_rows(AdaptCTWarmLeo15, AdaptCTWarmLeo17)




#***************************************************************************************

## WETTER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
  # remove Control plants at Gudmedalen and Rambera, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "LaterSM"),
         !is.na(cumTemp),
         Year == 2017) %>% 
  group_by(Year, Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lme(cumTemp ~ Treatment * DestTLevel.cen, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
AdaptCTWetter <- TidyResults(dfPolli, "LaterSM")


### Only Leo 2015 at VES
dfPolli <- CumulativeTemperature %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Site == "VES") %>%  
  # select only controls and wetter
  filter(Treatment %in% c("Control", "LaterSM"),
         !is.na(cumTemp),
         Year == 2015,
         Species == "LEO", 
         Variable == "Bud") %>% 
  group_by() %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))


# get the summary statistics by group in a tidy data_frame
AdaptCTWetterLeo15 <- TidyResults(dfPolli, "LaterSM") %>% 
  mutate(Year = 2015,
         Species = "LEO",
         Variable = "Bud")


### Ran 2015, only site SKJ has enough
dfPolli <- CumulativeTemperature %>%
  # select only controls and warmer
  filter(Year == 2015,
         Species == "RAN",
         Site == "SKJ",
         Treatment %in% c("Control", "LaterSM"),
         Variable %in% c("Bud", "Flower"),
         !is.na(cumTemp)) %>% 
  group_by(Variable) %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))


# get the summary statistics by group in a tidy data_frame
AdaptCTWetterRan15 <- TidyResults(dfPolli, "LaterSM") %>% 
  mutate(Year = 2015,
         Species = "RAN")

ResultsAdaptWetter <- AdaptCTWetter %>% 
  bind_rows(AdaptCTWetterLeo15, AdaptCTWetterRan15)



#***************************************************************************************

## WARM AND WETTER - Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- CumulativeTemperature %>% 
  # only keep Control plants at VES
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warm and wet
  filter(Treatment %in% c("Control", "WarmLate"),
         !is.na(cumTemp)) %>%  # remove NAs
  filter(!(Year == 2015 & Species == "LEO")) %>% 
  group_by(Year, Species, Variable) %>% # use group by to do analysis for each species and pheno stage
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))

# get the summary statistics by group in a tidy data_frame
AdaptCTWarmWet <- TidyResults(dfPolli, "WarmLateSM")


### Only Leo 2015
dfPolli <- CumulativeTemperature %>% 
  # remove Control plants at Gudmedalen and Skj, because they are not needed for the adaptation question
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate"),
         !is.na(cumTemp),
         Year == 2015, 
         Species == "LEO", 
         Variable == "Bud") %>% 
  group_by() %>% 
  do(fit = lme(cumTemp ~ Treatment, random = ~ 1 | NewBlock, data = .))


# get the summary statistics by group in a tidy data_frame
AdaptCTWarmWetLeo15 <- TidyResults(dfPolli, "WarmLateSM") %>% 
  mutate(Year = 2015,
         Species = "LEO",
         Variable = "Bud")

ResultsAdaptWarmWet <- AdaptCTWarmWet %>% 
  bind_rows(AdaptCTWarmWetLeo15)


#### ALL ADAPTATION SUMMARIES ####
Adapt_cumTPhenology <- ResultsAdaptWarmer %>% 
  bind_rows(ResultsAdaptWetter, ResultsAdaptWarmWet) %>% 
  mutate(Comparison = factor(Comparison, levels = c("Warmer", "LaterSM", "WarmLateSM"))) %>% 
  arrange(Species, Year, Comparison)

write_xlsx(Adapt_cumTPhenology, path = "Phenology_cumTAdapt.xlsx")




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
