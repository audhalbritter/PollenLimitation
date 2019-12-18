#### Script to download and process data ####
#remotes::install_github("centerforopenscience/osfr")
#devtools::install_github("Between-the-Fjords/downloader")
library("osfr")
library("downloader")
library("tidyverse")
library("ggforce")
library("lubridate")
library("nlme")

# Download data from OSF
get_file(node = "nbys2", 
         file = "cleaned_PhenologyPollination_2015_2017.csv", 
         path = "data_cleanded")

get_file(node = "nbys2", 
         file = "DailyAndCumulativeTemp.csv", 
         path = "data_cleanded")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

SP <- c(LEO = "Leontodon autumnalis", RAN = "Ranunculus acris")

dat <- read_csv(file = "data_cleanded/DailyAndCumulativeTemp.csv", col_names = TRUE)
# Climate data
DailyTemp <- DailyAndCumulativeTemp %>% 
  filter(variable == "dailyTemperature") %>% 
  rename("dailyTemp" = "value") %>% 
  # remove observations before SM
  filter(doy >= SM) %>% 
  select(Site, Year, doy, dailyTemp)


# Snowmelt date
Snowmelt <- data_frame(Site = rep(c("GUD", "RAM", "SKJ", "VES"), 2),
                       Year = as.numeric(c(rep("2015", 4), rep("2017", 4))),
                       SM = c(184, 167, 224, 174, 143, 137, 176, 135)) 

# read in data
dat <- read_csv(file = "data_cleanded/cleaned_PhenologyPollination_2015_2017.csv", col_names = TRUE)

dat <- dat %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  #filter(Variable == "Flower") %>% 
  mutate(DevelopmentRate = 1/value,
         DOY = SM + value) %>% 
  # Min = SM, Max = value
  full_join(DailyTemp, by = c("Site", "Year")) %>% 
  # remove temperature after flowering
  filter(doy < DOY) %>% 
  # Calculate mean temp per individual plant
  group_by(Site, Origin, Year, Species, Treatment, DestTLevel, DestPLevel, OrigTLevel, OrigPLevel, SM, NewBlock, ID, Variable, value, DevelopmentRate) %>% 
  summarise(meanTemp = mean(dailyTemp)) %>% 
  # Remove data, with less than 5 observations/individuals
  group_by(Year, Species, Origin, Site, Treatment, Variable) %>% 
  mutate(n = n()) %>% 
  filter(n > 5) %>% 
  # centre Plevel
  mutate(OrigPLevel.cen = scale(OrigPLevel, scale = FALSE),
         OrigTLevel.cen = scale(OrigTLevel, scale = FALSE),
         DestPLevel.cen = scale(DestPLevel, scale = FALSE),
         DestTLevel.cen = scale(DestTLevel, scale = FALSE))


ggplot(dat, aes(x = meanTemp, y = DevelopmentRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ Species)

dfPolli <- dat %>% 
  filter(!(Species == "LEO" & Year == 2015)) %>% 
  group_by(Species, Variable) %>% 
  do(fit = lm(DevelopmentRate ~ meanTemp, data = .))

tidy(dfPolli, fit, effects = "fixed") %>% 
  mutate(term = recode(term, "(Intercept)" = "intercept", "meanTemp" = "slope")) %>% 
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>% 
  mutate(thermalThreshold = -estimate_intercept / estimate_slope,
         heatAccLevel = 1 / estimate_slope) %>% 
  ggplot(aes(x = Variable, y = thermalThreshold)) +
  geom_point() +
  facet_grid(~ Species)


dfPolli <- dat %>% 
  filter(!(Species == "LEO" & Year == 2015)) %>% 
  group_by(Year, Species, Treatment, Origin, Site, Variable) %>% 
  do(fit = lm(DevelopmentRate ~ meanTemp, data = .))
  #do(fit = lme(DevelopmentRate ~ meanTemp, random = ~ 1 | NewBlock, data = .))

res <- tidy(dfPolli, fit, effects = "fixed") 

output <- res %>% 
  mutate(term = recode(term, "(Intercept)" = "intercept", "meanTemp" = "slope")) %>% 
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  mutate(thermalThreshold = -estimate_intercept / estimate_slope,
         heatAccLevel = 1 / estimate_slope) %>% 
  left_join(Snowmelt, by = c("Site", "Year")) %>% 
  ungroup() %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>% 
  mutate(Variable = factor(Variable, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(Habitat = case_when(Origin == "GUD" ~ "Alpine - early",
                             Origin == "RAM" ~ "Subalpine - early",
                             Origin == "SKJ" ~ "Alpine - late",
                             Origin == "VES" ~ "Subalpine - late"))

ggplot(output, aes(x = Variable, y = thermalThreshold, colour = Treatment, shape = Habitat)) +
  geom_jitter(size = 3, alpha = 0.8, width = 0.2) +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Origin", values = c(17, 18, 15, 16)) +
  labs(x = "Snowmelt date", y = "Thermal threshold") +
  facet_grid(Year ~ Species, labeller=labeller(Species = SP))
  

ggplot(output, aes(x = Variable, y = estimate_slope, colour = Treatment, shape = Habitat)) +
  geom_jitter(size = 3, alpha = 0.7) +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Origin", values = c(17, 18, 15, 16)) +
  labs(x = "Snowmelt date", y = "Heat use effeciency") +
  facet_grid(Year ~ Species)

dat %>% 
  filter(!(Species == "LEO" & Year == 2015)) %>% 
  ungroup() %>% 
  mutate(Habitat = case_when(Origin == "GUD" ~ "Alpine - early",
                             Origin == "RAM" ~ "Subalpine - early",
                             Origin == "SKJ" ~ "Alpine - late",
                             Origin == "VES" ~ "Subalpine - late")) %>% 
  ggplot(aes(x = meanTemp, y = DevelopmentRate, colour = Treatment, linetype = as.factor(Year), shape = Habitat)) +
  geom_point(aes()) +
  geom_smooth(method = "lm") +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Origin", values = c(17, 18, 15, 16)) +
  facet_grid(Variable ~ Species)


dat %>% 
  filter(!(Species == "LEO" & Year == 2015), Variable == "Flower") %>% 
  ungroup() %>% 
  mutate(Habitat = case_when(Origin == "GUD" ~ "Alpine - early",
                             Origin == "RAM" ~ "Subalpine - early",
                             Origin == "SKJ" ~ "Alpine - late",
                             Origin == "VES" ~ "Subalpine - late")) %>% 
  ggplot(aes(x = Habitat, y = DevelopmentRate, fill = Treatment)) +
  geom_sina() +
  scale_fill_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  labs(x = "", y = "Development rate") +
  facet_grid(Year ~ Species) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



dat %>% 
  filter(Species == "RAN" & Variable == "Flower") %>% 
  ungroup() %>% 
  mutate(Habitat = case_when(Origin == "GUD" ~ "Alpine - early",
                             Origin == "RAM" ~ "Subalpine - early",
                             Origin == "SKJ" ~ "Alpine - late",
                             Origin == "VES" ~ "Subalpine - late")) %>% 
  ggplot(aes(x = meanTemp, y = DevelopmentRate, colour = Treatment, shape = Habitat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Origin", values = c(17, 18, 15, 16)) +
  facet_grid(Year ~ Treatment)


dat %>% 
  filter(Species == "LEO" & Variable == "Flower", Year == 2017) %>% 
  ungroup() %>% 
  mutate(Habitat = case_when(Origin == "GUD" ~ "Alpine - early",
                             Origin == "RAM" ~ "Subalpine - early",
                             Origin == "SKJ" ~ "Alpine - late",
                             Origin == "VES" ~ "Subalpine - late")) %>% 
  ggplot(aes(x = meanTemp, y = DevelopmentRate, colour = Treatment, shape = Habitat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Origin", values = c(17, 18, 15, 16)) +
  facet_grid( ~ Treatment)





# WARMER
dfPolli <- dat %>% 
  # Only want Control plants at Gudmedalen and SKJ
  filter(Origin != "VES" | Treatment != "Control",
         Origin != "RAM" | Treatment != "Control") %>% 
  filter(!(Species == "LEO" & Year == 2015)) %>% 
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  # use group by to do analysis for each year, species and pheno stage
  group_by(Year, Species) %>% 
  do(fit = lme(DevelopmentRate ~ meanTemp * Treatment, random = ~ 1 | NewBlock, data = .))

# Thermal threshold = intercept
tidy(dfPolli, fit, effects = "fixed") %>% 
  filter(term == "(Intercept)") %>% 
  ggplot(aes(x = Species, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, colour = factor(Year))) +
  geom_point() +
  geom_errorbar(width = 0.1)

# Heat use efficiency
tidy(dfPolli, fit, effects = "fixed") %>% 
  filter(term == "meanTemp") %>% 
  ggplot(aes(x = Species, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, colour = factor(Year))) +
  geom_point() +
  geom_errorbar(width = 0.1)


### LATERSM
dfPolli <- dat %>% 
  # Only want Control plants at Gudmedalen and SKJ
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "LaterSM")) %>% 
  filter(!(Species == "LEO" & Year == 2015)) %>% 
  # use group by to do analysis for each year, species and pheno stage
  group_by(Year, Species) %>% 
  do(fit = lme(DevelopmentRate ~ meanTemp * Treatment, random = ~ 1 | NewBlock, data = .))

# Thermal threshold = intercept
tidy(dfPolli, fit, effects = "fixed") %>% 
  filter(term == "(Intercept)") %>% 
  ggplot(aes(x = Species, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, colour = factor(Year))) +
  geom_point() +
  geom_errorbar(width = 0.1)

# Heat use efficiency
tidy(dfPolli, fit, effects = "fixed") %>% 
  filter(term == "meanTemp") %>% 
  ggplot(aes(x = Species, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, colour = factor(Year))) +
  geom_point() +
  geom_errorbar(width = 0.1)


### WARM AND LATESM
dfPolli <- dat %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  # select only controls and warmer
  filter(Treatment %in% c("Control", "WarmLate")) %>% 
  #filter(!(Species == "LEO" & Year == 2015)) %>% 
  # use group by to do analysis for each year, species and pheno stage
  group_by(Year, Species) %>% 
  do(fit = lme(DevelopmentRate ~ meanTemp, random = ~ 1 | NewBlock, data = .))

# Thermal threshold = intercept
tidy(dfPolli, fit, effects = "fixed") %>% 
  filter(term == "(Intercept)") %>% 
  ggplot(aes(x = Species, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, colour = factor(Year))) +
  geom_point() +
  geom_errorbar(width = 0.1)

# Heat use efficiency
tidy(dfPolli, fit, effects = "fixed") %>% 
  filter(term == "meanTemp") %>% 
  ggplot(aes(x = Species, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, colour = factor(Year))) +
  geom_point() +
  geom_errorbar(width = 0.1)




library("lme4")
dfP <- dat %>% 
  group_by(Species) %>% 
  do(fit = glmer(DevelopmentRate ~ meanTemp * Treatment + (1|NewBlock) + (1|Year), family = "binomial", data = .))

tidy(dfP, fit, effects = "fixed")

Pollination %>% 
  filter(Variable == "Flower") %>% 
  mutate(DevelopmentRate = 1/value) %>% 
  left_join(CumulativeTemp, by = c("Site", "Year", "SM",  "value" = "dssm")) %>% 
  ggplot(aes(x = cumTemp, y = DevelopmentRate, colour = Treatment)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Origin)




## WETTER- Bud, Flower, Seed and Ripe Seed date for both species ##
dfPolli <- dat %>% 
  # we only want Control plants at GUD and RAM
  filter(Origin != "VES" | Treatment != "Control",
         Origin != "RAM" | Treatment != "Control") %>% 
  # select only controls and warmer
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  group_by(Year, Species) %>% 
  do(fit = lme(DevelopmentRate ~ log(cumTemp), random = ~ 1 | NewBlock, data = .))

tidy(dfPolli, fit, effects = "fixed")

# get the summary statistics by group in a tidy data_frame
PlasticityCTWetter <- TidyResults(dfPolli, "LaterSM")
