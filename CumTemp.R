#### CUMULATIVE TEMPERTUATRE ####

# load the data
source("Merging 2015 and 2017 data.R")
source("MyFunctions.R")
source("ClimateData.R")
source("Analysis 2017 CumT.R")
library("dplyr")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


##############################
#### ORIGIN - PLASTICITY  ####
##############################


### SNWOMELT ###
SMDiff <- CumulativeTemperature %>% 
  dplyr::select(Origin, Treatment, Year, Species, SM) %>% 
  distinct(Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin)


# Calculate mean
MeanVariables <- CumulativeTemperature %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  group_by(Species, Year, Treatment, Site, Origin, Variable) %>% 
  summarise(N1 = sum(!is.na(cumTemp)), mean1 = mean(cumTemp, na.rm = TRUE), se1 = sd(cumTemp, na.rm = TRUE)/sqrt(N1),
            N2 = sum(!is.na(value)), mean2 = mean(value, na.rm = TRUE), se2 = sd(value, na.rm = TRUE)/sqrt(N2)) %>% 
  mutate(N = ifelse(Variable %in% c("Bud", "Flower", "Seed"), N1, N2),
         mean = ifelse(Variable %in% c("Bud", "Flower", "Seed"), mean1, mean2),
         se = ifelse(Variable %in% c("Bud", "Flower", "Seed"), se1, se2)) %>% 
  dplyr::select(-N1, -N2, -mean1, -mean2, -se1, -se2)

# Number of observations per group
Number <- MeanVariables %>% 
  dplyr::select(Species, Year, Treatment, Origin, Site, Variable, N)

DiffVariables <- MeanVariables %>% 
  ungroup(Site) %>% 
  dplyr::select(-Site,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = Treatment, value = united) %>% 
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("Warmer_mean", "Warmer_se"), sep = "_", convert = TRUE) %>% 
  separate(col = LaterSM, into = c("LaterSM_mean", "LaterSM_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmLate, into = c("WarmLate_mean", "WarmLate_se"), sep = "_", convert = TRUE) %>% 
  # calculate difference between control and treatment in mean
  mutate(Warmer_mean = Warmer_mean - Control_mean, LaterSM_mean = LaterSM_mean - Control_mean, WarmLate_mean = WarmLate_mean - Control_mean) %>% 
  # calculate SE for difference
  mutate(Warmer_se = sqrt(Control_se^2 + Warmer_se^2), LaterSM_se = sqrt(Control_se^2 + LaterSM_se^2), WarmLate_se = sqrt(Control_se^2 + WarmLate_se^2)) %>% 
  dplyr::select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -Species, -Year, -Origin, -Variable) %>%
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% 
  inner_join(Number, by = c("Species", "Year", "Origin", "Treatment", "Variable"))


### PHENOLOGY ###

ResultsPlastic <- Plasticity_cumTPhenology %>%
  filter(Treatment %in% c("Warmer", "LaterSM", "WarmLate"))

SP <- c(LEO = "Leontodon autumnalis", RAN = "Ranunculus acris")
VAR <- c(EndSize = "Longest leaf", RepOutput = "Reproductive output")
STAGE <- c(Bud = "Bud", Flower = "Flower", Seed = "Fruit")
PRODUCTION <- c(EndSize = "Longest leaf", RepOutput = "Reproductive output")

PhenologyCumTPlastic <- DiffVariables %>% 
  left_join(SMDiff, by = c("Species", "Year", "Origin", "Treatment")) %>% 
  left_join(ResultsPlastic, by = c("Species", "Year", "Variable", "Treatment")) %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  mutate(Variable = factor(Variable, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  # change order of species
  mutate(Species = factor(Species, levels = c("RAN", "LEO"))) %>% 
  filter(N >= 5) %>% 
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = factor(Year), alpha = factor(signif), ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Year", values = c(15, 17)) +
  scale_alpha_manual(name = "Significance", values = c(0.4, 1)) +
  labs(y = "Difference in onset of stage [cumulative temperature > 1°C] \n after SMT between treatment and origin-control", x = "Shift in the timing of snowmelt due to transplant \n between origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 200, label = "higher", color = "grey20") +
  annotate(geom = "text", x = 25, y = -250, label = "lower", color = "grey20") +
  theme(#legend.position = "none",
        text = element_text(size = 10),
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 10),
        strip.text.x = element_text(face = "italic")) +
  facet_grid(Variable ~ Species, labeller=labeller(Species = SP, Variable = STAGE))
ggsave(PhenologyCumTPlastic, filename = "PhenologyCumTPlastic.jpg", height = 6, width = 8)




### BIOMASS AND SEEDS ###
# Which tests are significant
Plasticity_Growth <- read_excel(path = "Plasticity_Growth.xlsx")

ProductionPlastic <- DiffVariables %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>% 
  left_join(SMDiff, by = c("Species", "Year", "Origin", "Treatment")) %>% 
  left_join(Plasticity_Growth, by = c("Species", "Year", "Variable", "Treatment")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Species = factor(Species, levels = c("RAN", "LEO"))) %>% 
  filter(N >= 5) %>% 
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = factor(Year), alpha = factor(signif), ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Year", values = c(15, 17)) +
  scale_alpha_manual(name = "Significance", values = c(0.4, 1)) +
  labs(y = "Difference in longest leave [cm] or reproductive \n output [g] after SMT between treatment and origin-control", x = "Shift in the timing of snowmelt due to transplant \n between origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  theme(#legend.position = "none",
    text = element_text(size = 10),
    axis.text=element_text(size = 10),
    axis.title=element_text(size = 10),
    strip.text.x = element_text(face = "italic")) +
  facet_grid(Variable ~ Species, scales = "free_y", labeller=labeller(Species = SP, Variable = PRODUCTION))

ggsave(ProductionPlastic, filename = "ProductionPlastic.jpg", height = 4, width = 6)


###*******************************************************************************

####################################
#### DESTINATION - ADAPTATION ####
####################################

### SNOWMELT ###
SMDiffAdapt <- CumulativeTemperature %>% 
  dplyr::select(Site, Origin, Treatment, Year, Species, SM) %>% 
  distinct(Site, Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin)


# Calculate mean
MeanVariablesAdapt <- CumulativeTemperature %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  group_by(Species, Year, Treatment, Site, Origin, Variable) %>% 
  summarise(N1 = sum(!is.na(cumTemp)), mean1 = mean(cumTemp, na.rm = TRUE), se1 = sd(cumTemp, na.rm = TRUE)/sqrt(N1),
          N2 = sum(!is.na(value)), mean2 = mean(value, na.rm = TRUE), se2 = sd(value, na.rm = TRUE)/sqrt(N2)) %>% 
  mutate(N = ifelse(Variable %in% c("Bud", "Flower", "Seed"), N1, N2),
         mean = ifelse(Variable %in% c("Bud", "Flower", "Seed"), mean1, mean2),
         se = ifelse(Variable %in% c("Bud", "Flower", "Seed"), se1, se2)) %>% 
  dplyr::select(-N1, -N2, -mean1, -mean2, -se1, -se2)


# Number of observations per group
NumberAdapt <- MeanVariables %>% 
  dplyr::select(Species, Year, Treatment, Origin, Site, Variable, N)


DiffVariablesAdapt <- MeanVariablesAdapt %>% 
  ungroup(Origin) %>% 
  dplyr::select(-Origin,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = Treatment, value = united) %>% 
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("Warmer_mean", "Warmer_se"), sep = "_", convert = TRUE) %>% 
  separate(col = LaterSM, into = c("LaterSM_mean", "LaterSM_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmLate, into = c("WarmLate_mean", "WarmLate_se"), sep = "_", convert = TRUE) %>% 
  # calculate difference between control and treatment in mean
  mutate(Warmer_mean = Warmer_mean - Control_mean, LaterSM_mean = LaterSM_mean - Control_mean, WarmLate_mean = WarmLate_mean - Control_mean) %>% 
  # calculate SE for difference
  mutate(Warmer_se = sqrt(Control_se^2 + Warmer_se^2), LaterSM_se = sqrt(Control_se^2 + LaterSM_se^2), WarmLate_se = sqrt(Control_se^2 + WarmLate_se^2)) %>% 
  dplyr::select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -Species, -Year, -Site, -Variable) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% 
  inner_join(Number, by = c("Species", "Year", "Site", "Treatment", "Variable"))


### PHENOLOGY ###
ResultsAdapt <- Adapt_cumTPhenology %>% 
  filter(Treatment %in% c("Warmer", "LaterSM", "WarmLate"))

PhenologyCumTAdapt <- DiffVariablesAdapt %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Origin", "Treatment")) %>% 
  left_join(ResultsAdapt, by = c("Species", "Year", "Variable", "Treatment")) %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  mutate(Variable = factor(Variable, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  # change order of species
  mutate(Species = factor(Species, levels = c("RAN", "LEO"))) %>% 
  filter(N >= 5) %>% 
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = factor(Year), alpha = factor(signif), ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Year:", values = c(15, 17)) +
  scale_alpha_manual(name = "Significance:", values = c(0.4, 1)) +
  labs(y = "Difference in onset of stage [cumulative temperature > 1°C] \n after SMT between treatment and destination-control", x = "Shift in the timing of snowmelt due to transplant \n between origin and destination site [days]", title = "Genetic difference: destination-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 200, label = "higher", color = "grey20") +
  annotate(geom = "text", x = 25, y = -250, label = "lower", color = "grey20") +
  theme(#legend.position = "none",
    text = element_text(size = 10),
    axis.text=element_text(size = 10),
    axis.title=element_text(size = 10),
    strip.text.x = element_text(face = "italic")) +
  facet_grid(Variable ~ Species, labeller=labeller(Species = SP, Variable = STAGE))

ggsave(PhenologyCumTAdapt, filename = "PhenologyCumTAdapt.jpg", height = 6, width = 8)


### BIOMASS AND SEEDS ###
# Which tests are significant
Adapt_Growth <- read_excel(path = "Adapt_Growth.xlsx")

ProductionAdapt <- DiffVariablesAdapt %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Origin", "Treatment")) %>% 
  left_join(Adapt_Growth, by = c("Species", "Year", "Variable", "Treatment")) %>% 
  filter(Variable %in% c("EndSize", "RepOutput")) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  # change order of species
  mutate(Species = factor(Species, levels = c("RAN", "LEO"))) %>% 
  filter(N >= 5) %>% 
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = factor(Year), alpha = factor(signif), ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("#E69F00", "#56B4E9", "#D55E00")) +
  scale_shape_manual(name = "Year", values = c(15, 17)) +
  scale_alpha_manual(name = "Significance", values = c(0.4, 1)) +
  labs(y = "Difference in longest leave [cm] or reproductive output [g] \n after SMT between treatment and destination-control", x = "Shift in the timing of snowmelt due to transplant \n between origin and destination site [days]", title = "Genetic difference: destination-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  theme(#legend.position = "none",
    text = element_text(size = 10),
    axis.text=element_text(size = 10),
    axis.title=element_text(size = 10),
    strip.text.x = element_text(face = "italic")) +
  facet_grid(Variable ~ Species, scales = "free_y", labeller=labeller(Species = SP, Variable = PRODUCTION))

ggsave(ProductionAdapt, filename = "ProductionAdapt.jpg", height = 4, width = 6)
