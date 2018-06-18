#### CUMULATIVE TEMPERTUATRE ####

# load the data
source("Merging 2015 and 2017 data.R")
source("ClimateData.R")


##############################
#### ORIGIN - PLASTICITY  ####
##############################

Pollination17 <- Pollination %>% 
  filter(Year == 2017) %>% 
  # remove Second Flowers
  filter(!Pollination == "")

### Join cumulative temperature with phenology data by Origin
CumulativeTemperature <- Pollination17 %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  left_join(CumulativeTemp, by = c("Site" = "Site", "value" = "dssm", "Year"))


### SNWOMELT ###
SMDiff <- CumulativeTemperature %>% 
  select(Origin, Treatment, Year, Species, SM) %>% 
  distinct(Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin)


# Calculate mean
MeanVariables <- CumulativeTemperature %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  group_by(Species, Year, Treatment, Site, Origin, Variable) %>% 
  summarise(N = sum(!is.na(cumTemp)), mean = mean(cumTemp, na.rm = TRUE), se = sd(cumTemp, na.rm = TRUE)/sqrt(N))

# Number of observations per group
Number <- MeanVariables %>% 
  select(Species, Year, Treatment, Origin, Site, Variable, N)

DiffVariables <- MeanVariables %>% 
  ungroup(Site) %>% 
  select(-Site,-N) %>% 
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
  select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -Species, -Year, -Origin, -Variable) %>%
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% 
  inner_join(Number, by = c("Species", "Year", "Origin", "Treatment", "Variable"))


### PHENOLOGY ###

# Which tests are significant
Significance0 <- Plasticity_warmer1 %>% 
  bind_rows(Plasticity_wetter1, Plasticity_warmwet1) %>%
  filter(term %in% c("TreatmentWarmer", "TreatmentLaterSM", "TreatmentWarmLate")) %>% 
  mutate(signif = ifelse(p.value < 0.05, 1, 0)) %>% 
  mutate(Treatment = substr(term, 10, nchar(term))) %>% 
  mutate(signif = ifelse(Species == "LEO" & Variable %in% c("Flower", "Seed") & Treatment == "Warmer", 1, signif))

SP <- c(LEO = "Leontodon autumnalis", RAN = "Ranunculus acris")
VAR <- c(EndSize = "Longest leaf", RepOutput = "Reproductive output")

PhenologyCumTPlastic <- DiffVariables %>% 
  left_join(SMDiff, by = c("Species", "Year", "Origin", "Treatment")) %>% 
  #left_join(Significance0, by = c("Species", "Variable", "Treatment")) %>% 
  mutate(Variable = factor(Variable, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  #mutate(shape1 = factor(paste(Treatment, signif, sep = "_"))) %>% 
  #mutate(shape1 = factor(shape1, levels = c("Warmer_0", "Warmer_1", "Later SM_0", "Later SM_1", "Warm & late SM_0", "Warm & late SM_1"))) %>%
  #ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = shape1, alpha = N < 5, linetype = N < 5, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, alpha = N < 5, linetype = N < 5, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  #scale_shape_manual(name = "Treatment:", values = c(2, 17, 1, 16, 0, 15)) +
  scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(y = "Difference in onset of stage [cumulative temperature > 1°C] \n after SMT between treatment and origin-control", x = "Difference in SMT between origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 200, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -300, label = "earlier", color = "grey20") +
  theme(legend.position = "none", 
        text = element_text(size = 10),
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 10), 
        strip.text.x = element_text(face = "italic")) +
  facet_grid(Variable ~ Species, labeller=labeller(Species = SP))
ggsave(PhenologyCumTPlastic, filename = "FinalFigures/PhenologyCumTPlastic.pdf", height = 6, width = 8)




###*******************************************************************************

####################################
#### DESTINATION - ADAPTATION ####
####################################


### Join cumulative temperature with phenology data by Site
CumulativeTemperature <- Pollination17 %>% 
  filter(Variable %in% c("Bud", "Flower", "Seed")) %>% 
  left_join(CumulativeTemp, by = c("Site" = "Site", "value" = "dssm", "Year"))



### SNOWMELT ###
SMDiffAdapt <- CumulativeTemperature %>% 
  select(Site, Origin, Treatment, Year, Species, SM) %>% 
  distinct(Site, Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin)


# Calculate mean
MeanVariablesAdapt <- CumulativeTemperature %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  group_by(Species, Year, Treatment, Site, Origin, Variable) %>% 
  summarise(N = sum(!is.na(cumTemp)), mean = mean(cumTemp, na.rm = TRUE), se = sd(cumTemp, na.rm = TRUE)/sqrt(N))

# Number of observations per group
NumberAdapt <- MeanVariables %>% 
  select(Species, Year, Treatment, Origin, Site, Variable, N)


DiffVariablesAdapt <- MeanVariablesAdapt %>% 
  ungroup(Origin) %>% 
  select(-Origin,-N) %>% 
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
  select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -Species, -Year, -Site, -Variable) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% 
  inner_join(Number, by = c("Species", "Year", "Site", "Treatment", "Variable"))


### PHENOLOGY ###

# Which tests are significant
Significance2 <- Adapt_warmer1 %>% 
  bind_rows(Adapt_wetter1, Adapt_warmwet1) %>%
  filter(term %in% c("TreatmentWarmer", "TreatmentLaterSM", "TreatmentWarmLate")) %>% 
  mutate(signif = ifelse(p.value < 0.05, 1, 0)) %>% 
  mutate(Treatment = substr(term, 10, nchar(term)))

PhenologyCumTAdapt <- DiffVariablesAdapt %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Treatment")) %>% 
  #left_join(Significance2, by = c("Species", "Variable", "Treatment")) %>% 
  mutate(Variable = factor(Variable, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  #mutate(shape1 = factor(paste(Treatment, signif, sep = "_"))) %>%
  #mutate(shape1 = factor(shape1, levels = c("Warmer_0", "Warmer_1", "Later SM_0", "Later SM_1", "Warm & late SM_0", "Warm & late SM_1"))) %>%
  #ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = shape1, alpha = N < 5, linetype = N < 5, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) +
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, alpha = N < 5, linetype = N < 5, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  #scale_shape_manual(name = "Treatment:", values = c(2, 17, 1, 16, 0, 15)) +
  scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(y = "Difference in onset of stage [cumulative temperature > 1°C] \n after SMT between treatment and destination-control", x = "Difference in SMT between origin and destination site [days]", title = "Genetic difference: destination-control") +
  geom_errorbar(width=0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 80, label = "later", color = "grey30") +
  annotate(geom = "text", x = 25, y = -400, label = "earlier", color = "grey30") +
  theme(legend.position = "none", 
        text = element_text(size = 10),
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 10), 
        strip.text.x = element_text(face = "italic")) +
  facet_grid(Variable ~ Species, labeller=labeller(Species = SP))
ggsave(PhenologyCumTAdapt, filename = "FinalFigures/PhenologyCumTAdapt.pdf", height = 6, width = 8)
