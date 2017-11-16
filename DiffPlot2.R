#### ORIGIN - PHENOTYPIC PLASTICITY ####

### SNWOMELT ###
SMDiff <- Pollination %>% 
  select(Origin, Treatment, Year, Species, SM) %>% 
  distinct(Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin)
  #mutate(Origin = plyr::mapvalues(Origin, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late")))


### PHENOLOGY ###
MeanVariables <- Pollination %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  #mutate(Origin = plyr::mapvalues(Origin, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>% 
  #mutate(Origin = factor(Origin, levels = c("Alpine-early", "Alpine-late", "Subalpine-early"))) %>%
  group_by(Species, Year, Treatment, Site, Origin, Variable) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))

Number <- MeanVariables %>% 
  select(Species, Year, Treatment, Origin, Site, Variable, N) %>% 
  filter(N > 0)

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


PhenologyPlastic <- DiffVariables %>% 
  filter(Year == 2017, Variable %in% c("Bud", "Flower", "Seed")) %>% 
  left_join(SMDiff, by = c("Species", "Year", "Origin", "Treatment")) %>% 
  mutate(Variable = factor(Variable, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = Treatment, alpha = N < 5, linetype = N< 5, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(17, 16, 15)) +
  scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  #scale_fill_manual(name = "Year:", values = c("white", "red")) +
  labs(y = "Difference in onset of stage [days] after SMT \n between treatment and origin-control", x = "Difference in SMT between origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 13, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -25, label = "earlier", color = "grey20") +
  theme(legend.position = "none") +
  facet_grid(Variable ~ Species)
ggsave(PhenologyPlastic, filename = "PhenologyPlastic.pdf", height = 6, width = 8)



### BIOMASS ###

ProductionPlastic <- DiffVariables %>% 
  filter(Year == 2017, Variable %in% c("EndSize", "RepOutput")) %>% 
  left_join(SMDiff, by = c("Species", "Year", "Origin", "Treatment")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = Treatment, alpha = N < 5, linetype = N< 5, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(17, 16, 15)) +
  scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  labs(y = "Difference in onset of stage [days] after SMT \n between treatment and origin-control", x = "Difference in SMT between origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  #annotate(geom = "text", x = 25, y = 13, label = "later", color = "grey20") +
  #annotate(geom = "text", x = 25, y = -25, label = "earlier", color = "grey20") +
  theme(legend.position = "none") +
  facet_grid(Variable ~ Species, scales = "free")

ggsave(ProductionPlastic, filename = "ProductionPlastic.pdf", height = 6, width = 6)





###********************************************************************************************
#### DESTINATION - ADAPTATION ####

### SNOWMELT ###
SMDiffAdapt <- Pollination %>% 
  select(Site, Origin, Treatment, Year, Species, SM) %>% 
  distinct(Site, Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin) %>%
  mutate(Site = plyr::mapvalues(Site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late")))



### PHENOLOGY ###
EventDiffAdaptData <- Pollination %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  mutate(Site = plyr::mapvalues(Site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  mutate(Site = factor(Site, levels = c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  group_by(Species, Year, Treatment, Site, Origin, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))


EventDiffAdapt <- EventDiffAdaptData %>% 
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
  gather(key = Treatment, value = united, -Species, -Year, -Site, -pheno.stage) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  #mutate(mean = replace(mean, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  #mutate(se = replace(se, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  filter(!is.na(mean))



EventAdapt <- EventDiffAdapt %>% 
  filter(Year == 2017, pheno.stage != "Ripe Seed") %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Treatment")) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  #scale_shape_manual(name = "Treatment:", values = c(16, 17)) +
  #scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  #scale_fill_manual(name = "Year:", values = c("white", "red")) +
  labs(y = "Difference in onset of stage [days] after SMT \n between treatment and destination-control", x = "Difference in SMT between origin and destination site [days]", title = "Genetic difference: destination-control") +
  geom_errorbar(width=0.18) +
  geom_point(size = 3) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 13, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -25, label = "earlier", color = "grey20") +
  theme(legend.position = "none") +
  facet_grid(Species ~ pheno.stage)
ggsave(EventAdapt, filename = "EventAdapt.pdf", height = 6, width = 8)



### BIOMASS ###
BiomassDiffDataAdapt <- Pollination %>% 
  filter(Origin != "GUD" | Treatment != "Control", pheno.stage == "Bud") %>% # remove Control at Gudmedalen
  mutate(Site = plyr::mapvalues(Site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  mutate(Site = factor(Site, levels = c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  group_by(Species, Year, Treatment, Site, Origin) %>% 
  summarise(N = sum(!is.na(EndSize)), mean = mean(EndSize, na.rm = TRUE), se = sd(EndSize, na.rm = TRUE)/sqrt(N))



BiomassDiffAdapt <- BiomassDiffDataAdapt %>% 
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
  gather(key = Treatment, value = united, -Species, -Year, -Site) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  #mutate(mean = replace(mean, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  #mutate(se = replace(se, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  filter(!is.na(mean))


BiomassAdapt <- BiomassDiffAdapt %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Treatment")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  filter(Year == 2017) %>% 
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + #linetype = Origin
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  #scale_shape_manual(name = "Flowering:", values = c(1, 16)) +
  labs(y = "Difference in biomass [g] \n between treatment and destination-control", x = "Difference in SMT between origin and destination site [days]", title = "Genetic differentiation: destination-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 4) +
  panel_border(colour = "black") +
  #annotate(geom = "text", x = 25, y = 13, label = "higher", color = "grey20") +
  #annotate(geom = "text", x = 25, y = -25, label = "lower", color = "grey20") +
  facet_grid( ~ Species)
ggsave(BiomassAdapt, filename = "BiomassAdapt.pdf", height = 4, width = 8)



### REP.OUTPUT ###
RepOutputDiffDataAdapt <- Pollination %>% 
  filter(Origin != "GUD" | Treatment != "Control", pheno.stage == "Bud", Year == 2017) %>% # remove Control at Gudmedalen
  mutate(Site = plyr::mapvalues(Site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  mutate(Site = factor(Site, levels = c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  group_by(Species, Year, Treatment, Site, Origin) %>% 
  summarise(N = sum(!is.na(RepOutput)), mean = mean(RepOutput, na.rm = TRUE), se = sd(RepOutput, na.rm = TRUE)/sqrt(N))


RepOutputDiffAdapt <- RepOutputDiffDataAdapt %>% 
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
  gather(key = Treatment, value = united, -Species, -Year, -Site) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean))


RepOutputAdapt <- RepOutputDiffAdapt %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Treatment")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se)) + #linetype = Origin
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  #scale_shape_manual(name = "Flowering:", values = c(1, 16)) +
  labs(y = "Difference in reproductive output [g] \n between treatment and destination-control", x = "Difference in SMT between origin and destination site [days]", title = "Genetic differentiation: destination-control") +
  geom_errorbar(width = 0.18) +
  geom_point(size = 4) +
  panel_border(colour = "black") +
  #annotate(geom = "text", x = 25, y = 13, label = "higher", color = "grey20") +
  #annotate(geom = "text", x = 25, y = -25, label = "lower", color = "grey20") +
  facet_grid( ~ Species)
ggsave(RepOutputAdapt, filename = "RepOutputAdapt.pdf", height = 4, width = 8)


