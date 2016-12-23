#### FIGURE ABOUT PHENOLOGY

## ----loadPhenology
library("lubridate")
library("ggplot2")
library("tidyr")
library("dplyr")
library("cowplot")

# load data
load("Ranunculus.RData")

# set the theme
th <- theme()


## ----FirstTrial
### FIGURE ### DA CONTROLLARE
### DOY
FirstTrial <- Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  ggplot() +
  ggtitle("First flowering") +
  geom_boxplot(aes(x= orig, y = value, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

print(FirstTrial) + th

## ----FirstTrialDogs
### DOGS
ggplot() +
  geom_boxplot(data = ran, aes(x= so, y = dogs.f, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

## ----SecondTrial
SecondTrial <- Ranunculus %>%
  filter(pheno.unit == "dogs") %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(site, orig, trt, pheno.unit, pheno.stage, OrigTempLevel, OrigPrecLevel) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(OrigTempLevel2 = plyr::mapvalues(OrigTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(OrigPrecLevel2 = plyr::mapvalues(OrigPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  mutate(grouping = paste(OrigTempLevel2, OrigPrecLevel2, sep = " ")) %>% 
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean value in days after Snowmelt")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~ grouping)

print(SecondTrial)

#################################################################################################################
### PLASTICITY
#################################################################################################################

## ----EffectSizePlot
### MAKING FIGURE
### PLOT FOR DIFFERENCE IN TREATMENT - CONTROL IN DOY
# Get Mean and SE
MeanSE <- Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "doy") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.unit, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = 2*sd(value, na.rm = TRUE)/sqrt(N))

MeanSE %>% filter(trt %in% c("Control", "Wetter")) %>% 
  ggplot(aes(x = trt, y = mean, color = orig, group = orig)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ pheno.stage)


# Calculate difference between Control and Treatment for SE
SEData <- MeanSE %>% 
  ungroup() %>% 
  select(-mean, -N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = se) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = SE, -orig, -pheno.unit, -pheno.stage, -Control) %>% # gather Treatments
  filter(!is.na(SE)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(orig, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("GUD_Warmer", "SKJ_Warmer", "GUD_Wetter", "RAM_Wetter", "GUD_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  select(SE, orig, pheno.stage, Treatment)

# Calculate difference between Control and Treatment for Mean
MeanData <- MeanSE %>% 
  ungroup() %>% 
  select(-se, -N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = mean) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = Effect, -orig, -pheno.unit, -pheno.stage, -Control) %>% # gather Treatments
  filter(!is.na(Effect)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(orig, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("GUD_Warmer", "SKJ_Warmer", "GUD_Wetter", "RAM_Wetter", "GUD_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  left_join(SEData, by = c("orig" = "orig", "pheno.stage" = "pheno.stage", "Treatment" = "Treatment")) %>%  # join SE
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "Wetter", "WarmWet"), c("Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Wetter", "Warm & wet"))) 
  

EffectSizePlot <- ggplot(MeanData, aes(x = newname, y = Effect, color = Treatment, ymax = Effect + SE, ymin = Effect - SE)) +
  geom_point(size = 1.8) +
  labs(x = "", y = "Treatment - control in days") +
  scale_colour_manual(name = "", values = c("red", "blue", "purple")) +
  geom_hline(yintercept=0, color = "gray") +
  facet_grid(~ pheno.stage) +
  geom_errorbar(width=0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(EffectSizePlot)
save_plot("EffectSizePlot_Doy.jpeg", EffectSizePlot,base_aspect_ratio = 1.8)


## ----EventPlot
### PLOT FOR ONSET AND DURATION OF EVENTS
EventMeanAndSE <- Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit != "doy") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"), c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit"))) %>%
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-dry", "Subalpine-dry", "Alpine-wet"))) %>%
  mutate(orig = factor(orig, levels = c("Alpine-dry", "Alpine-wet", "Subalpine-dry"))) %>%
  group_by(trt, site, orig, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = 2*sd(value, na.rm = TRUE)/sqrt(N))
  
EventSE <- EventMeanAndSE %>% 
  select(-mean, -N) %>% # remove mean
  spread(key = pheno.unit, value = se)
  
EventData <- EventMeanAndSE %>% 
  select(-se, -N) %>% # remove mean
  spread(key = pheno.unit, value = mean) %>% 
  left_join(EventSE, by = c("orig" = "orig", "site" = "site", "pheno.stage" = "pheno.stage", "trt" = "trt"),  suffix = c(".mean", ".se")) %>% # join SE
  ungroup(trt) %>% 
  mutate(trt = plyr::mapvalues(trt, c("Control", "Warmer", "Wetter", "WarmWet"), c("Control", "Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(trt = factor(trt, levels = c("Control", "Warmer", "Wetter", "Warm & wet"))) 

EventData %>% group_by(pheno.stage, trt) %>% summarise(mean = mean(days.x))
EventData %>% filter(trt %in% c("Control", "Wetter")) %>% 
  ggplot(aes(x = trt, y = days.mean, color = orig, group = orig)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ pheno.stage)
  

OnsetDurationEventPlot <- ggplot(EventData, aes(x = dogs.mean, y = days.mean, shape = pheno.stage, color = trt, group = trt, ymax = days.mean + days.se, ymin = days.mean - days.se, xmax = dogs.mean + dogs.se, xmin = dogs.mean - dogs.se)) +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "blue", "purple")) +
  scale_shape_manual(name = "Stage", values = c(16,17,15)) +
  labs(x = "Onset of event in days after snowmelt", y = "Duration between events in days") +
  geom_errorbar(width=0.18) +
  geom_errorbarh() +
  geom_line(linetype="dashed") +
  geom_point(size = 2) +
  facet_grid(~ orig)
 
save_plot("OnsetDurationEventPlotDOGS.jpeg", OnsetDurationEventPlot, base_aspect_ratio = 2)

ggplot(EventData, aes(x = doy.mean, y = days.mean, shape = pheno.stage, color = trt, group = trt, ymax = days.mean + days.se, ymin = days.mean - days.se, xmax = doy.mean + doy.se, xmin = doy.mean - doy.se)) +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "blue", "purple")) +
  scale_shape_manual(name = "Event", values = c(16,17,15)) +
  labs(x = "Onset of event in days after snowmelt", y = "Duration between events in days") +
  geom_errorbar(width=0.18) +
  geom_errorbarh() +
  geom_line(linetype="dashed") +
  geom_point(size = 2) +
  facet_grid(~ orig)

#################################################################################################################
### ADAPTATION
#################################################################################################################
## ----EffectSizePlotAdapt
### PLOT FOR DIFFERENCE IN TREATMENT - CONTROL IN DOY
# Get Mean and SE
MeanSEAdapt <- Ranunculus %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.unit, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = 2*sd(value, na.rm = TRUE)/sqrt(N)) %>% print(n = 24)

MeanSEAdapt %>% filter(trt %in% c("Control", "Wetter")) %>% 
  ggplot(aes(x = trt, y = mean, color = site, group = site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ pheno.stage)

# Calculate difference between Control and Treatment for SE
SEData <- MeanSEAdapt %>% 
  ungroup() %>% 
  select(-mean, -N, -orig) %>% # remove origin, because it causes problems
  spread(key = trt, value = se) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = SE, -site, -pheno.unit, -pheno.stage, -Control) %>% # gather Treatments
  filter(!is.na(SE)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(site, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("RAM_Warmer", "VES_Warmer", "SKJ_Wetter", "VES_Wetter", "VES_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  select(SE, site, pheno.stage, Treatment)

# Calculate difference between Control and Treatment for Mean
MeanDataAdapt <- MeanSEAdapt %>% 
  ungroup() %>% 
  select(-se, -N, -orig) %>% # remove orig, because it causes problems
  spread(key = trt, value = mean) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = Effect, -site, -pheno.unit, -pheno.stage, -Control) %>% # gather Treatments
  filter(!is.na(Effect)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(site, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("RAM_Warmer", "VES_Warmer", "SKJ_Wetter", "VES_Wetter", "VES_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  left_join(SEData, by = c("site" = "site", "pheno.stage" = "pheno.stage", "Treatment" = "Treatment")) %>%  # join SE
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "Wetter", "WarmWet"), c("Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Wetter", "Warm & wet"))) 


EffectSizePlotAdapt <- ggplot(MeanDataAdapt, aes(x = newname, y = Effect, color = Treatment, ymax = Effect + SE, ymin = Effect - SE)) +
  geom_hline(yintercept=0, color = "gray") +
  geom_point(size = 1.8) +
  labs(x = "", y = "Treatment - control in days") +
  scale_colour_manual(name = "", values = c("red", "blue", "purple")) +
  facet_grid(~ pheno.stage) +
  geom_errorbar(width=0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(EffectSizePlotAdapt)
save_plot("EffectSizePlotAdapt_Doy.jpeg", EffectSizePlotAdapt,base_aspect_ratio = 1.8)


## ----EventPlot
### PLOT FOR ONSET AND DURATION OF EVENTS
EventMeanAndSEAdapt <- Ranunculus %>% 
  filter(site != "GUD") %>% # remove Gudmedalen
  filter(pheno.unit != "doy") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"), c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit"))) %>%
  mutate(site = plyr::mapvalues(site, c("RAM", "VES", "SKJ"), c("Subalpine-dry", "Sublpine-wet", "Alpine-wet"))) %>%
  mutate(site = factor(site, levels = c("Subalpine-dry", "Sublpine-wet", "Alpine-wet"))) %>%
  group_by(trt, site, orig, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = 2*sd(value, na.rm = TRUE)/sqrt(N))

EventSEAdapt <- EventMeanAndSEAdapt %>% 
  select(-mean, -N) %>% # remove mean
  spread(key = pheno.unit, value = se)

EventDataAdapt <- EventMeanAndSEAdapt %>% 
  select(-se, -N) %>% # remove mean
  spread(key = pheno.unit, value = mean) %>% 
  left_join(EventSEAdapt, by = c("orig" = "orig", "site" = "site", "pheno.stage" = "pheno.stage", "trt" = "trt"),  suffix = c(".mean", ".se")) %>%  # join SE
  ungroup(trt) %>% 
  mutate(trt = plyr::mapvalues(trt, c("Control", "Warmer", "Wetter", "WarmWet"), c("Control", "Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(trt = factor(trt, levels = c("Control", "Warmer", "Wetter", "Warm & wet")))

EventDataAdapt %>% group_by(pheno.stage, trt) %>% summarise(mean = mean(days.x))
EventDataAdapt %>% filter(trt %in% c("Control", "Wetter")) %>% 
  ggplot(aes(x = trt, y = days.mean, color = site, group = site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ pheno.stage)

OnsetDurationEventPlotAdapt <- ggplot(EventDataAdapt, aes(x = dogs.mean, y = days.mean, shape = pheno.stage, color = trt, group = trt, ymax = days.mean + days.se, ymin = days.mean - days.se, xmax = dogs.mean + dogs.se, xmin = dogs.mean - dogs.se)) +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "blue", "purple")) +
  scale_shape_manual(name = "Stage", values = c(16,17,15)) +
  labs(x = "Onset of event in days after snowmelt", y = "Duration between events in days") +
  geom_errorbar(width=0.18) +
  geom_errorbarh() +
  geom_line(linetype="dashed") +
  geom_point(size = 2) +
  facet_grid(~ site)

save_plot("OnsetDurationEventPlotAdaptDOGS.jpeg", OnsetDurationEventPlotAdapt, base_aspect_ratio = 2)

