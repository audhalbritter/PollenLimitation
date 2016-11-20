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
  filter(pheno.unit == "doy", pheno.stage == "Flower") %>% 
  ggplot() +
  ggtitle("First flowering") +
  geom_boxplot(aes(x= orig, y = value, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

print(FirstTrial)

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

## ----EffectSizePlot
### MAKING FIGURE
## PLASTICITY
### PLOT FOR DIFFERENCE IN TREATMENT - CONTROL IN DOY
# Get Mean and SE
MeanSE <- Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "doy") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.unit, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))


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
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Wetter", "WarmWet"))) 
  

EffectSizePlot <- ggplot(MeanData, aes(x = newname, y = Effect, color = Treatment, ymax = Effect + SE, ymin = Effect - SE)) +
  geom_point(size = 1.8) +
  labs(x = "", y = "Treatment - control in days") +
  scale_colour_manual(name = "", values = c("red", "blue", "purple")) +
  geom_hline(yintercept=0, color = "gray") +
  facet_grid(~ pheno.stage) +
  geom_errorbar(width=0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(EffectSizePlot)
#save_plot("EffectSizePlot_Doy.jpeg", EffectSizePlot,base_aspect_ratio = 1.8)


## ----EventPlot
### PLOT FOR ONSET AND DURATION OF EVENTS
EventMeanAndSE <- Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit != "doy") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"), c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-dry", "Subalpine-dry", "Alpine-wet"))) %>%
  mutate(orig = factor(orig, levels = c("Alpine-dry", "Alpine-wet", "Subalpine-dry"))) %>%
  group_by(trt, site, orig, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))
  
EventSE <- EventMeanAndSE %>% 
  select(-mean, -N) %>% # remove mean
  spread(key = pheno.unit, value = se)
  
EventData <- EventMeanAndSE %>% 
  select(-se, -N) %>% # remove mean
  spread(key = pheno.unit, value = mean) %>% 
  left_join(EventSE, by = c("orig" = "orig", "site" = "site", "pheno.stage" = "pheno.stage", "trt" = "trt"),  suffix = c(".mean", ".se"))  # join SE
  

OnsetDurationEventPlot <- ggplot(EventData, aes(x = dogs.x, y = days.x, shape = pheno.stage, color = trt, group = trt, ymax = days.x + days.y, ymin = days.x - days.y, xmax = dogs.x + dogs.y, xmin = dogs.x - dogs.y)) +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "blue", "purple")) +
  scale_shape_manual(name = "Event", values = c(16,17,15)) +
  labs(x = "Onset of event in days after Snowmelt", y = "Duration between events in days") +
  geom_errorbar(width=0.2) +
  geom_errorbarh() +
  geom_line(linetype="dashed") +
  geom_point() +
  facet_grid(~ orig)
 
print(OnsetDurationEventPlot) 
#save_plot("OnsetDurationEventPlotDOGS.jpeg", OnsetDurationEventPlot, base_aspect_ratio = 2)





## ----Rest
### Plasticity Figure
Ranunculus %>%
  filter(pheno.unit == "dogs") %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(site, orig, trt, pheno.unit, pheno.stage, OrigTempLevel, OrigPrecLevel) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(OrigTempLevel2 = plyr::mapvalues(OrigTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(OrigPrecLevel2 = plyr::mapvalues(OrigPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  mutate(grouping = paste(OrigTempLevel2, OrigPrecLevel2, sep = " ")) %>% 
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean value in days after SM")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  facet_grid(~ grouping)


# Cumulative Temperature
Ranunculus %>%
  filter(pheno.unit == "dogs") %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(site, orig, trt, pheno.unit, pheno.stage, OrigTempLevel, OrigPrecLevel) %>% 
  summarise(N = sum(!is.na(CumTempAfterSM)), mean = mean(CumTempAfterSM, na.rm = TRUE), se = sd(CumTempAfterSM, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(OrigTempLevel2 = plyr::mapvalues(OrigTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(OrigPrecLevel2 = plyr::mapvalues(OrigPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  mutate(grouping = paste(OrigTempLevel2, OrigPrecLevel2, sep = " ")) %>% 
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean cumulative temperature after SM")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  facet_grid(~ grouping)



### Adaptation Figure
Ranunculus %>%
  filter(pheno.unit == "days") %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  group_by(site, orig, trt, pheno.unit, pheno.stage, DestTempLevel, DestPrecLevel) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(DestTempLevel2 = plyr::mapvalues(DestTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(DestPrecLevel2 = plyr::mapvalues(DestPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  mutate(grouping = paste(DestTempLevel2, DestPrecLevel2, sep = " ")) %>% 
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean value in days after SM")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  facet_grid(~ grouping)


### Adaptation Figure
Ranunculus %>%
  filter(pheno.unit == "dogs") %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  group_by(site, orig, trt, pheno.unit, pheno.stage, DestTempLevel, DestPrecLevel) %>% 
  summarise(N = sum(!is.na(CumTempAfterSM)), mean = mean(CumTempAfterSM, na.rm = TRUE), se = sd(CumTempAfterSM, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(DestTempLevel2 = plyr::mapvalues(DestTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(DestPrecLevel2 = plyr::mapvalues(DestPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  mutate(grouping = paste(DestTempLevel2, DestPrecLevel2, sep = " ")) %>% 
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean cumulative temperature after SM")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  facet_grid(~ grouping)


### Adaptation Figure
Ranunculus %>% 
  group_by(site, orig, trt, pheno.unit, pheno.stage, DestTempLevel, DestPrecLevel) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(DestTempLevel2 = plyr::mapvalues(DestTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(DestPrecLevel2 = plyr::mapvalues(DestPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean value")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_grid(DestTempLevel2 ~ DestPrecLevel2, scales = "free")



###### FIGURE ABOUT LEAF GROWTH

data <- read.table("size.csv", h=T, dec=",")
head(data)

### LIBRARIES
library("ggplot2")
library("tidyr")
library("dplyr")
library(car)

# SUBSET DATA AND CREATE NEW VARIABLES
size <- data %>%  
  gather(key = variable, value = value, -sp, -site, -orig, -ID, -ind, -blk, -DOY, -Torig, -Porig, -Tdest, -Pdest, -trt) %>%
  separate(variable, into = c("size.end"), sep = "\\.") %>%
  mutate(size.end = plyr::mapvalues(size.end, c("size_end"), c("size"))) %>%
  mutate(trt = plyr::mapvalues(trt, c("c", "wa", "we", "ww"), c("Control", "Warmer", "Wetter", "Warmer & wetter"))) %>%
  mutate(trt = factor(trt, levels = rev(c("Control", "Warmer", "Wetter", "Warmer & wetter")))) %>% 
  mutate(OrigTempLevel = ifelse(Torig %in% c(5.87, 6.58), 1, 2)) %>%
  mutate(OrigPrecLevel = ifelse(Porig %in% c(1925, 1848), 1, 2)) %>%
  mutate(DestTempLevel = ifelse(Tdest %in% c(5.87, 6.58), 1, 2)) %>%
  mutate(DestPrecLevel = ifelse(Pdest %in% c(1925, 1848), 1, 2))

head(size)

### MAKING FIGURE
size %>% 
  group_by(site, orig, trt, sp) %>% 
  summarise(mean = mean(value, na.rm=TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = trt, shape = sp, color = sp), size = 3) +
  ylab(paste("")) + xlab(paste("Leaf growth [cm]")) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_grid(~site ~ sp, scales = "free")
