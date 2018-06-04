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

size %>%
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(site, orig, trt) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = sd(size_end, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(orig2 = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ", "VES"), c("Alpine dry", "Subalpine dry", "Alpine wet", "Subalpine wet"))) %>%
  ggplot(aes(x = trt, y = mean, color = site, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  geom_errorbar(width=0.2) +
  facet_grid(~orig) +
  coord_flip()



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
  filter(pheno.unit == "doy") %>% 
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



Ranunculus %>% 
  filter(pheno.unit == "doy", pheno.stage == "Flower") %>% 
  ggplot() +
  ggtitle("First flowering") +
  geom_boxplot(aes(x= site, y = value, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

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
