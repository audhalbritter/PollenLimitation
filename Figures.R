#### FIGURE ABOUT PHENOLOGY

data <- read.csv("data_pollenlimitaiton_Sept16.csv", sep=";")
head(data)

### LIBRARIES
library("ggplot2")
library("tidyr")
library("dplyr")

# SUBSET DATA AND CREATE NEW VARIABLES
Ranunculus <- data %>% 
  filter(sp == "RAN") %>% # only Ranunculus
  select(- s.ter, -s.ter.1, -s.ter.2, -s.ter.3, -s.ter.4, -doy.bs, -dogs.bs, -doy.rs, -dogs.rs, -doy.bp, -doy.f, -doy.s) %>% 
  gather(key = variable, value = value, -sp, -site, -orig, -ID, -TD, -PD, -TO, -PO, -trt, -sm) %>%
  separate(variable, into = c("pheno.unit", "pheno.stage"), sep = "\\.") %>%
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("bp", "f",  "s"), c("Bud", "Flower", "Fruit"))) %>%
  mutate(trt = plyr::mapvalues(trt, c("c", "wa", "we", "ww"), c("Control", "Warmer", "Wetter", "Warmer & wetter"))) %>%
  mutate(trt = factor(trt, levels = c("Control", "Warmer", "Wetter", "Warmer & wetter"))) %>% 
  mutate(OrigTempLevel = ifelse(TO %in% c(5.87, 6.58), 1, 2)) %>%
  mutate(OrigPrecLevel = ifelse(PO %in% c(1925, 1848), 1, 2)) %>%
  mutate(DestTempLevel = ifelse(TD %in% c(5.87, 6.58), 1, 2)) %>%
  mutate(DestPrecLevel = ifelse(PD %in% c(1925, 1848), 1, 2))

head(Ranunculus)

### FIGURE ### DA CONTROLLARE
### DOY
ggplot() +
  geom_boxplot(data = ran, aes(x= so, y = doy.f, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

### DOGS
ggplot() +
  geom_boxplot(data = ran, aes(x= so, y = dogs.f, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))
  
limits <- aes(ymax = resp + se, ymin=resp - se)
### MAKING FIGURE

### Plasticity Figure
Ranunculus %>%
  group_by(site, orig, trt, pheno.unit, pheno.stage, OrigTempLevel, OrigPrecLevel) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(OrigTempLevel2 = plyr::mapvalues(OrigTempLevel, c("1", "2"), c("Alpine", "Subalpine"))) %>%
  mutate(OrigPrecLevel2 = plyr::mapvalues(OrigPrecLevel, c("1", "2"), c("dry", "wet"))) %>%
  ggplot(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage, ymax = mean + se, ymin = mean - se)) +
  geom_point(size = 3) +
  ylab(paste("Mean value in days after snowmelt")) + xlab(paste("")) +
  geom_errorbar(width=0.2) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_grid(OrigTempLevel2 ~ OrigPrecLevel2, scales = "free")


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
