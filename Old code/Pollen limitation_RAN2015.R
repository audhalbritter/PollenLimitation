library("readxl")

data <- read.csv("data_pollenlimitaiton_Sept16.csv", sep=";")
head(data)

data2017 <- read_excel(path = "~/Dropbox/Pollen limitation/Data/PollinationDataSheet_2017.xlsx", sheet = 1, col_names = TRUE, col_types = c(rep("text", 6), rep("numeric", 2), "date", rep("text", 3), "date", "numeric", rep("date", 9), "numeric", "text"))
head(data2017)

### LIBRARIES
library("lubridate")
library("ggplot2")
library("tidyverse")
library("cowplot")
library("readxl")

# SUBSET DATA AND CREATE NEW VARIABLES
Ranunculus <- data %>% 
  filter(sp == "RAN") %>% # only Ranunculus
  select(- s.ter, -s.ter.1, -s.ter.2, -s.ter.3, -s.ter.4, -doy.bs, -dogs.bs, -doy.rs, -dogs.rs) %>% 
  mutate(block = c(rep(1, 10), rep(2, 10), rep(3, 10), rep(c(1,1,1,2,2), 5), rep(c(1,1,2,2,2), 5), rep(1, 35), rep(2, 35), rep(1, 60), rep(2, 59))) %>% 
  
  # prob. flowering
  mutate(prob.fl = ifelse(is.na(doy.f), 0, 1)) %>% 
  
  # calculate days between sm and bud, flower and seed
  #mutate(days.smb = doy.bp - sm, days.smf = doy.f - sm, days.sms = doy.s - sm) %>% 
  # calculate days between sm and bud, bud and flower, flower and seed
  mutate(days.smb = doy.bp - sm, days.smf = doy.f - doy.bp, days.sms = doy.s - doy.f) %>% 
  gather(key = variable, value = value, -sp, -site, -orig, -ID, -ind, -TD, -PD, -TO, -PO, -trt, -sm, -block, -prob.fl) %>%
  separate(variable, into = c("pheno.unit", "pheno.stage"), sep = "\\.") %>%
  
  #na.omit(value) %>% 
  
  # Make Code nice
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("bp", "f",  "s", "smb", "smf", "sms"), c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"))) %>%
  mutate(trt = plyr::mapvalues(trt, c("c", "wa", "we", "ww"), c("Control", "Warmer", "LaterSM", "WarmLate"))) %>%
  mutate(trt = factor(trt, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(OrigTempLevel = ifelse(TO %in% c(5.87, 6.58), 1, 2)) %>%
  mutate(OrigPrecLevel = ifelse(PO %in% c(1925, 1848), 1, 2)) %>%
  mutate(DestTempLevel = ifelse(TD %in% c(5.87, 6.58), 1, 2)) %>%
  mutate(DestPrecLevel = ifelse(PD %in% c(1925, 1848), 1, 2)) %>% 

  # add Plant size
  #left_join(size, by = c("sp", "site", "orig", "ID", "ind", "trt")) %>% 
  
  # Cumulative Temperature after snowmelt
  mutate(doy = ifelse(pheno.unit == "doy", value, ifelse(pheno.unit == "dogs", (value + sm), NA))) %>% # get doy for each observation
  left_join(climateData, by = c("site" = "site", "doy" = "doy")) %>% 
  mutate(CumTempAfterSM = ifelse(pheno.unit == "days", NA, CumTempAfterSM)) # does not make sense for durations

save(Ranunculus, file = "Ranunculus.RData")
head(Ranunculus)

### FIGURES
### DOY
ggplot() +
  geom_boxplot(data = ran, aes(x= so, y = doy.f, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

### DOGS
ggplot() +
  geom_boxplot(data = ran, aes(x= so, y = dogs.f, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

### MAKING FIGURE
Ranunculus %>% 
  group_by(site, orig, trt, pheno.unit, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm=TRUE)) %>% 
  filter(pheno.stage %in% c("Bud", "Flower")) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = trt, shape = pheno.stage, color = pheno.stage), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_grid(~site ~ pheno.unit, scales = "free")

PP <- Ranunculus %>% 
  mutate(OrigTempLevel = plyr::mapvalues(OrigTempLevel, c("1", "2"), c("alpine", "subalpine"))) %>% 
  group_by(OrigTempLevel, trt, pheno.unit, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm=TRUE)) %>% 
  filter(pheno.stage %in% c("Bud", "Flower"), pheno.unit == "doy") %>% 
  ggplot() +
  geom_point(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage), size = 3) +
  xlab(paste("")) + ylab(paste("Day of the year")) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_wrap(~ OrigTempLevel, nrow = 2)

PP + theme_grey(base_size = 20) + theme(legend.title=element_blank())

LA <- Ranunculus %>% 
  mutate(DestTempLevel = plyr::mapvalues(DestTempLevel, c("1", "2"), c("alpine", "subalpine"))) %>% 
  group_by(DestTempLevel, trt, pheno.unit, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm=TRUE)) %>% 
  filter(pheno.stage %in% c("Bud", "Flower"), pheno.unit == "doy") %>% 
  ggplot() +
  geom_point(aes(x = trt, y = mean, shape = pheno.stage, color = pheno.stage), size = 3) +
 xlab(paste("")) + ylab(paste("Days of the year")) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_wrap(~DestTempLevel, nrow = 2)

LA + theme_grey(base_size = 20) + theme(legend.title=element_blank())

##### PLASTICITY
# Remove control plot at Veskre, not needed for this comparison
plasticity <- Ranunculus[!(Ranunculus$trt == "Control" & Ranunculus$site == "VES"), ]


### WARMING
plasticity2 <- plasticity %>% filter(OrigTempLevel == 1, pheno.stage == "Flower", pheno.unit == "doy")

fit <- glm(dogs.f ~ trt * OrigPrecLevel, data = plasticity, subset = trt %in% c("Control", "Warm"), family = "poisson")
summary(fit)
newdat <- expand.grid(trt=c("Control", "Warm")
                      , OrigPrecLevel=c(1,2)
                      , dogs.f = 0
)
mm <- model.matrix(terms(fit), newdat)
newdat$dogs.f <- predict(fit,newdat, type="response")
newdat

#### Plants transplanted to warmer climate flower earlier in the season (doy) compared to plants from colder climate
#### Plants transplanted to warmer climate flower earlier after snowmelt than plants in colder climate


### WET
fit <- glm(dogs.f ~ trt * OrigTempLevel, data = plasticity, subset = trt %in% c("Control", "Wet"), family = "poisson")
summary(fit)
newdat <- expand.grid(trt=c("Control", "Wet")
                      , OrigTempLevel=c(1,2)
                      , dogs.f = 0
)
mm <- model.matrix(terms(fit), newdat)
newdat$dogs.f <- predict(fit,newdat, type="response")
newdat

# not much difference in flowering along precipitation gradient, it is more important where you are along the gradient


### WARM AND WET
ww <- plasticity[(plasticity$trt %in% c("Control", "Warm & wet") & plasticity$orig == "GUD"), ]
fit <- glm(doy.f ~ trt , data = ww, family = "poisson")
summary(fit)
newdat <- expand.grid(trt=c("Control", "Warm & wet")
                      , doy.f = 0
)
mm <- model.matrix(terms(fit), newdat)
newdat$doy.f <- predict(fit,newdat, type="response")
newdat

# plants transplanted to warmer and wetter climate flower earlier in the season and after snowmelt compared to plants from drier and colder sites


###### ADAPTATION
# Remove control plot at Veskre, not needed for this comparison
adaptation <- Ranunculus[!(Ranunculus$trt == "Control" & Ranunculus$site == "GUD"), ]


#### DOY
### WARM
fit <- glm(dogs.f ~ trt * DestPrecLevel, data = adaptation, subset = trt %in% c("Control", "Warm"), family = "poisson")
summary(fit)
newdat <- expand.grid(trt=c("Control", "Warm")
                      , DestPrecLevel=c(1,2)
                      , dogs.f = 0
)
mm <- model.matrix(terms(fit), newdat)
newdat$dogs.f <- predict(fit,newdat, type="response")
newdat

# no difference for doy. I find that a bit surprising...
# plants from colder sites flower earlier after snowmelt compared to plants form warmer sites, but more pronounced at drier end of gradient



### WET
fit <- glm(dogs.f ~ trt * DestTempLevel, data = adaptation, subset = trt %in% c("Control", "Wet"), family = "poisson")
summary(fit)
newdat <- expand.grid(trt=c("Control", "Wet")
                      , DestTempLevel=c(1,2)
                      , dogs.f = 0
)
mm <- model.matrix(terms(fit), newdat)
newdat$dogs.f <- predict(fit,newdat, type="response")
newdat

# nod difference in flowering along along prec gradient, elevations seems more important


### WARM AND WET
ww <- adaptation[(adaptation$trt %in% c("Control", "Warm & wet") & adaptation$site == "VES"), ]
fit <- glm(dogs.f ~ trt , data = ww, family = "poisson")
summary(fit)
newdat <- expand.grid(trt=c("Control", "Warm & wet")
                      , dogs.f = 0
)
mm <- model.matrix(terms(fit), newdat)
newdat$dogs.f <- predict(fit,newdat, type="response")
newdat

# no difference in flowering for warmer and wetter




  

data2017 %>% 
  filter(Species == "RAN") %>% 
  ggplot() +
  geom_boxplot(aes(x= Origin, y = Date_bp)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple")) +
  facet_wrap(~ Site)
