data <- read.table("size2.csv", h=T, dec=",")
head(data)

### LIBRARIES
library("ggplot2")
library("tidyr")
library("dplyr")


# SUBSET DATA AND CREATE NEW VARIABLES
size2 <- data %>% 
  filter(sp == "RAN") %>% 
  select(site, orig, ID, ind, trt, size_end) %>% 
  #gather(key = variable, value = value, -sp, -site, -orig, -ID, -ind, -blk, -DOY, -Torig, -Porig, -Tdest, -Pdest, -trt) %>%
  #separate(variable, into = c("size.end"), sep = "\\.") %>%
  #mutate(size.end = plyr::mapvalues(size.end, c("size_end"), c("size"))) %>%
  mutate(trt = plyr::mapvalues(trt, c("c", "wa", "we", "ww"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(trt = factor(trt, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) #%>% 
  #mutate(OrigTempLevel = ifelse(Torig %in% c(5.87, 6.58), 1, 2)) %>%
  #mutate(OrigPrecLevel = ifelse(Porig %in% c(1925, 1848), 1, 2)) %>%
  #mutate(DestTempLevel = ifelse(Tdest %in% c(5.87, 6.58), 1, 2)) %>%
  #mutate(DestPrecLevel = ifelse(Pdest %in% c(1925, 1848), 1, 2))

head(size2)

### MAKING FIGURE
size2 %>% 
  #filter(sp == "RAN") %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(site, orig, trt) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = sd(size_end, na.rm = TRUE)/sqrt(N)) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = trt, shape = sp, color = sp), size = 3) +
  ylab(paste("")) + xlab(paste("Leaf growth [cm]")) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  facet_grid(~orig ~ sp, scales = "free")
  
############## RANUNCULUS

Ranunculus <- size[!(size$sp == "LEO"), ]

##### PLASTICITY

# Remove control plot at Veskre, not needed for this comparison
plasticity <- Ranunculus[!(Ranunculus$trt == "Control" & Ranunculus$site == "VES"), ]
plasticity %>% 
  filter(site == "VES")
  
plasticity$trt2 <- relevel(plasticity$trt, "Control")
  
### WARMING

plasticityWA <- plasticity %>% 
  filter(OrigTempLevel == 1)

fitWA.pl.ran <- glm(value ~ trt2 * as.factor(OrigPrecLevel), data = plasticityWA, subset = trt2 %in% c("Control", "Warm"), family = "Gamma")
summary(fitWA.pl.ran)

newdat <- expand.grid(trt2=c("Control", "Warm")
                      , OrigPrecLevel=c(1,2)
                      , value = 0
)
mm <- model.matrix(terms(fitWA.pl.ran), newdat)
newdat$value <- predict(fitWA.pl.ran,newdat, type="response")
newdat


### WET

plasticityWE <- plasticity %>%
   filter(OrigPrecLevel == 1)

fitWE.pl.ran <- glm(value ~ trt2 * as.factor(OrigTempLevel), data = plasticityWE, subset = trt2 %in% c("Control", "Wet"), family = "Gamma")

summary(fitWE.pl.ran)

newdat <- expand.grid(trt2=c("Control", "Wet")
					  , OrigTempLevel=c(1,2)
					  , value = 0
)
mm <- model.matrix(terms(fitWE.pl.ran), newdat)
newdat$value <- predict(fitWE.pl.ran,newdat, type="response")
newdat


### WARM AND WET

ww.pl.ran <- plasticity[(plasticity$trt2 %in% c("Control", "Warm & wet") & plasticity$orig == "GUD"), ]

fitWW.pl.ran <- glm(value ~ trt2 , data = ww.pl.ran, family = "Gamma")
summary(fitWW.pl.ran)
newdat <- expand.grid(trt2=c("Control", "Warm & wet")
                      , value = 0
)
mm <- model.matrix(terms(fitWW.pl.ran), newdat)
newdat$value <- predict(fitWW.pl.ran,newdat, type="response")
newdat


Anova(fitWA.pl.ran, test.statistic="Wald")
Anova(fitWE.pl.ran, test.statistic="Wald")
Anova(fitWW.pl.ran, test.statistic="Wald")


##### ADAPTATION

# Remove control plot at Gudmedalen, not needed for this comparison

adaptation <- Ranunculus[!(Ranunculus$trt == "Control" & Ranunculus$site == "GUD"), ]

adaptation$trt2 <- relevel(adaptation$trt, "Control")

# WARMING

adaptationWA <- adaptation %>%
	filter(DestTempLevel == 2)
	
fitWA.ad.ran <- glm(value ~ trt2 * as.factor(DestPrecLevel), data = adaptationWA, subset = trt2 %in% c("Control", "Warm"), family = "Gamma")

summary(fitWA.ad.ran)

newdat <- expand.grid(trt2=c("Control", "Warm")
                      , DestPrecLevel=c(1,2)
                      , value = 0
)
mm <- model.matrix(terms(fitWA.ad.ran), newdat)
newdat$value <- predict(fitWA.ad.ran,newdat, type="response")
newdat

# WET

adaptationWE <- adaptation %>%
	filter(DestPrecLevel == 2)

fitWE.ad.ran <- glm(value ~ trt2 * as.factor(DestTempLevel), data = adaptationWE, subset = trt2 %in% c("Control", "Wet"), family = "Gamma")

summary(fitWE.ad.ran)

newdat <- expand.grid(trt2=c("Control", "Wet")
					  , DestTempLevel=c(1,2)
					  , value = 0
)
mm <- model.matrix(terms(fitWE.ad.ran), newdat)
newdat$value <- predict(fitWE.ad.ran,newdat, type="response")
newdat

### WARM AND WET

ww.ad.ran <- adaptation[(adaptation$trt2 %in% c("Control", "Warm & wet") & adaptation$site == "VES"), ]

   
fitWW.ad.ran <- glm(value ~ trt2 , data = ww.ad.ran, family = "Gamma")

summary(fitWW.ad.ran)

newdat <- expand.grid(trt2=c("Control", "Warm & wet")
                      , value = 0
)
mm <- model.matrix(terms(fitWW.ad.ran), newdat)
newdat$value <- predict(fitWW.ad.ran,newdat, type="response")
newdat

Anova(fitWA.ad.ran, test.statistic="Wald")
Anova(fitWE.ad.ran, test.statistic="Wald")
Anova(fitWW.ad.ran, test.statistic="Wald")



############## LEONTODON

Leontodon <- size[!(size$sp == "RAN"), ]

##### PLASTICITY

# Remove control plot at Veskre, not needed for this comparison
plasticity.leo <- Leontodon[!(Leontodon$trt == "Control" & Leontodon$site == "VES"), ]
plasticity.leo %>% 
  filter(site == "VES")
  
plasticity.leo$trt2 <- relevel(plasticity.leo$trt, "Control")
  
### WARMING

plasticityWA.leo <- plasticity.leo %>% 
  filter(OrigTempLevel == 1)

fitWA.pl.leo <- glm(value ~ trt2 * as.factor(OrigPrecLevel), data = plasticityWA.leo, subset = trt2 %in% c("Control", "Warm"), family = "Gamma")
summary(fitWA.pl.leo)

newdat <- expand.grid(trt2=c("Control", "Warm")
                      , OrigPrecLevel=c(1,2)
                      , value = 0
)
mm <- model.matrix(terms(fitWA.pl.leo), newdat)
newdat$value <- predict(fitWA.pl.leo,newdat, type="response")
newdat


### WET

plasticityWE.leo <- plasticity.leo %>%
   filter(OrigPrecLevel == 1)

fitWE.pl.leo <- glm(value ~ trt2 * as.factor(OrigTempLevel), data = plasticityWE.leo, subset = trt2 %in% c("Control", "Wet"), family = "Gamma")

summary(fitWE.pl.leo)

newdat <- expand.grid(trt2=c("Control", "Wet")
					  , OrigTempLevel=c(1,2)
					  , value = 0
)
mm <- model.matrix(terms(fitWE.pl.leo), newdat)
newdat$value <- predict(fitWE.pl.leo,newdat, type="response")
newdat


### WARM AND WET

ww.pl.leo <- plasticity.leo[(plasticity.leo$trt2 %in% c("Control", "Warm & wet") & plasticity.leo$orig == "GUD"), ]

fitWW.pl.leo <- glm(value ~ trt2 , data = ww.pl.leo, family = "Gamma")
summary(fitWW.pl.leo)
newdat <- expand.grid(trt2=c("Control", "Warm & wet")
                      , value = 0
)
mm <- model.matrix(terms(fitWW.pl.leo), newdat)
newdat$value <- predict(fitWW.pl.leo,newdat, type="response")
newdat


Anova(fitWA.pl.leo, test.statistic="Wald")
Anova(fitWE.pl.leo, test.statistic="Wald")
Anova(fitWW.pl.leo, test.statistic="Wald")


##### ADAPTATION

# Remove control plot at Gudmedalen, not needed for this comparison

adaptation.leo <- Leontodon[!(Leontodon$trt == "Control" & Leontodon$site == "GUD"), ]

adaptation.leo$trt2 <- relevel(adaptation.leo$trt, "Control")

# WARMING

adaptationWA.leo <- adaptation.leo %>%
	filter(DestTempLevel == 2)
	
fitWA.ad.leo <- glm(value ~ trt2 * as.factor(DestPrecLevel), data = adaptationWA.leo, subset = trt2 %in% c("Control", "Warm"), family = "Gamma")

summary(fitWA.ad.leo)

newdat <- expand.grid(trt2=c("Control", "Warm")
                      , DestPrecLevel=c(1,2)
                      , value = 0
)
mm <- model.matrix(terms(fitWA.ad.leo), newdat)
newdat$value <- predict(fitWA.ad.leo,newdat, type="response")
newdat

# WET

adaptationWE.leo <- adaptation.leo %>%
	filter(DestPrecLevel == 2)

fitWE.ad.leo <- glm(value ~ trt2 * as.factor(DestTempLevel), data = adaptationWE.leo, subset = trt2 %in% c("Control", "Wet"), family = "Gamma")

summary(fitWE.ad.leo)

newdat <- expand.grid(trt2=c("Control", "Wet")
					  , DestTempLevel=c(1,2)
					  , value = 0
)
mm <- model.matrix(terms(fitWE.ad.leo), newdat)
newdat$value <- predict(fitWE.ad.leo,newdat, type="response")
newdat

### WARM AND WET

ww.ad.leo <- adaptation.leo[(adaptation.leo$trt2 %in% c("Control", "Warm & wet") & adaptation.leo$site == "VES"), ]

   
fitWW.ad.leo <- glm(value ~ trt2 , data = ww.ad.leo, family = "Gamma")

summary(fitWW.ad.leo)

newdat <- expand.grid(trt2=c("Control", "Warm & wet")
                      , value = 0
)
mm <- model.matrix(terms(fitWW.ad.leo), newdat)
newdat$value <- predict(fitWW.ad.leo,newdat, type="response")
newdat

Anova(fitWA.ad.leo, test.statistic="Wald")
Anova(fitWE.ad.leo, test.statistic="Wald")
Anova(fitWW.ad.leo, test.statistic="Wald")
