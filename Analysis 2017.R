### ANALYSIS ###
library("lme4")
library("MuMIn")

dat <- Pollination %>% 
  left_join(climate, by = c("Year" = "year", "Site" = "site", "value" = "doy")) %>% 
  filter(Year == 2017) %>% 
  mutate(OrigPLevelRescale = (OrigPLevel - min(OrigPLevel))/(max(OrigPLevel) - min(OrigPLevel))) %>% 
  mutate(OrigTLevelRescale = (OrigTLevel - min(OrigTLevel))/(max(OrigTLevel) - min(OrigTLevel)))


dat2 <- dat %>% 
  filter(Species == "LEO", pheno.stage == "Flower") %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  # remove Control at Veskre
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(!is.na(value))
  

fit <- glm(value ~ OrigPLevelRescale * Treatment, data = dat2, family = "poisson")
summary(fit)


new.dat <- data.frame(expand.grid(OrigPLevel = c(2000, 2700),
                      Treatment = c("Control", "Warmer")))
new.dat$pred <- exp(predict(fit, new.dat))
new.dat

dat2 <- dat %>% 
  filter(Species == "LEO", pheno.stage == "Flower")

fit <- glmer(value ~ Treatment + OrigPLevelRescale + OrigTLevelRescale + Treatment:OrigPLevelRescale + Treatment:OrigTLevelRescale + (1|NewBlock), data = dat2, family = "poisson")
summary(fit)


