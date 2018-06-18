# load the data
source("Merging 2015 and 2017 data.R")

Pollination17 <- Pollination %>% 
  filter(Year == 2017) %>% 
  # remove Second Flowers
  filter(!Pollination == "")

#### CORRELATION BETWEEN GROWTH AND REPRODUCTION ####
# Is there a corrleation/trade-off between Longest leaf and Cumulative Fitness?
# Longest leaf and CumFitness
dat <- Pollination17 %>% 
  filter(Pollination == "control", Variable %in% c("Growth", "EndSize", "RepOutput")) %>% 
  spread(key = Variable, value = value) %>% 
  # calculate CumFitness (RepOutput * NrFlowers)
  mutate(CumFitness = RepOutput * NrFlowers) %>% 
  filter(!is.na(CumFitness))


### CONTROL PLANTS
# Relationship between size and reproduciton in control plants
dd <- dat %>% 
  filter(Treatment == "Control") %>% 
  ggplot(aes(x = EndSize, y = RepOutput)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ Site)

summary(lm((RepOutput) ~ EndSize + Site, dd))
# no relationship


dat %>% 
  filter(Site != "GUD") %>% 
  filter(Site != "RAM") %>% 
  filter(Treatment != "LaterSM" | Species != "RAN") %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>% 
  ggplot(aes(x = EndSize, y = CumFitness, color = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  facet_grid(Species ~ Site)



### WARMER
dd <- dat %>%
  filter(Treatment %in% c("Control", "Warmer"), Species == "RAN") %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin == "SKJ") 
  # centre Plevel
  #mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE))

fit <- lm((RepOutput) ~ EndSize + Treatment, dd)
summary(fit)
fix.check(fit)

# Only Leontodon
dd <- dat %>%
  filter(Treatment %in% c("Control", "Warmer"), Species == "LEO") %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin == "SKJ")

fit <- lm((RepOutput) ~ EndSize * Treatment, dd)
summary(fit)
fix.check(fit)



### LATERSM
dd <- dat %>%
  filter(Treatment %in% c("Control", "LaterSM"), Species == "LEO") %>% 
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # centre Plevel
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE))

fit <- lm(log(RepOutput) ~ EndSize + Treatment + DestTLevel.cen + EndSize:Treatment + EndSize:DestTLevel.cen, dd)
summary(fit)
fix.check(fit)


dd <- dat %>%
  filter(Treatment %in% c("Control", "LaterSM"), Species == "RAN") %>% 
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>% 
  filter(Origin == "GUD")

fit <- lm((RepOutput) ~ EndSize + Treatment, dd)
summary(fit)
fix.check(fit)


### WARMLATE
dd <- dat %>%
  filter(Treatment %in% c("Control", "WarmLate"), Species == "RAN") %>% 
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control")

fit <- lm((RepOutput) ~ EndSize * Treatment, dd)
summary(fit)
fix.check(fit)


dd <- dat %>%
  filter(Treatment %in% c("Control", "WarmLate"), Species == "LEO") %>% 
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control")

fit <- lm(log(RepOutput) ~ EndSize * Treatment, dd)
summary(fit)
fix.check(fit)




### POLLEN LIMITATION
dat3 <- Pollination17 %>% 
  filter(Variable %in% c("RepOutput"), Species == "LEO", !is.na(value)) %>% 
  # centre P and T-level
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  left_join(ProbFlower, by = c("Species", "Site", "Treatment", "Flowering")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM")))

# Only Leontodon and only at Veskre
dat3 %>% 
  filter(Treatment %in% c("Control", "Warmer", "Later SM"), Site == "VES") %>% 
  filter(Site != "GUD") %>%  
  filter(Site != "RAM") %>%  
  ggplot(aes(x = Pollination, y = value, fill = Pollination)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "yellow")) +
  labs(x = "", y = "Reproductive Output in g") +
  facet_grid( ~ Treatment)

dd <- dat3 %>% 
  filter(Treatment %in% c("Control", "Warmer", "Later SM"), Site == "VES") %>% 
  filter(Pollination == "pollination")

summary(lm(value ~ Treatment, dd))

dat3 %>% filter(Site == "VES") %>% group_by(Treatment, Pollination) %>% summarise(n = n())
### only pollinated plants in LaterSM have higher reproduction