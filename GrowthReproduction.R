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


#### CORRELATION BETWEEN GROWTH AND PROBABILITY OF FLOWERING ####
# Is plant size and nr of flowers correlated

# calculate Probability of flowering
ProbFlower <- Pollination17 %>% 
  filter(Pollination == "control", !is.na(NrFlowers)) %>% 
  group_by(Species, Site, Treatment, Flowering) %>%
  summarise (n = n()) %>%
  mutate(prob = n / sum(n))



# subset only growth and join with ProbFlower
dat2 <- Pollination17 %>% 
  filter(Pollination == "control", Variable %in% c("EndSize")) %>% 
  # centre P and T-level
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  left_join(ProbFlower, by = c("Species", "Site", "Treatment", "Flowering")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM")))
  

### Only control plants
dat2 %>% 
  filter(Treatment == "Control") %>% 
  ggplot(aes(x = value, y = prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(x = "Longest leaf", y = "Probability of flowering") +
  facet_grid(Species ~ Site)

# test these ralationships, all, per site, per elevation/prec
# if I find anything, then I can test the treatments

dd <- dat2 %>% 
  filter(Treatment == "Control", Species == "LEO") %>% 
  # centre Plevel
  mutate(OrigTLevel.cen = scale(OrigTLevel, scale = FALSE)) %>% 
  filter(OrigTLevel == 6.5)

fit1 <- glm(Flowering ~ 1, dd, family = "binomial")
fit2 <- glm(Flowering ~ value, dd, family = "binomial")
anova(fit1, fit2)


ggplot(dat2, aes(x = value, y = prop, color = Treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  labs(x = "Longest leaf", y = "Probability of flowering") +
  facet_grid(Species ~ Site)



dd <- dat2 %>% 
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control")

mylogit <- glm(Flowering ~ value + Treatment + DestPLevel.cen + value:Treatment + value:DestPLevel.cen, data = dd, family = "binomial")
summary(mylogit)
confint(mylogit)
 
## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))



Pollination %>% 
  filter(Year == 2015, Variable == "Flower", Pollination == "control") %>% 
  mutate(Flowering = ifelse(is.na(value), 0, 1)) %>% 
  group_by(Species, Site, Treatment, Flowering) %>%
  summarise (n = n()) %>%
  mutate(prob = n / sum(n))



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