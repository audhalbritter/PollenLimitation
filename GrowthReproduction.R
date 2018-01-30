# load the data
source("Merging 2015 and 2017 data.R")

Pollination17 <- Pollination %>% 
  filter(Year == 2017) %>% 
  # remove Second Flowers
  filter(!Pollination == "")


# Is there a corrleation/trade-off between Longest leaf and Cumulative Fitness?
# Longest leaf and CumFitness
dat <- Pollination17 %>% 
  filter(Pollination == "control", Variable %in% c("Growth", "EndSize", "RepOutput")) %>% 
  spread(key = Variable, value = value) %>% 
  # calculate CumFitness (RepOutput * NrFlowers)
  mutate(CumFitness = RepOutput * NrFlowers) %>% 
  filter(!is.na(CumFitness))


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
  # centre Plevel
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE))

fit <- lm(log(CumFitness) ~ EndSize + Treatment + DestPLevel.cen + EndSize:Treatment + EndSize:DestPLevel.cen, dd)
summary(fit)
fix.check(fit)

# Only Leontodon
dd <- dat %>%
  filter(Treatment %in% c("Control", "Warmer"), Species == "LEO") %>% 
  filter(Origin == "SKJ")

fit <- lm(log(CumFitness) ~ EndSize * Treatment, dd)
summary(fit)
fix.check(fit)



### LATERSM
# Only enough data for LEO
dd <- dat %>%
  filter(Treatment %in% c("Control", "LaterSM"), Species == "LEO") %>% 
  filter(Origin != "SKJ" | Treatment != "Control") %>%  
  filter(Origin != "VES" | Treatment != "Control") %>%  
  # centre Plevel
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE))

fit <- lm(log(CumFitness) ~ EndSize + Treatment + DestTLevel.cen + EndSize:Treatment + EndSize:DestTLevel.cen, dd)
summary(fit)
fix.check(fit)


### WARMLATE
dd <- dat %>%
  filter(Treatment %in% c("Control", "WarmLate"), Species == "LEO") %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control")

fit <- lm(log(CumFitness) ~ EndSize * Treatment, dd)
summary(fit)
fix.check(fit)



# Is plant size and nr of flowers correlated

# calculate Probability of flowering
ProbFlower <- Pollination17 %>% 
  filter(Pollination == "control", !is.na(NrFlowers)) %>% 
  group_by(Species, Site, Treatment, Flowering) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n))

# subset data
dat2 <- Pollination17 %>% 
  filter(Pollination == "control", Variable %in% c("EndSize")) %>% 
  # centre P and T-level
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  left_join(ProbFlower, by = c("Species", "Site", "Treatment", "Flowering")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM")))
  

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


### POLLEN LIMITATION
dat3 <- Pollination17 %>% 
  filter(Variable %in% c("RepOutput"), Species == "LEO", !is.na(value)) %>% 
  # centre P and T-level
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  left_join(ProbFlower, by = c("Species", "Site", "Treatment", "Flowering")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM")))


dat3 %>% 
  filter(Treatment %in% c("Control", "Warmer", "Later SM")) %>% 
  filter(Site != "GUD") %>%  
  filter(Site != "RAM") %>%  
  ggplot(aes(x = Pollination, y = value, fill = Pollination)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "yellow")) +
  labs(x = "", y = "Reproductive Output in g") +
  facet_grid(Treatment ~ Site)

