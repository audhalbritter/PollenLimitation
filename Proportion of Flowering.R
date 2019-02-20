##################################################################
#### RELASHINSHIP BETWEEN GROWTH AND PROBABILITY OF FLOWERING ####
##################################################################
source("Merging 2015 and 2017 data.R")

library("lme4")
library("broom")

# Select data and           
# calculate Probability of flowering
ProbFlower <- Pollination %>% 
  # remove Second Flowers
  filter(!Pollination == "") %>% 
  filter(!is.na(NrFlowers), 
         Variable %in% c("EndSize")) %>% # only control plants, remove if unknown if plant flowered, only one variable
  group_by(Year, Species, Site, Treatment) %>%
  mutate(nPlant = n()) %>% 
  group_by(Year, Species, Site, Treatment, Flowering) %>%
  mutate(n = n(),
         ProbFl = n / nPlant) %>% 
  ungroup() %>% 
  # centre P and T-level
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE),
         DestTLevel.cen = scale(DestTLevel, scale = FALSE),
         value.cen = scale(value, scale = FALSE)) 
  #mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  #mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM")))

# Plot everything
SP <- c(LEO = "L. autumnalis", RAN = "R. acris")
ProbFlowerPlot <- ProbFlower %>% 
  mutate(Species = factor(Species, levels = c("RAN", "LEO"))) %>% 
  ggplot(aes(x = value, y = ProbFl, color = Treatment, linetype = factor(Year))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  labs(x = "Longest leaf in cm", y = "Probability of flowering") +
  facet_grid(Species ~ Site, labeller=labeller(Species = SP)) + 
  theme(strip.text.y = element_text(face = "italic"))
#ggsave(ProbFlowerPlot, filename = "FinalFigures/ProbFlowerPlot.jpg", height = 5, width = 8)

# Check if there is a difference between pollinated and non-pollinated flowers. There is not!
summary(glm(ProbFl ~ Pollination + Species + Year, ProbFlower, family = "binomial", weights = nPlant))

ggplot(ProbFlower, aes(x = Pollination, y = ProbFl)) +
  geom_boxplot() +
  facet_grid(Year ~ Species)


# Only test plants at Veskre - Ranunculus
dfProbRan <- ProbFlower %>% 
  filter(Site == "VES",
         !is.na(value), 
         Species == "RAN") %>%  
  group_by(Species, Year) %>% 
  do(mylogit = glmer(Flowering ~ value * Treatment + (1|NewBlock), data = ., family = "binomial"))

tidy(dfProbRan, mylogit, effect = "fixed") %>% 
  mutate(estimate = (round(estimate, 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))

ProbPlotRan <- augment(dfProbRan, mylogit) %>% 
  ggplot(aes(x = value, y = plogis(.fitted), colour = Treatment)) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  labs(x = "Longest leaf in cm", y = "Probability of flowering") +
  facet_grid( ~ Year) 
  #facet_grid(Year ~ Species, labeller=labeller(Species = SP)) + 
  #theme(strip.text.x = element_text(face = "italic"))
ggsave(ProbPlot, filename = "FinalFigures/ProbPlot.jpg", height = 5, width = 8)


# Only at Veskre - Leontodon 2017
dfProbLeo <- ProbFlower %>% 
  filter(Site == "VES",
         !is.na(value), 
         Species == "LEO", 
         Year == 2017) %>%  
  # fix overdispersion
  mutate(OD = rnorm(266)) %>% 
  group_by(Species) %>% 
  do(mylogit = glmer(Flowering ~ value * Treatment + (1|NewBlock) + (1|OD), data = ., family = "binomial"))

fit <- glmer(Flowering ~ value * Treatment + (1|NewBlock) + (1|OD), data = dfProbLeo, family = "binomial")
summary(fit2)

ss <- getME(fit,c("theta","fixef"))
fit2 <- update(fit,start=ss,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e3)))

tidy(dfProbLeo, mylogit, effect = "fixed") %>% 
  mutate(estimate = (round(estimate, 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))

newdata <- ProbFlower %>% 
  filter(Site == "VES",
         !is.na(value), 
         Species == "LEO", 
         Year == 2017) %>% 
  distinct(value, Treatment) %>% 
  mutate(NewBlock = 1,
         OD = 0,
         fitted = predict(fit, newdata = newdata, allow.new.levels = TRUE))

#augment(dfProbLeo, mylogit), newdata, allow.new.levels = TRUE) %>% 
#augment(fit2, effect = "fixed") %>% 

ggplot(newdata, aes(x = value, y = plogis(fitted), colour = Treatment)) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Treatment", values = c("#999999", "#E69F00", "#56B4E9", "#D55E00")) +
  labs(x = "Longest leaf in cm", y = "Probability of flowering")












### WARMER
dfProb <- ProbFlower %>% 
  filter(Treatment %in% c("Control", "Warmer")) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "RAM" | Treatment != "Control") %>% 
  filter(!is.na(value), !is.na(ProbFl)) %>% 
  group_by(Species) %>% 
  do(mylogit = glm(Flowering ~ value + Treatment * DestPLevel.cen + value:Treatment + value:DestPLevel, data = ., family = "binomial"))

ProbFl_warmer1 <- tidy(dfProb, mylogit) %>% 
  mutate(estimate = (round(exp(estimate), 2)), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(p.value = round(p.value, 3))


confint(mylogit)

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))


### LATER SM
subPF <- ProbFlower %>% 
  filter(Treatment %in% c("Control", "Later SM")) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>% 
  filter(!is.na(value), !is.na(ProbFl)) %>% 
  filter(Species == "RAN")

mylogit <- glmer(Flowering ~ value + Treatment + DestTLevel.cen + value:Treatment + value:DestTLevel.cen + (1|NewBlock), data = subPF, family = "binomial")
summary(mylogit)
fix.check(mylogit)



### WARM & LATE SM
subPF <- ProbFlower %>% 
  filter(Treatment %in% c("Control", "Warm & late SM")) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>%  
  filter(Origin != "SKJ" | Treatment != "Control") %>% 
  filter(Origin != "RAM" | Treatment != "Control") %>% 
  filter(!is.na(value), !is.na(ProbFl)) %>% 
  filter(Species == "RAN")

mylogit <- glmer(Flowering ~ value * Treatment + (1|NewBlock), data = subPF, family = "binomial")
summary(mylogit)


### 2015 DATA #### only RAN
# Probability of flowering 2015 data
Polli2015 <- read_excel(path = "Data/2015/data_pollenlimitaiton_Sept16.xls", sheet = 1, col_types = )

Treats <- data_frame(Origin = c("GUD", "GUD", "GUD", "GUD", "RAM", "RAM", "SKJ", "SKJ", "VES"),
            Site = c("GUD", "RAM", "SKJ", "VES", "RAM", "VES", "SKJ", "VES", "VES"),
           Treatment = c("Control", "Warmer", "Later", "WarmLate", "Control", "Later", "Control", "Warmer", "Control"))

ProbFlower15 <- Polli2015 %>% 
  filter(SP == "RAN", Treatment == "control") %>% 
  select(Site, Origin, `DOY (f)`, `size_end [cm]`) %>% 
  rename(FlowerDOY = `DOY (f)`, EndSize = `size_end [cm]`) %>% 
  mutate(EndSize = as.numeric(EndSize)) %>% 
  mutate(Flowering = ifelse(is.na(FlowerDOY), 0, 1)) %>% 
  left_join(Treats, by = c("Origin", "Site")) %>% 
  group_by(Site, Treatment) %>% 
  mutate(nPlant = n()) %>% 
  group_by(Site, Treatment, Flowering) %>%
  mutate(n = n()) %>%
  mutate(prob = n / nPlant)


ggplot(ProbFlower15, aes(x = EndSize, y = prob, color = Treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  labs(x = "Longest leaf", y = "Probability of flowering") +
  facet_grid( ~ Site)
