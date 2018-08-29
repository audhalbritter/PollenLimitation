source("Merging 2015 and 2017 data.R")

library("lme4")
library("broom")

Pollination17 <- Pollination %>% 
  filter(Year == 2017) %>% 
  # remove Second Flowers
  filter(!Pollination == "")

#### CORRELATION BETWEEN GROWTH AND PROBABILITY OF FLOWERING ####
# Is plant size and nr of flowers correlated
# Only control plants
# calculate Probability of flowering
ProbFlower <- Pollination17 %>% 
  filter(Pollination == "control", !is.na(NrFlowers), Variable %in% c("EndSize")) %>% # only control plants, remove if unknown if plant flowered, only one variable
  group_by(Species, Site, Treatment) %>%
  mutate(nPlant = n()) %>% 
  group_by(Species, Site, Treatment, Flowering) %>%
  mutate(n = n()) %>%
  mutate(ProbFl = n / nPlant) %>% 
  ungroup() %>% 
  # centre P and T-level
  mutate(DestPLevel.cen = scale(DestPLevel, scale = FALSE)) %>% 
  mutate(DestTLevel.cen = scale(DestTLevel, scale = FALSE)) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Control", "Warmer", "LaterSM", "WarmLate"), c("Control", "Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "Later SM", "Warm & late SM")))

SP <- c(LEO = "L. autumnalis", RAN = "R. acris")
ProbFlowerPlot <- ProbFlower %>% 
  mutate(Species = factor(Species, levels = c("RAN", "LEO"))) %>% 
  #mutate(Species = plyr::mapvalues(Species, c("LEO", "RAN"), c("L. autumnails", "R. acris"))) %>% 
  ggplot(aes(x = value, y = ProbFl, color = Treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  labs(x = "Longest leaf in cm", y = "Probability of flowering") +
  facet_grid(Species ~ Site, labeller=labeller(Species = SP)) + 
  theme(strip.text.y = element_text(face = "italic"))
ggsave(ProbFlowerPlot, filename = "FinalFigures/ProbFlowerPlot.jpg", height = 5, width = 8)

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
