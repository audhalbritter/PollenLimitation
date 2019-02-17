Pollination %>% 
  mutate(InitSize2014 = ifelse(Year == 2017, initial_size, InitSize2014)) %>% 
  dplyr::select(Site, Origin, Treatment, Species, NewBlock, ID, Pollination, InitSize2014, Year, NrFlowers, Flowering, Variable, value) %>% 
  filter(Variable == "EndSize") %>% 
  mutate(survival = ifelse(is.na(value), 0, 1)) %>% 
  group_by(Year, Species, Site) %>%
  mutate(nPlant = n()) %>% 
  group_by(Year, Species, Site, survival) %>%
  mutate(n = n(),
         ProbSurvival = n / nPlant) %>% 
  group_by(Year, Species, Site) %>%
  summarise(MeanSurvival = mean(ProbSurvival)) %>% 
  #arrange(-MeanSurvival) %>% pn
  spread(key = Year, value = MeanSurvival)
  
