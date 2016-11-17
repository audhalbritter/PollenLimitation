#### CALCULATE CUMSUM FOR EACH LOGGER ####

# Load daily temperature data
load("DailyTemperature.Rdata")

# Rearange temperature data
dailyT <- dailyTemperature %>% 
  mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))) %>% 
  arrange(logger, site) %>% 
  mutate(doy = yday(date), year = year(date))


#### calcualte cumsum #### 
climateData <- dailyT %>% 
  filter(site %in% c("Skj", "Gud", "Ves", "Ram")) %>% 
  mutate(site = plyr::mapvalues(site, from = c("Skj", "Gud", "Ves", "Ram"), to = c("SKJ", "GUD", "VES", "RAM"))) %>% 
  filter(date > (ymd("2015.1.1")), date < (ymd("2015.12.31"))) %>%
  filter(logger == "temp30cm") %>% 
  mutate(temp = replace(value, is.na(value), 0)) %>%  # replace NA by zero
  mutate(temp = replace(value, value < 5, 0)) %>%
  # include snowmelt date
  mutate(TempAfterSM = ifelse(site == "RAM", replace(temp, doy < 167, 0),
         ifelse(site == "GUD", replace(temp, doy < 184, 0),
                ifelse(site == "SKJ", replace(temp, doy < 224, 0),
                       ifelse(site == "VES", replace(temp, doy < 174, 0), temp))))) %>% 
  group_by(site) %>% 
  mutate(CumTempAfterSM = cumsum(TempAfterSM)) %>%
  select(site, doy, CumTempAfterSM)
  
  
  
  ggplot(aes(x = date, y = cumTemp)) +
  geom_line() +
  facet_wrap(~site)

head(climateData)
