#### CALCULATE CUMSUM FOR EACH LOGGER ####

# Load daily temperature data
load("DailyTemperature.Rdata")

# Rearange temperature data
dailyT <- dailyTemperature %>% 
  mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))) %>% 
  mutate(site = plyr::mapvalues(site, from = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"), to = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedalen", "Arhelleren", "Vikesland", "Fauske"))) %>% 
  arrange(logger, site) %>% 
  mutate(doy = yday(date), year = year(date))


#### calcualte cumsum #### 
climateData <- dailyT %>% 
  filter(site %in% c("Skjellingahaugen", "Gudmedalen", "Veskre", "Rambera")) %>% 
  mutate(site = plyr::mapvalues(site, from = c("Skjellingahaugen", "Gudmedalen", "Veskre", "Rambera"), to = c("SKJ", "GUD", "VES", "RAM"))) %>% 
  filter(date > (ymd("2015.1.1")), date < (ymd("2015.12.31"))) %>%
  filter(logger == "temp30cm") %>% 
  mutate(temp = replace(value, is.na(value), 0)) %>%  # replace NA by zero
  mutate(temp = replace(value, value < 5, 0)) %>%
  group_by(site) %>% 
  mutate(cumTemp = cumsum(temp)) %>%
  ggplot(aes(x = date, y = cumTemp)) +
  geom_line() +
  facet_wrap(~site)

head(climateData)
