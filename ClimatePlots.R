load("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Temperature.RData")
head(temperature)

climate <- temperature %>% 
  filter(year(date) == 2015, site %in% c("Gud", "Ram", "Skj", "Ves"), logger == "temp30cm") %>% 
  mutate(site = factor(site, c("Gud", "Skj", "Ram", "Ves"))) %>% 
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, site) %>%
  summarise(n = n(), sum = sum(value), value = mean(value)) %>%
  select(-n, -sum) %>% 
  mutate(doy = yday(date))
  
climate %>% filter(doy > 160 & doy < 356) %>% 
  ggplot(aes(x = doy, y = value)) + geom_line() + facet_wrap(~ site) +
  geom_hline(yintercept=2.5, color = "grey")

# Find end of growing season
climate %>% filter(site == "Skj") %>% print(n = 355)


CumulativeTemp <- climate %>% 
  # threshold 1°C
  mutate(value = ifelse(value > 1, value, 0)) %>% 
  spread(key = site, value = value) %>% 
  mutate(Ram = ifelse(doy < 167, 0, Ram)) %>% 
  mutate(Gud = ifelse(doy < 184, 0, Gud)) %>% 
  mutate(Ves = ifelse(doy < 174, 0, Ves)) %>% 
  mutate(Skj = ifelse(doy < 224, 0, Skj)) %>% 
  gather(key = site, value = value, -date, -doy) %>% 
  group_by(site) %>% 
  mutate(cumTemp = cumsum(value)) %>% 
  ungroup(site) %>% 
  mutate(site = factor(site, c("Gud", "Skj", "Ram", "Ves"))) %>% 
  mutate(Tlevel = plyr::mapvalues(site, c("Gud", "Skj", "Ram", "Ves"), c("Alpine", "Alpine", "Subalpine", "Subalpine"))) %>% 
  mutate(Plevel = plyr::mapvalues(site, c("Gud", "Skj", "Ram", "Ves"), c("dry", "wet", "dry", "wet")))

# Mean temperature in the first 7 days
CumulativeTemp %>%
  filter(value > 0) %>% 
  group_by(site) %>%
  slice(c(1:7)) %>% 
  summarise(mean(value))

library(cowplot)
CumTempPlot <- CumulativeTemp %>% filter(doy > 160 & doy < 273) %>%
  ggplot(aes(x = date, y = cumTemp, color = Tlevel, linetype = Plevel)) +
  geom_line() +
  scale_colour_manual(name = "Temperature", values = c("blue", "red")) +
  scale_linetype_manual(name = "Precipitation", values = c(2,1)) +
  labs(x = "", y = "Cum. temperature")  +
  theme(text = element_text(size = 9), axis.text = element_text(size = 9))


TempPlot <- CumulativeTemp %>% filter(doy > 160 & doy < 273) %>% 
  ggplot(aes(x = date, y = value, color = Tlevel, linetype = Plevel)) +
  geom_line() +
  scale_colour_manual(name = "Temperature", values = c("blue", "red")) +
  scale_linetype_manual(name = "Precipitation", values = c(2,1)) +
  labs(x = "", y = "Mean daily\n temperature in °C") +
  theme(text = element_text(size = 9), axis.text = element_text(size = 9))


ClimatePlot <- plot_grid(TempPlot, CumTempPlot, labels = c("a)", "b)"), nrow = 2, align = "v")
save_plot("ClimatePlot.jpeg", ClimatePlot, base_aspect_ratio = 1)
