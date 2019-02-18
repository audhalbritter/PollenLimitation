#############################
#### IMPORT CLIMATE DATA ####
#############################

### LIBRARIES
library("gridExtra")

#### SOIL MOISTURE LOGGERS ####
soilmoisture <- read_excel(path = "Data/2017/Soilmoisture_2017.xlsx", sheet = 1, col_names = TRUE, col_types = c(rep("text", 2), "date", rep("numeric", 3), rep("text", 4)))

soilmoisture <- soilmoisture %>% 
  select(-average) %>% 
  mutate(Soilmoisture = (Soilmoisture_1 + Soilmoisture_2 + Soilmoisture_3)/3) %>% # calculate mean
  mutate(Block = gsub(" ", "_", Block))

SoilmoisturePlot <- soilmoisture %>% 
  ungroup(Site) %>% 
  mutate(Site = factor(Site, c("Gud", "Skj", "Ram", "Ves"))) %>% 
  mutate(Tlevel = plyr::mapvalues(Site, c("Gud", "Skj", "Ram", "Ves"), c("Alpine", "Alpine", "Subalpine", "Subalpine"))) %>% 
  mutate(Plevel = plyr::mapvalues(Site, c("Gud", "Skj", "Ram", "Ves"), c("early", "late", "early", "late"))) %>% 
  mutate(Site2 = paste(Tlevel, Plevel, sep = " - ")) %>% 
  ggplot(aes(x = Date, y = Soilmoisture, color = Tlevel, linetype = Plevel)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(x = "", y = "Soil moisture in %") +
  scale_colour_manual(name = "Temperature", values = c("lightblue", "orange")) +
  scale_linetype_manual(name = "Precipitation", values = c(2,1)) +
  facet_wrap(~ Site2) +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15), legend.position = "none")
#ggsave(SoilmoisturePlot, filename = "FinalFigures/SoilmoisturePlot.jpg", width = 6, height = 4)


#### READ IN iBUTTON ####
# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = "Data/2017/iButton", pattern = "csv", recursive = TRUE, full.names = TRUE)

dat <- read.csv("Data/2017/iButton/GUD_ran1.csv", header = TRUE, skip = 19, stringsAsFactors = FALSE)

#### Read in iButtons Function
ReadIniButtons <- function(textfile){
  print(textfile)
  
  dat <- read.csv(textfile, header = TRUE, skip = 19, stringsAsFactors = FALSE)
  
  dat <- dat %>%
    rename(Date = Date.Time) %>% 
    mutate(Value = paste(Value, X, sep = ".")) %>% 
    select(-Unit, -X) %>% 
    mutate(Date = dmy_hms(Date)) %>% 
    mutate(Value = as.numeric(Value))
  
  # Extract siteID, iButtonID and Year from file name
  dat$ID <- basename(textfile)
  dat <- dat %>%
    mutate(ID = basename(textfile))
  return(dat)
}

# Read in iButton data
mdat <- map_df(myfiles, ReadIniButtons)

iButton <- mdat %>% 
  mutate(Site = substr(ID, 1, 3)) %>% 
  mutate(Species = substr(ID, 5, 7)) %>% 
  mutate(Block = substr(ID, 5, 8))
# "RAM_leo3(2).csv" why 3 and 2???


#ggplot(iButton, aes(x = Date, y = Value, colour = Block)) +
  #geom_line() +
  #facet_grid(Species ~ Site)


#### CLLIMATE DATA SEEDCLIM ####

#### LOAD DAILY TEMPERATURE WITH IMPUTED VALUES ####
load(file = "DailyTemperature_Imputed.Rdata", verbose = TRUE)

dailyTemp <- DailyTemperature_Imputed %>% 
  filter(site %in% c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"),
         year(date) %in% c(2015, 2017),
         logger == "temp30cm") %>% 
  mutate(doy = yday(date)) %>% 
  mutate(site = factor(site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"))) %>%
  mutate(Tlevel = plyr::mapvalues(site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"), c("Alpine", "Alpine", "Subalpine", "Subalpine"))) %>% 
  mutate(Plevel = plyr::mapvalues(site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"), c("early", "late", "early", "late")))


# Cumulative temperature since snowmelt
DailyAndCumulativeTemp <- dailyTemp %>% 
  mutate(Year = year(date)) %>% 
  select(site, Year, doy, value, Tlevel, Plevel) %>% 
  rename(Site = site) %>% 
  mutate(Site = plyr::mapvalues(Site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"), c("GUD", "SKJ", "RAM", "VES"))) %>% 
  left_join(Snowmelt, by = c("Site", "Year")) %>% 
  # threshold 1°C else 0
  mutate(value = ifelse(value > 1, value, 0)) %>%
  # remove all data before SM
  mutate(value = ifelse(doy < SM, 0, value)) %>% 
  # calculate cumulative temperature
  group_by(Site, Plevel, Tlevel, Year) %>% 
  mutate(cumTemp = cumsum(value)) %>% 
  ungroup(Site, Plevel, Tlevel) %>% 
  mutate(dssm = doy - (SM - 1)) %>% 
  mutate(dssm = ifelse(dssm < 1, 0, dssm)) %>% 
  mutate(Site = factor(Site, c("GUD", "SKJ", "RAM", "VES"))) %>% 
  rename(dailyTemperature = value, cumulativeTemperature = cumTemp) %>% 
  gather(key = variable, value = value, - Site, - Year, - doy, -Tlevel, -Plevel, -SM, -dssm) %>% 
  mutate(variable = factor(variable, levels = c("dailyTemperature", "cumulativeTemperature")))


ClimatePlot <- DailyAndCumulativeTemp %>% 
  filter(doy > 120 & doy < 260) %>%
  ggplot(aes(x = doy, y = value, color = Tlevel, linetype = Plevel)) +
  geom_line(size = 0.6) +
  scale_colour_manual(name = "Temperature level", values = c("grey20", "grey80")) +
  scale_linetype_manual(name = "Snowmelt time", values = c(2,1)) +
  labs(x = "Day of the year", y = "Cumulative temperature above 1°C      Mean daily temperature in °C")  +
  facet_grid(variable ~ Year, scales = "free_y") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "top")
#ggsave(ClimatePlot, filename = "FinalFigures/ClimatePlot.jpg", width = 6, height = 4.5)

# Calculate summer temp in 2015 and 2017
DailyAndCumulativeTemp %>% 
  mutate(Month = month(as.Date(doy, origin = "2015-01-01"))) %>% 
  filter(variable == "dailyTemperature",
         Month %in% c(6,7,8,9)) %>% 
  group_by(Site, Year) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = Year, value = mean) %>% 
  mutate(Diff = `2017` - `2015`)

# gridded data for precipitation
load(file = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedDailyClimateData2009-2017.RData", verbose = TRUE)

dailyTemp %>% 
  filter(site %in% c("Gudmedalen", "Rambera", "Skjellingahaugen", "Veskre"),
         year(date) %in% c(2015, 2017),
         month(date) %in% c(6,7,8,9)) %>% 
  group_by(year(date)) %>% 
  summarise(mean = mean(value), se = sd(value)/sqrt(n())) %>% 
  unite(Value, mean, se, sep = "_") %>% 
  spread(key = `year(date)`, value = Value) %>%
  separate(col = `2015`, into = c("2015_mean", "2015_se"), sep = "_", convert = TRUE) %>% 
  separate(col = `2017`, into = c("2017_mean", "2017_se"), sep = "_", convert = TRUE) %>% 
  mutate(Diff = `2017_mean` - `2015_mean`,
         SE = sqrt(`2017_se`^2 + `2015_se`^2))


# Slopes of cumulative temp
res1 <- DailyAndCumulativeTemp %>%
  filter(variable == "cumulativeTemperature",
         value > 0) %>% 
  group_by(Year) %>% 
  do(fit = lm(value ~ doy + Site, data = .))
tidy(res1, fit)


dat15 <- DailyAndCumulativeTemp %>%
  filter(variable == "cumulativeTemperature",
         value > 0,
         Year == 2015)

fit1 = lm(value ~ doy * Site, data = dat15)
fit2 = lm(value ~ doy + Site, data = dat15)
anova(fit1, fit2)

