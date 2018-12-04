#############################
#### IMPORT CLIMATE DATA ####
#############################

### LIBRARIES
library("lubridate")
library("tidyverse")
library("cowplot")
library("readxl")
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
ggsave(SoilmoisturePlot, filename = "FinalFigures/SoilmoisturePlot.jpg", width = 6, height = 4)


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
# Load daily temperature data
load(file = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Temperature.Rdata", verbose = TRUE)

# Calculate daily min, mean and max values for 2017
dailyTemp <- temperature2 %>% 
  # calculate daily values
  mutate(year = year(date), date = ymd(format(date, "%Y.%b.%d")), doy = yday(date)) %>%
  filter(year == 2017, site %in% c("Gudmedalen", "Rambera", "Skjellingahaugen", "Veskre")) %>% 
  group_by(logger, date, site) %>%
  summarise(n = n(), mean = mean(value, na.rm = TRUE), min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)) %>% 
  filter(logger == "tempabove") %>% 
  mutate(doy = yday(date)) %>% 
  ungroup(site) %>% 
  mutate(site = factor(site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"))) %>% 
  mutate(Tlevel = plyr::mapvalues(site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"), c("Alpine", "Alpine", "Subalpine", "Subalpine"))) %>% 
  mutate(Plevel = plyr::mapvalues(site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"), c("early", "late", "early", "late")))

# Temperature plot
TemperaturePlot <- dailyTemp %>% 
  filter(doy > 120 & doy < 260) %>% 
  ggplot(aes(x = date, y = mean, color = Tlevel, linetype = Plevel)) +
  geom_line() +
  scale_colour_manual(name = "Temperature level", values = c("lightblue", "orange")) +
  scale_linetype_manual(name = "Snowmelt time", values = c(2,1)) +
  labs(x = "Month", y = "Mean daily\n temperature in °C") +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15), legend.position = "none")
#ggsave(TemperaturePlot, filename = "FinalFigures/TemperaturePlot.jpg", width = 6, height = 4)


# Cumulative temperature since snowmelt
CumulativeTemp <- dailyTemp %>% 
  mutate(Year = year(date)) %>% 
  dplyr::select(site, Year, doy, mean, Tlevel, Plevel) %>% 
  rename(Site = site) %>% 
  mutate(Site = plyr::mapvalues(Site, c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"), c("GUD", "SKJ", "RAM", "VES"))) %>% 
  left_join(Snowmelt, by = c("Site", "Year")) %>% 
  # threshold 1°C else 0
  mutate(mean = ifelse(mean > 1, mean, 0)) %>%
  # remove all data before SM
  mutate(mean = ifelse(doy < SM, 0, mean)) %>% 
  # calculate cumulative temperature
  group_by(Site, Plevel, Tlevel) %>% 
  mutate(cumTemp = cumsum(mean)) %>% 
  ungroup(Site, Plevel, Tlevel) %>% 
  mutate(dssm = doy - (SM - 1)) %>% 
  mutate(dssm = ifelse(dssm < 1, 0, dssm)) %>% 
  mutate(Site = factor(Site, c("GUD", "SKJ", "RAM", "VES"))) %>% 
  select(-mean, -SM)

#mutate(Tlevel = plyr::mapvalues(site, c("Gud", "Skj", "Ram", "Ves"), c("Alpine", "Alpine", "Subalpine", "Subalpine"))) %>% 
#mutate(Plevel = plyr::mapvalues(site, c("Gud", "Skj", "Ram", "Ves"), c("dry", "wet", "dry", "wet"))) %>%


CumTempPlot <- CumulativeTemp %>% 
  filter(doy > 120 & doy < 260) %>%
  ggplot(aes(x = doy, y = cumTemp, color = Tlevel, linetype = Plevel)) +
  geom_line() +
  scale_colour_manual(name = "Temperature level", values = c("lightblue", "orange")) +
  scale_linetype_manual(name = "Snowmelt time", values = c(2,1)) +
  labs(x = "Day of the year", y = "Cumulative temperature \n above 1°C")  +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15), legend.position = "none")
#ggsave(CumTempPlot, filename = "FinalFigures/CumTempPlot.jpg", width = 6, height = 4)

# get legend
p <- CumulativeTemp %>% 
  filter(doy > 120 & doy < 260) %>%
  ggplot(aes(x = doy, y = cumTemp, color = Tlevel, linetype = Plevel)) +
  geom_line() +
  scale_colour_manual(name = "Temperature level", values = c("lightblue", "orange")) +
  scale_linetype_manual(name = "Snowmelt time", values = c(2,1)) +
  labs(x = "Day of the year", y = "Cumulative temperature \n in GDD above 1°C")  +
  theme(text = element_text(size = 12), axis.text = element_text(size = 12), legend.position = "top")
LegendTop <- get_legend(p)

Plot1 <- grid.arrange(TemperaturePlot, CumTempPlot, 
                        layout_matrix = cbind(c(1), c(2)))

Plot2 <- grid.arrange(LegendTop, Plot1, layout_matrix = rbind(c(1), c(2), c(2), c(2), c(2), c(2)))
ggsave(Plot2, filename = "FinalFigures/TempPlot.jpg", width = 6, height = 4)
