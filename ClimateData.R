#############################
#### IMPORT CLIMATE DATA ####
#############################

### LIBRARIES
library("lubridate")
library("ggplot2")
library("tidyverse")
library("cowplot")
library("readxl")

#### SOIL MOISTURE LOGGERS ####
soilmoisture <- read_excel(path = "Data/2017/Soilmoisture_2017.xlsx", sheet = 1, col_names = TRUE, col_types = c(rep("text", 2), "date", rep("numeric", 3), rep("text", 4)))

soilmoisture <- soilmoisture %>% 
  select(-average) %>% 
  mutate(Soilmoisture = (Soilmoisture_1 + Soilmoisture_2 + Soilmoisture_3)/3) %>% # calculate mean
  mutate(Block = gsub(" ", "_", Block))


ggplot(soilmoisture, aes(x = Date, y = Soilmoisture, color = Block)) +
  geom_point() +
  facet_wrap(~ Site)


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


ggplot(iButton, aes(x = Date, y = Value, colour = Block)) +
  geom_line() +
  facet_grid(Species ~ Site)


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

dailyT %>% 
  filter(logger == "temp30cm", date >= "2015-06-01" & date <= "2015-08-31") %>% 
  group_by(site) %>% 
  summarise(mean(value))
  
head(Ranunculus)
Ranunculus %>% filter(trt == "Wetter", pheno.stage == "Fruit")
  
