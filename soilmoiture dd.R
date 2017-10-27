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
myfiles <- dir(path = "Data/2017/iButton", pattern = "csv", recursive = TRUE, full.names = TRUE)

dat <- read.csv("Data/2017/iButton/GUD_ran1.csv", header = TRUE, skip = 19, stringsAsFactors = FALSE)
ReadIniButtons <- function(textfile){
  print(textfile)
  
  dat <- read.csv(textfile, header = TRUE, skip = 19, stringsAsFactors = FALSE)
  
  dat <- dat %>%
    rename(Date = Date.Time) %>% 
    mutate(Value = paste(Value, X, sep = ".")) %>% 
    select(-Unit, -X) %>% 
    mutate(Date = dmy_hms(Date)) %>% 
    mutate(Value = as.numeric(Value))
  
  dat$ID <- basename(textfile)
  dat <- dat %>%
    mutate(ID = basename(textfile))
  return(dat)
}
mdat <- map_df(myfiles, ReadIniButtons)

iButton <- mdat %>% 
  mutate(Site = substr(ID, 1, 3)) %>% 
  mutate(Species = substr(ID, 5, 7)) %>% 
  mutate(Block = substr(ID, 5, 8))
# "RAM_leo3(2).csv" why 3 and 2???


ggplot(iButton, aes(x = Date, y = Value, colour = Block)) +
  geom_line() +
  facet_grid(Species ~ Site)

load("DailyTemperature.Rdata")
dailyT <- dailyTemperature %>% 
  mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))) %>% 
  arrange(logger, site) %>% 
  mutate(doy = yday(date), year = year(date))
dailyT <- dailyTemperature
mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))) %>% 
  arrange(logger, site)
mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau")))
