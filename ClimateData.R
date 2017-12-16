#############################
#### IMPORT CLIMATE DATA ####
#############################

### LIBRARIES
library("lubridate")
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


#### CLLIMATE DATA SEEDCLIM ####
# Load daily temperature data
load(file = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Temperature.Rdata", verbose = TRUE)

# Calculate daily min, mean and max values for 2017
dailyTemp <- temperature %>% 
  # calculate daily values
  mutate(year = year(date), date = ymd(format(date, "%Y.%b.%d")), doy = yday(date)) %>%
  filter(year %in% c(2015, 2017)) %>% 
  group_by(logger, year, date, site) %>%
  summarise(n = n(), mean = mean(value, na.rm = TRUE), min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)) %>% 
  filter(n > 18)

  
# Filling gaps for daily mean, min and max values



fit200 <- lm(temp200cm ~ temp30cm + tempabove + tempsoil, data = lav)
fit30 <- lm(temp30cm ~ temp200cm + tempabove + tempsoil + site, data = MeanT)
fitabove <- lm(tempabove ~ tempsoil + site, data = MeanT)
fitsoil <- lm(tempsoil ~ temp200cm + temp30cm + tempabove + site, data = MeanT)
summary(fitabove)
plot(fit200)

dat <- lav

PredictMissingTempData <- function(dat){
  fit <- lm(tempabove ~ temp30cm + temp200cm, data = dat2)
  new.dat <- data.frame(temp200cm = dat$temp200cm)
  new.dat$pred <- predict(fit, new.dat)
  new.dat$pred
  res <- data_frame(date = dat$date,
                    pred = new.dat$pred)
  return(res)
}

# Run function to predict peak flower
dailyTemp %>% 
  select(-n, -min, -max) %>% 
  spread(key = logger, value = mean) %>% 
  group_by(site) %>%
  filter(!is.na(temp30cm)) %>% 
  do(PredictMissingTempData(.))




dailyTemp %>% 
  filter(year == 2017, site %in% c("Gud", "Skj", "Ves", "Ram"), logger == "temp30cm") %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() +
  facet_wrap(~ site)

temp30 %>% 
  filter(site == "Gud", year(date) ==  "2012") %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_smooth(method = "gam", formula = y ~ poly(x, 4)) +
  geom_line()


fit <- loess(mean ~ doy, data = temp30, span = 0.5, degree = 2)
fit2 <- lm(mean ~ doy + I(doy^2) + I(doy^3), data = temp30)
summary(fit)

new.dat <- data.frame(doy = seq(min(temp30$doy), max(temp30$doy), length.out = 365))
new.dat$pred <- predict(fit2, new.dat)
plot(temp30$doy, temp30$mean)
with(new.dat, lines(x = doy, y = pred, col = "red"))



#### CALCULATE CUMSUM FOR EACH LOGGER ####
# extracting 2015 and 2017 data from the 4 sites
climate <- dailyTemp %>% 
  filter(year(date) %in% c("2017"), site %in% c("Gud", "Ram", "Skj", "Ves")) %>% ## ADD 2015 again!!! 
  filter(logger == "tempabove") %>%
  mutate(site = plyr::mapvalues(site, c("Gud", "Ram", "Skj", "Ves"), c("GUD", "RAM", "SKJ", "VES"))) %>% 
  mutate(doy = yday(date)) %>% 
  left_join(Snowmelt, by = c("year" = "Year", "site" = "Site")) %>% 
  # replace mean, min and max with 0 until SM
  mutate(mean = ifelse(doy < SM, 0, mean)) %>% 
  mutate(min = ifelse(doy < SM, 0, min)) %>% 
  mutate(max = ifelse(doy < SM, 0, max)) %>% 
  select(-n) %>% 
  gather(key = measure, value = value, mean, min, max) %>% 
  # Calculate Cumsum for each site and measure (min, max, mean) 
  group_by(site, measure) %>% 
  mutate(CumTempAfterSM = cumsum(value)) %>% 
  spread(key = measure, value = CumTempAfterSM) %>% 
  rename(MaxTempSumAfterSM = max, MeanTempSumAfterSM = mean, MinTempSumAfterSM = min) %>% 
  select(year, site, doy, MaxTempSumAfterSM, MeanTempSumAfterSM, MinTempSumAfterSM)



