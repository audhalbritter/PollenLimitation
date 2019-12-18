######################
#### IMPORT  DATA ####
######################

### 2015 and 2017 data
### LIBRARIES
library("lubridate")
library("tidyverse")
library("cowplot")
library("readxl")

pn <- . %>% print(n = Inf)

#### 2015 DATA ####

# Ranunculus
data2015Ran <- read.csv("Data/2015/data_pollenlimitaiton_Sept16.csv", sep=";", stringsAsFactors = FALSE)

data2015Ran <- data2015Ran %>% 
  as_tibble() %>% 
  rename(Species = sp, Site = site, Origin = orig, Treatment = trt, SM = sm) %>% 
  select(-ind, -TD, -PD, -TO, -PO, - s.ter, -s.ter.1, -s.ter.2, -s.ter.3, -s.ter.4) %>% 
  mutate(Block = c(rep(1, 10), rep(2, 10), rep(3, 10), rep(c(1,1,1,2,2), 5), rep(c(1,1,2,2,2), 5), rep(1, 35), rep(2, 35), rep(1, 60), rep(2, 59))) %>% 
  select(-doy.bs, -doy.bp, -doy.f, -doy.s, -doy.rs, -dogs.bs) %>% 
  rename(days_smb = dogs.bp, days_smf = dogs.f, days_sms = dogs.s, days_smrs = dogs.rs) %>% 
  #mutate(days.smb = doy.bp - sm, days.smf = doy.f - doy.bp, days.sms = doy.s - doy.f) %>% 
  #gather(key = variable, value = value, -Species, -Site, -Origin, -ID, -Treatment, -sm, -Block) %>%
  #filter(!is.na(value)) %>% 
  #separate(variable, into = c("pheno.unit", "pheno.stage"), sep = "\\.") %>%
  # Make Code nice
  #mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("bs", "bp", "f",  "s", "rs"), c("Bud", "Bud2", "Flower", "Fruit", "RipeFruit"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("c", "wa", "we", "ww"), c("Control", "Warmer", "LaterSM", "WarmLate"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(Year = 2015)


### META DATA
# T and P levels
TPLevels <- data_frame(Site = c("GUD", "RAM", "SKJ", "VES"),
                       TempLevel = c(6.5, 8.5, 6.5, 8.5),
                       PrecLevel = c(2000, 2000, 2700, 2700))

# meta data site, origin and treatment
Treatment <- data2015Ran %>% 
  select(Site, Origin, Treatment) %>% 
  distinct(Site, Origin, Treatment) %>% 
  # destination  
  left_join(TPLevels, by = "Site") %>% 
  rename(DestTLevel = TempLevel, DestPLevel = PrecLevel) %>% 
  # origin  
  left_join(TPLevels, by = c("Origin" = "Site")) %>% 
  rename(OrigTLevel = TempLevel, OrigPLevel = PrecLevel)



# Leontodon
data2015 <- read_excel(path = "Data/2015/DataSheet_2015_Nicola.xlsx", col_names = TRUE, sheet = 1)

colnames(data2015) <- c("Species", "Site", "Origin", "Pollination", "ID_old", "ID", "Block", "InitSize2014", "InitDate2014", "name", "weather", "remark", "InitDate", "InitSize", "Date_bs", "buds", "Date_bp", "bud_p", "Date_f", "flower", "Date_s", "seed", "Date_rs", "ripe_seed", "poll_1", "poll_2", "poll_3", "EndDate", "EndSize", "remark2")


data2015 <- data2015 %>% 
  filter(Species == "LEO") %>% # only Leontodon
  select(-buds, -bud_p, -flower, -seed, -ripe_seed) %>% 
  mutate(InitDate2014 = yday(ymd(InitDate2014)),
         InitSize = as.numeric(InitSize),
         poll_1 = as.numeric(poll_1),
         poll_2 = as.numeric(poll_2),
         poll_3 = as.numeric(poll_3),
         EndSize = as.numeric(EndSize),
         Year = 2015)



#### 2017 DATA
data2017 <- read.csv(file = "Data/2017/17-11-18_phenology.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

data2017 <- data2017 %>% 
  rename(Species = SP, Pollination = Treatment, InitDate = Date, InitSize = size_start..cm., EndDate = Date2 , EndSize = size_end..cm., RepOutput = Wt, NrFlowers = flowers, RepOutput2 = X2nd.flower) %>%
  mutate(InitDate = yday(dmy(InitDate))) %>%
  mutate(EndDate = yday(dmy(EndDate)),
         Date_bs = yday(dmy(Date_bs)),
         Date_bp = yday(dmy(Date_bp)),
         Date_f = yday(dmy(Date_f)),
         Date_s = yday(dmy(Date_s)),
         Date_rs = yday(dmy(Date_rs)),
         poll_1 = yday(dmy(poll_1)),
         poll_2 = yday(dmy(poll_2)),
         poll_3 = yday(dmy(poll_3)),
         InitSize = as.numeric(InitSize),
         EndSize = as.numeric(EndSize),
         NrFlowers = as.numeric(NrFlowers),
         RepOutput = as.numeric(RepOutput),
         RepOutput2 = as.numeric(RepOutput2),
         Growth = EndSize - InitSize,
         Flowering = ifelse(NrFlowers > 0, 1, 0),
         Year = 2017) %>% 
  select(-growth) %>% 
  as_tibble()

Snowmelt <- data_frame(Site = rep(c("GUD", "RAM", "SKJ", "VES"), 2),
                       Year = as.numeric(c(rep("2015", 4), rep("2017", 4))),
                       SM = c(184, 167, 224, 174, 143, 137, 176, 135)) 


# MERGING 2015 AND 2017 DATA
Pollination <- data2015 %>% 
  bind_rows(data2017)

# add metadata
Pollination <- Treatment %>% 
  left_join(Pollination, by = c("Site", "Origin")) %>% 
  left_join(Snowmelt, by = c(Site = "Site", "Year")) %>% 
  # if sessile bud missing, then take bud on stalk
  mutate(Date_bs = ifelse(is.na(Date_bs), Date_bp, Date_bs)) %>% 
  # calculate days between sm and bud, bud and flower, flower and seed
  select(-Date_bp) %>% 
  mutate(days_smb = Date_bs - SM, days_smf = Date_f - SM, days_sms = Date_s - SM, days_smrs = Date_rs - SM) %>% 
  select(-Date_bs, -Date_f, -Date_s, -Date_rs) %>% 
  bind_rows(data2015Ran) %>% 
  gather(key = Variable, value = value, days_smb, days_smf, days_sms, days_smrs, InitSize, EndSize, RepOutput, Growth) %>%
  mutate(Flowering = factor(Flowering)) %>% 
  
  # Make Code nice
  mutate(Variable = plyr::mapvalues(Variable, c("days_smb", "days_smf", "days_sms", "days_smrs", "InitSize", "EndSize", "RepOutput", "Growth"), c("Bud", "Flower", "Seed", "Ripe Seed", "InitSize", "EndSize", "RepOutput", "Growth"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate")))


# Add NewBlock
# RAN: GUD 1-3; RAM: 4-5; SKJ: 6-7; VES: 8-9: GUD: change ind in blocks 1-3!
# LEO: GUD 1-4; RAM: 5-8; SKJ: 9-12; VES: 13-18

MetaNewBlock <- data_frame(Species = c(rep("LEO", 18), rep("RAN", 9)),
                           Site = c(rep("GUD", 4), rep("RAM", 4), rep("SKJ", 4), rep("VES", 6), rep("GUD", 3), rep("RAM", 2), rep("SKJ", 2), rep("VES", 2)),
                           Block = c(rep(1:4, 3), 1:6, 1:3, rep(1:2, 3)),
                           NewBlock = c(1:18, 1:9))

MetaNewBlockGUD <- data_frame(Species = rep("RAN", 30),
                              Site = rep("GUD", 30),
                              Block = c(rep(1, 10), rep(2, 10), rep(3, 10)),
                              ID = c("G_C_1", "G_C_4", "G_P_1", "G_P_4", "G_P_2", "G_C_5", "G_C_3", "G_P_5", "G_C_2", "G_P_3",
                                     "G_C_3", "G_P_4", "G_P_3", "G_P_1", "G_P_2", "G_C_1", "G_C_4", "G_C_2", "G_P_5", "G_C_5",
                                     "G_C_4", "G_C_3", "G_P_2", "G_P_4", "G_C_5", "G_P_3", "G_C_2", "G_C_1", "G_P_1", "G_P_5"),
                              NewBlockGUD = c(rep(1, 4), rep(2, 3), rep(3, 3), rep(1, 3), rep(2, 4), rep(3, 3), rep(1, 3), rep(2, 3), rep(3, 4)))

Pollination <- Pollination %>% 
  left_join(MetaNewBlock, by = c("Species", "Site", "Block")) %>% 
  left_join(MetaNewBlockGUD, by = c("Species", "Site", "Block", "ID")) %>% 
  mutate(NewBlock = ifelse(Species == "RAN" & Site == "GUD", NewBlockGUD, NewBlock)) %>% 
  select(-NewBlockGUD)

# 2015 data: get Flower no-flower, biomass, rep output, nr flower
# FIX FLOWERING, ALL PLANTS WITH BIOMASS, SHOULD HAVE FLOWERING 0 AND REP OUTPUT SHOULD NOT BE NA
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
