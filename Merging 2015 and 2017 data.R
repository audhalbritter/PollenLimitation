######################
#### IMPORT  DATA ####
######################

### 2015 and 2017 data
### LIBRARIES
library("lubridate")
library("cowplot")
library("readxl")
library("broom")
library("tidyverse")

pn <- . %>% print(n = Inf)

#### 2015 DATA ####

# Ranunculus
data2015Ran1 <- read.csv("Data/2015/data_pollenlimitaiton_Sept16.csv", sep=";", stringsAsFactors = FALSE)

data2015Ran <- data2015Ran1 %>% 
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
  mutate(Year = 2015,
         Pollination = substr(ID, 3,3)) %>% 
  mutate(Pollination = recode(Pollination, "C" = "control", "P" = "pollination"))

### Biomass, rep output etc. from Ran 2015 ###
biomass2015Ran <- read_excel(path = "Data/2015/data_pollenlimitaiton_Sept16.xlsx", sheet = "spring 2015")

repOutput2015Ran <- read_excel(path = "Data/2015/data_pollenlimitaiton_Sept16.xlsx", sheet = "Weight & Count Ranunculus")

biomass2015Ran <- biomass2015Ran %>% 
  filter(SP == "RAN") %>% 
  select(SP, Site, Origin, Treatment, ID, `size_start [cm]`, `size_end [cm]`, `#1 flower`) %>% 
  rename(Species = SP, InitSize = `size_start [cm]`, EndSize = `size_end [cm]`, NrFlowers = `#1 flower`) %>% 
  mutate(InitSize = as.numeric(InitSize),
         EndSize = as.numeric(EndSize),
         Growth = EndSize - InitSize) %>%
  mutate(ID =  gsub("_bis|_tris|_quad", "", ID)) %>%
  group_by(ID, Site, Origin, Treatment) %>% 
  mutate(NrFlowers2 = sum(NrFlowers, na.rm = TRUE)) %>% 
  group_by(ID, Site, Origin, Treatment) %>% 
  arrange(ID, Site, Origin, Treatment, NrFlowers) %>% 
  slice(1) %>%  # remove duplicates
  select(-NrFlowers) %>% 
  rename(NrFlowers = NrFlowers2, Pollination = Treatment) 
  


# Reproductive output RAN 2015
repOutput2015Ran <- repOutput2015Ran %>% 
  select(SP, Site, Origin, Treatment, ID, TOT_Weight) %>% 
  rename(RepOutput = TOT_Weight, Pollination = Treatment, Species = SP) %>% 
  filter(!grepl("_bis|_tris|_quad", ID)) # remove double flowers



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
  
data2015Ran <- data2015Ran %>% 
  left_join(Treatment) %>% 
  # Merge ran data, biomass and repoutput
  left_join(biomass2015Ran, by = c("Species", "Site", "Origin", "ID", "Pollination")) %>% 
  left_join(repOutput2015Ran, by = c("Species", "Site", "Origin", "ID", "Pollination")) %>% 
  mutate(Flowering = ifelse(NrFlowers > 0, 1, 0))

  

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
  mutate(Pollination = ifelse(Pollination == "GUD", "pollination", Pollination)) %>% 
  as_tibble()

Snowmelt <- data_frame(Site = rep(c("GUD", "RAM", "SKJ", "VES"), 2),
                       Year = as.numeric(c(rep("2015", 4), rep("2017", 4))),
                       SM = c(184, 167, 224, 174, 143, 137, 176, 135)) 
  

# MERGING 2015 AND 2017 DATA
Pollination1 <- data2015 %>% 
  bind_rows(data2017)
  
# add metadata
Pollination <- Treatment %>% 
  left_join(Pollination1, by = c("Site", "Origin")) %>% 
  left_join(Snowmelt, by = c(Site = "Site", "Year")) %>% 
  # if sessile bud missing, then take bud on stalk
  mutate(Date_bs = ifelse(is.na(Date_bs), Date_bp, Date_bs)) %>% 
  # calculate days between sm and bud, bud and flower, flower and seed
  select(-Date_bp) %>% 
  mutate(days_smb = Date_bs - SM, days_smf = Date_f - SM, days_sms = Date_s - SM, days_smrs = Date_rs - SM) %>% 
  select(-Date_bs, -Date_f, -Date_s, -Date_rs) %>% 
  bind_rows(data2015Ran) %>% 
  gather(key = Variable, value = value, days_smb, days_smf, days_sms, days_smrs, InitSize, EndSize, RepOutput, Growth) %>%
  filter(!is.na(value)) %>% 
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
