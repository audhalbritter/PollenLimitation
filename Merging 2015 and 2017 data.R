######################
#### IMPORT  DATA ####
######################

### 2015 and 2017 data
### LIBRARIES
library("lubridate")
library("ggplot2")
library("tidyverse")
library("cowplot")
library("readxl")

pn <- . %>% print(n = Inf)

#### 2015 DATA

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

# meta data site, origin and treatment
Treatment <- data2015Ran %>% select(Site, Origin, Treatment) %>% distinct(Site, Origin, Treatment)

# Leontodon
data2015 <- read_excel(path = "~/Dropbox/Pollen limitation/Data/Pollen limitation 2015 DATA/DataSheet_2015_Nicola.xlsx", sheet = 1, col_names = FALSE, skip = 1)

colnames(data2015) <- c("Species", "Site", "Origin", "Pollination", "ID_old", "ID", "Block", "InitSize2014", "InitDate2014", "name", "weather", "InitDate", "InitSize", "Date_bs", "buds", "Date_bp", "bud_p", "Date_f", "flower", "Date_s", "seed", "Date_rs", "ripe_seed", "poll_1", "poll_2", "poll_3", "EndDate", "EndSize", "remark2")


data2015 <- data2015 %>% 
  filter(Species == "LEO") %>% # only Leontodon
  select(-buds, -bud_p, -flower, -seed, -ripe_seed) %>% 
  mutate(InitDate2014 = yday(ymd(InitDate2014)),
         poll_3 = as.numeric(poll_3),
         EndSize = as.numeric(EndSize),
         Year = 2015)
  
  
#### 2017 DATA
#data2017 <- read_excel(path = "Data/2017/INSERT_NAME_OF_DATA_FILE.xlsx", sheet = 1, col_names = TRUE, col_types = c(rep("text", 6), rep("numeric", 2), "date", rep("text", 3), "date", "numeric", rep("date", 9), "numeric", "text"))
data2017 <- read.csv(file = "Data/2017/phenology.csv", header = TRUE, stringsAsFactors = FALSE)
# replace N/A
data2017[data2017 == "N/A"] <- NA

data2017 <- data2017 %>% 
  rename(Pollination = Treatment, InitDate = date, InitSize = `size_start..cm.`, EndDate = Date2, EndSize = `size_end..cm.`, Biomass = Wt, Flower2 = X2nd.flower) %>%
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
         Growth = EndSize - InitSize,
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
  gather(key = pheno.stage, value = value, days_smb, days_smf, days_sms, days_smrs) %>%
  filter(!is.na(value)) %>% 
  
  # Make Code nice
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("days_smb", "days_smf", "days_sms", "days_smrs"), c("Bud", "Flower", "Seed", "Ripe Seed"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate")))
  
  # Cumulative Temperature after snowmelt
  #mutate(doy = ifelse(pheno.unit == "doy", value, ifelse(pheno.unit == "dogs", (value + sm), NA))) %>% # get doy for each observation
  #left_join(climateData, by = c("site" = "site", "doy" = "doy")) %>% 
  #mutate(CumTempAfterSM = ifelse(pheno.unit == "days", NA, CumTempAfterSM)) # does not make sense for durations



# ORIGIN - PHENOTYPIC PLASTICITY

# Snowmelt difference
SMDiff <- Pollination %>% 
  select(Origin, Treatment, Year, Species, SM) %>% 
  distinct(Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin) %>%
  mutate(Origin = plyr::mapvalues(Origin, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late")))


EventDiffData <- Pollination %>% 
  filter(Origin != "VES" | Treatment != "Control") %>% # remove Control at Veskre
  mutate(Origin = plyr::mapvalues(Origin, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>% 
  mutate(Origin = factor(Origin, levels = c("Alpine-early", "Alpine-late", "Subalpine-early"))) %>%
  group_by(Species, Year, Treatment, Site, Origin, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))


EventDiff <- EventDiffData %>% 
  ungroup(Site) %>% 
  select(-Site,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = Treatment, value = united) %>% 
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("Warmer_mean", "Warmer_se"), sep = "_", convert = TRUE) %>% 
  separate(col = LaterSM, into = c("LaterSM_mean", "LaterSM_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmLate, into = c("WarmLate_mean", "WarmLate_se"), sep = "_", convert = TRUE) %>% 
  # calculate difference between control and treatment in mean
  mutate(Warmer_mean = Warmer_mean - Control_mean, LaterSM_mean = LaterSM_mean - Control_mean, WarmLate_mean = WarmLate_mean - Control_mean) %>% 
  # calculate SE for difference
  mutate(Warmer_se = sqrt(Control_se^2 + Warmer_se^2), LaterSM_se = sqrt(Control_se^2 + LaterSM_se^2), WarmLate_se = sqrt(Control_se^2 + WarmLate_se^2)) %>% 
  select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -Species, -Year, -Origin, -pheno.stage) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  #mutate(mean = replace(mean, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  #mutate(se = replace(se, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  filter(!is.na(mean))


YearPlot <- EventDiff %>% 
  filter(Species == "RAN", pheno.stage == "Bud") %>% 
  left_join(SMDiff, by = c("Species", "Year", "Origin", "Treatment")) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = factor(Year), alpha = factor(Year), ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = Origin)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(16, 17)) +
  scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  #scale_fill_manual(name = "Year:", values = c("white", "red")) +
  labs(y = "", x = "", title = "") +
  geom_errorbar(width=0.18) +
  geom_point(size = 4) +
  #theme(legend.position="none") +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 13, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -25, label = "earlier", color = "grey20") +
  theme(legend.position = "none")
#+ facet_grid( ~ pheno.stage)
ggsave(YearPlot, filename = "YearPlot.pdf", height = 4.5, width = 4.5)




# DESTINATION - ADAPTATION

# Snowmelt difference
SMDiffAdapt <- Pollination %>% 
  select(Site, Origin, Treatment, Year, Species, SM) %>% 
  distinct(Site, Origin, Treatment, Year, Species, SM) %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  left_join(Snowmelt, by = c(Origin = "Site", "Year")) %>% 
  rename(SMOrigin = SM.y, SM = SM.x) %>% 
  mutate(SMDiff = SM - SMOrigin) %>%
  mutate(Site = plyr::mapvalues(Site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late")))



EventDiffAdaptData <- Pollination %>% 
  filter(Origin != "GUD" | Treatment != "Control") %>% # remove Control at Gudmedalen
  mutate(Site = plyr::mapvalues(Site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  mutate(Site = factor(Site, levels = c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  group_by(Species, Year, Treatment, Site, Origin, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))


EventDiffAdapt <- EventDiffAdaptData %>% 
  ungroup(Origin) %>% 
  select(-Origin,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = Treatment, value = united) %>% 
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("Warmer_mean", "Warmer_se"), sep = "_", convert = TRUE) %>% 
  separate(col = LaterSM, into = c("LaterSM_mean", "LaterSM_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmLate, into = c("WarmLate_mean", "WarmLate_se"), sep = "_", convert = TRUE) %>% 
  # calculate difference between control and treatment in mean
  mutate(Warmer_mean = Warmer_mean - Control_mean, LaterSM_mean = LaterSM_mean - Control_mean, WarmLate_mean = WarmLate_mean - Control_mean) %>% 
  # calculate SE for difference
  mutate(Warmer_se = sqrt(Control_se^2 + Warmer_se^2), LaterSM_se = sqrt(Control_se^2 + LaterSM_se^2), WarmLate_se = sqrt(Control_se^2 + WarmLate_se^2)) %>% 
  select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -Species, -Year, -Site, -pheno.stage) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  #mutate(mean = replace(mean, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  #mutate(se = replace(se, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  filter(!is.na(mean))



EventDiffAdapt %>% 
  filter(Species == "RAN", pheno.stage == "Bud") %>% 
  left_join(SMDiffAdapt, by = c("Species", "Year", "Site", "Treatment")) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = SMDiff, y = mean, colour = Treatment, shape = factor(Year), alpha = factor(Year), ymax = mean + 1.96*se, ymin = mean - 1.96*se)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(16, 17)) +
  scale_alpha_manual(name = "Treatment:", values = c(1, 0.3)) +
  #scale_fill_manual(name = "Year:", values = c("white", "red")) +
  labs(y = "", x = "", title = "") +
  geom_errorbar(width=0.18) +
  geom_point(size = 3) +
  #theme(legend.position="none") +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 13, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -25, label = "earlier", color = "grey20") +
  theme(legend.position = "none")
