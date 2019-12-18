Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  ggplot(aes( x = site, y = value, color = trt)) +
  geom_jitter() +
  facet_grid(~ orig)



Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  filter(trt == "Control") %>%
  group_by(site) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(n), meanCT = mean(CumTempAfterSM, na.rm = TRUE), seCT = sd(value, na.rm = TRUE)/sqrt(n))


EventDiff %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower")

EventDiffAdapt %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower")


Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  filter(trt %in% c("Control", "Warmer")) %>%
  filter(site %in% c("SKJ", "VES")) %>%
  filter(!is.na(value)) %>% 
  ggplot(aes(x = trt, y = value)) + 
  geom_jitter() +
  stat_summary()
  

# Difference Treatment - Control
size %>% 
  #filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(trt, site, orig) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = sd(size_end, na.rm = TRUE)/sqrt(N)) %>% 
  ungroup() %>% 
  unite(mean_se, mean, se, sep = "_") %>%
  select(-N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = mean_se) %>% # spread Treatments
  separate(col = Control, into = c("C_mean", "C_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("W_mean", "W_se"), sep = "_", convert = TRUE) %>% 
  separate(col = LaterSM, into = c("L_mean", "L_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmLate, into = c("WW_mean", "WW_se"), sep = "_", convert = TRUE)
  #mutate(Warmer.prop = Warmer * 100 / Control, LaterSM.prop = LaterSM * 100 / Control, WarmLate.prop = WarmLate * 100 / Control) 
  



# CV
Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  filter(trt %in% c("Control")) %>%
  group_by(orig, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  summarise(mean = mean(mean), sd = sd(mean)) %>% 
  mutate(CV = sd / mean)
  

Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Bud") %>% 
  filter(site %in% c("VES")) %>%
  group_by(orig) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  summarise(mean = mean(mean), sd = sd(mean)) %>% 
  mutate(CV = sd / mean)

  
  
  