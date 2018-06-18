
library(readxl)
fitness <- read_excel("Data/2015/data_pollenlimitaiton_Sept16.xlsx", sheet = 6, col_names = TRUE)
head(fitness)

meta <- Ranunculus %>% distinct(orig, site, trt)

fitness %>% 
  filter(SP == "RAN", Treatment == "control") %>%
  left_join(meta, by = c("Site" = "site", "Origin" = "orig")) %>% 
  group_by(Origin, trt) %>% 
  summarise(n = n(), mean = mean(TOT_Weight)) %>% 
  filter(Origin != "VES" | trt != "Control") %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Warmer = Warmer - Control, LaterSM = LaterSM - Control, WarmLate = WarmLate - Control) %>%
  select(-Control) %>% 
  gather(key = Treatment, value = mean, -Origin) %>% 
  filter(!is.na(mean)) %>% 
  mutate(orig = plyr::mapvalues(Origin, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>%
  left_join(SMDiff, by = c("orig", "Treatment")) %>%
  ggplot(aes(x = smDiff, y = mean, color = Treatment)) +
  geom_point() +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple"))



### Proportion seed production
Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage != "Flower") %>% 
  filter(orig != "VES" | trt != "Control") %>% 
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>%
  #filter(orig != "Subalpine-early") %>% 
  spread(key = pheno.stage, value = value) %>% 
  mutate(fruit.production = ifelse(is.na(Fruit), 0, 1)) %>% 
  mutate(Treatment = factor(trt, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  group_by(Treatment, orig, fruit.production) %>% 
  summarise(N = n()) %>% 
  spread(key = fruit.production, value = N) %>% 
  mutate(total = `0` + `1`, prop.fruit = `1` * 100 / total) %>% 
  left_join(SMDiff, by = c("orig", "Treatment")) %>%
  ggplot(aes(x = smDiff, y = prop.fruit, color = Treatment)) +
  geom_jitter(height = 0.2) +
  labs(x = "Advanced SMT delayed", y = "Proportion fruit") +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  facet_wrap(~ orig)

Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage != "Flower") %>% 
  filter(orig != "GUD" | trt != "Control") %>% 
  mutate(site = plyr::mapvalues(site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>% 
  #filter(trt != "LaterSM", site != "Alpine-late") %>% 
  spread(key = pheno.stage, value = value) %>% 
  mutate(fruit.production = ifelse(is.na(Fruit), 0, 1)) %>% 
  mutate(Treatment = factor(trt, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  group_by(Treatment, site, fruit.production) %>% 
  summarise(N = n()) %>% 
  spread(key = fruit.production, value = N) %>% 
  mutate(total = `0` + `1`, prop.fruit = `1` * 100 / total) %>% 
  left_join(SMDiffAdapt, by = c("site", "Treatment")) %>%
  ggplot(aes(x = smDiff, y = prop.fruit, color = Treatment)) +
  geom_jitter(height = 0.2) +
  labs(x = "Advanced SMT delayed", y = "Proportion of fruits") +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  facet_wrap(~ site)
  

# Cumulative temp to Flower
Ranunculus %>% 
  filter(pheno.stage == "Flower") %>% 
  ggplot(aes(x = site, y = CumTempAfterSM, color = trt)) +
  geom_jitter() +
  facet_grid(~ orig)

survival <- read_excel("data_pollenlimitaiton_Sept16.xlsx", sheet = "spring 2015", col_names = TRUE)
head(survival)


survival %>% 
  filter(SP == "RAN") %>% 
  left_join(meta, by = c("Site" = "site", "Origin" = "orig")) %>%
  mutate(summer.survival = ifelse(is.na(`size_end [cm]`), 0, 1)) %>% 
  select(Site, Origin, trt, `size_end [cm]`, summer.survival) %>% 
  ggplot(aes(x = Origin, y = summer.survival, color = trt)) +
  geom_jitter(height = 0.2) +
  geom_hline(yintercept = 0.5, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Site)
  




### Plastic
Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  group_by(orig, trt) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(DiffWarm = Warmer - Control) %>% 
  mutate(PropWarm = DiffWarm * 100 /Control) %>% 
  mutate(DiffLateSM = LaterSM - Control) %>% 
  mutate(PropLateSM = DiffLateSM * 100 /Control) %>% 
  mutate(DiffWarmLate = WarmLate - Control) %>% 
  mutate(PropWarmLate = DiffWarmLate * 100 /Control)



### Genetic
Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>% 
  group_by(site, trt) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  spread(key = trt, value = mean) %>%
  mutate(DiffWarm = Warmer - Control) %>% 
  mutate(PropWarm = DiffWarm * 100 /Control) %>% 
  mutate(DiffLateSM = LaterSM - Control) %>% 
  mutate(PropLateSM = DiffLateSM * 100 /Control) %>% 
  mutate(DiffWarmLate = WarmLate - Control) %>% 
  mutate(PropWarmLate = DiffWarmLate * 100 /Control)





