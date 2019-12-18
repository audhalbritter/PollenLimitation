### DOY - PP

Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "doy") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Warmer"), pheno.stage == "Fruit") %>% 
  filter(orig != "RAM" | site != "RAM") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Warmer - Control)

Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "doy") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Wetter"), pheno.stage == "Fruit") %>% 
  filter(orig != "SKJ" | site != "SKJ") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Wetter - Control)

Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "doy") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "WarmWet"), pheno.stage == "Fruit") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = WarmWet - Control)



### DOGS - PP
Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Warmer"), pheno.stage == "Fruit") %>% 
  filter(orig != "RAM" | site != "RAM") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Warmer - Control)

Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Wetter"), pheno.stage == "Fruit") %>% 
  filter(orig != "SKJ" | site != "SKJ") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Wetter - Control)

Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "WarmWet"), pheno.stage == "Fruit") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = WarmWet - Control)


### DOGS - Adapt
Ranunculus %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gud
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Warmer"), pheno.stage == "Fruit") %>% 
  filter(orig != "SKJ" | site != "SKJ") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Warmer - Control)

Ranunculus %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gud
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Wetter"), pheno.stage == "Bud") %>% 
  filter(orig != "RAM" | site != "RAM") %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Wetter - Control)

### Difference alpine subalpine
Ranunculus %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gud
  filter(pheno.unit == "dogs") %>% # select unit: doy or dogs plot
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  filter(trt %in% c("Control", "Wetter"), pheno.stage == "Flower") %>% 
  filter(orig != "RAM" | site != "RAM") %>% 
  group_by(trt, site) %>% 
  summarise(mean = mean(mean)) %>% 
  spread(key = trt, value = mean) %>% 
  mutate(Diff = Wetter - Control)