# plant size data
size <- read.table("size.csv", header = TRUE, sep = ";")
head(size)
size <- size %>% 
  filter(sp == "RAN") %>% 
  select(-Torig, -Porig, -Tdest, -Pdest, -DOY, -blk) %>% 
  mutate(trt = plyr::mapvalues(trt, c("c", "wa", "we", "ww"), c("Control", "Warmer", "LaterSM", "WarmLate")))

size %>% 
  ggplot() +
  ggtitle("First flowering") +
  geom_boxplot(aes(x= orig, y = size_end, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))


SizeMeanSE <- size %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(trt, site, orig) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = sd(size_end, na.rm = TRUE)/sqrt(N))

# only for checking results
MeanSE %>% filter(trt %in% c("Control", "Warmer")) %>% 
  ggplot(aes(x = trt, y = mean, color = orig, group = orig)) +
  geom_point() +
  geom_line()

SizeMeanSE %>% 
  ggplot(aes(x = site, y = mean, color = trt)) + 
  geom_point() +
  facet_grid(~ orig)

# Calculate difference between Control and Treatment for SE
SizeSEData <- SizeMeanSE %>% 
  ungroup() %>% 
  select(-mean, -N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = se) %>% # spread Treatments
  mutate(Warmer = sqrt(Control^2 + Warmer^2), LaterSM = sqrt(Control^2 + LaterSM^2), WarmLate = sqrt(Control^2 + WarmLate^2)) %>%
  select(-Control) %>% 
  gather(key = Treatment, value = SE, -orig) %>% # gather Treatments
  filter(!is.na(SE)) %>% # remove e.g. Warmer in RAM, no such treatment
  select(SE, orig, Treatment)
  

# Calculate difference between Control and Treatment for Mean
SizeMeanData <- SizeMeanSE %>% 
  ungroup() %>% 
  select(-se, -N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = mean) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, LaterSM = LaterSM - Control, WarmLate = WarmLate - Control) %>% # Difference Treatment - Control
  #mutate(Warmer.prop = Warmer * 100 / Control, LaterSM.prop = LaterSM * 100 / Control, WarmLate.prop = WarmLate * 100 / Control) %>% # Difference Treatment - Control
  select(-Control) %>%
  gather(key = Treatment, value = Effect, -orig) %>% # gather Treatments
  filter(!is.na(Effect)) %>% # remove e.g. Warmer in RAM, no such treatment
  left_join(SizeSEData, by = c("orig" = "orig", "Treatment" = "Treatment")) %>% 
  left_join(SizeMeanSE, by = c("orig" = "orig", "Treatment" = "trt")) %>% 
  select(-mean, -se) %>% 
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>%
  mutate(pheno.stage = "Leaf length") %>%
  mutate(pheno.unit = "cm") %>%
  rename(mean = Effect, se = SE) %>%
  mutate(newname = NA) %>% 
  left_join(SMDiff, by = c("orig", "Treatment", "site")) %>% 
  select(orig, pheno.stage, pheno.unit, Treatment, mean, se, newname, N, site, sm, smOrig, smDiff)
  


LeafSizePlot <- SizeMeanData %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "LaterSM", "WarmLate"))) %>% 
  ggplot(aes(x = smDiff, y = mean, color = Treatment, shape = Treatment, ymax = mean + se, ymin = mean - se)) +
  geom_hline(yintercept=0, color = "gray", linetype = "dashed") +
  geom_point(size = 3) +
  labs(y = "Difference in leaf size [cm] between\n treatment and origin-control", x = "Difference in SMT between\n origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  scale_colour_manual(name = "Treatment", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(17,16,15)) +
  ylim(-2.5, 3) +
  geom_errorbar(width=0.2) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 32, y = 3, label = "larger", color = "grey20") +
  annotate(geom = "text", x = 32, y = -2.5, label = "smaller", color = "grey20") +
  theme(legend.position="none")




########################################################################
#### ADAPTATION #####
########################################################################

MeanSEAdapt <- size %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  group_by(trt, site, orig) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = 2*sd(size_end, na.rm = TRUE)/sqrt(N))

# Calculate difference between Control and Treatment for SE
SEAdaptData <- MeanSEAdapt %>% 
  ungroup() %>% 
  select(-mean, -N, -orig) %>% # remove origin, because it causes problems
  spread(key = trt, value = se) %>% # spread Treatments
  mutate(Warmer = sqrt(Control^2 + Warmer^2), LaterSM = sqrt(Control^2 + LaterSM^2), WarmLate = sqrt(Control^2 + WarmLate^2)) %>%
  select(-Control) %>% 
  gather(key = Treatment, value = SE, -site) %>% # gather Treatments
  filter(!is.na(SE)) %>% # remove e.g. Warmer in RAM, no such treatment
  select(SE, site, Treatment)

# Calculate difference between Control and Treatment for Mean
MeanDataAdapt <- MeanSEAdapt %>% 
  ungroup() %>% 
  select(-se, -N, -orig) %>% # remove orig, because it causes problems
  spread(key = trt, value = mean) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, LaterSM = LaterSM - Control, WarmLate = WarmLate - Control) %>% # Difference Treatment - Control
  select(-Control) %>% 
  gather(key = Treatment, value = Effect, -site) %>% # gather Treatments
  filter(!is.na(Effect)) %>% # remove e.g. Warmer in RAM, no such treatment
  left_join(SEAdaptData, by = c("site" = "site", "Treatment" = "Treatment")) %>%  # join SE
  left_join(MeanSEAdapt, by = c("site" = "site", "Treatment" = "trt")) %>% 
  select(-mean, -se) %>% 
  mutate(site = plyr::mapvalues(site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>% 
  mutate(pheno.stage = "Leaf length") %>%
  mutate(pheno.unit = "cm") %>%
  rename(mean = Effect, se = SE) %>%
  mutate(newname = NA) %>% 
  left_join(SMDiffAdapt, by = c("orig", "Treatment", "site")) %>% 
  select(orig, pheno.stage, pheno.unit, Treatment, mean, se, newname, N, site, sm, smOrig, smDiff) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "LaterSM", "WarmLate"))) 

  
LeafSizePlotAdapt <- MeanDataAdapt %>% 
  mutate(Pvalue = factor(1)) %>% 
  ggplot(aes(x = smDiff, y = mean, color = Treatment, shape = Treatment, ymax = mean + se, ymin = mean - se)) +
  geom_hline(yintercept=0, color = "gray", linetype = "dashed") +
  geom_point(size = 3) +
  labs(y = "Difference in leaf size [cm] between\n treatment and destination-control", x = "Difference in SMT between\n origin and destination site [days]", title = "Genetic differentiation: destination-control") +
  scale_colour_manual(values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(2,1,0)) +
  #scale_alpha_manual(values = c(0, 1)) +
  ylim(-2.5, 3) +
  geom_errorbar(width=0.2) +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 32, y = 3, label = "larger", color = "grey20") +
  annotate(geom = "text", x = 32, y = -2.5, label = "smaller", color = "grey20") +
  theme(legend.position="none")


LeafSizes <- plot_grid(LeafSizePlot, LeafSizePlotAdapt, nrow = 1, align = "h")
ggsave(LeafSizes, filename = "LeafSizes.pdf", height = 6)
