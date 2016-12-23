# plant size data
size <- read.table("size.csv", header = TRUE, sep = ";")
head(size)
size <- size %>% 
  filter(sp == "RAN") %>% 
  select(-Torig, -Porig, -Tdest, -Pdest, -DOY, -blk) %>% 
  mutate(trt = plyr::mapvalues(trt, c("c", "wa", "we", "ww"), c("Control", "Warmer", "Wetter", "WarmWet")))

size %>% 
  ggplot() +
  ggtitle("First flowering") +
  geom_boxplot(aes(x= orig, y = size_end, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))


MeanSE <- size %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  group_by(trt, site, orig) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = 2*sd(size_end, na.rm = TRUE)/sqrt(N))

# only for checking results
MeanSE %>% filter(trt %in% c("Control", "Warmer")) %>% 
  ggplot(aes(x = trt, y = mean, color = orig, group = orig)) +
  geom_point() +
  geom_line()

# Calculate difference between Control and Treatment for SE
SEData <- MeanSE %>% 
  ungroup() %>% 
  select(-mean, -N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = se) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = SE, -orig, -Control) %>% # gather Treatments
  filter(!is.na(SE)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(orig, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("GUD_Warmer", "SKJ_Warmer", "GUD_Wetter", "RAM_Wetter", "GUD_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  select(SE, orig, Treatment)

# Calculate difference between Control and Treatment for Mean
MeanData <- MeanSE %>% 
  ungroup() %>% 
  select(-se, -N, -site) %>% # remove site, because it causes problems
  spread(key = trt, value = mean) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = Effect, -orig, -Control) %>% # gather Treatments
  filter(!is.na(Effect)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(orig, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("GUD_Warmer", "SKJ_Warmer", "GUD_Wetter", "RAM_Wetter", "GUD_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  left_join(SEData, by = c("orig" = "orig", "Treatment" = "Treatment")) %>%  # join SE
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "Wetter", "WarmWet"), c("Warmer", "Wetter", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Wetter", "Warm & wet"))) 


LeafSizePlot <- ggplot(MeanData, aes(x = newname, y = Effect, color = Treatment, ymax = Effect + SE, ymin = Effect - SE)) +
  geom_hline(yintercept=0, color = "gray") +
  geom_point(size = 1.8) +
  labs(x = "", y = "Treatment - origin\n control for leaf size in cm") +
  scale_colour_manual(name = "Treatment", values = c("red", "blue", "purple")) +
  ylim(-2.5, 3) +
  geom_errorbar(width=0.2) +
  theme(text = element_text(size = 9), axis.text = element_text(size = 9))

#print(LeafSizePlot)
save_plot("LeafSizePlot_PP.jpeg", LeafSizePlot,base_aspect_ratio = 1.2)



#### ADAPTATION

size %>% 
  ggplot() +
  geom_boxplot(aes(x= site, y = size_end, fill=trt)) +
  scale_fill_manual(values=c("white", "red", "blue", "purple"))

MeanSEAdapt <- size %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  group_by(trt, site, orig) %>% 
  summarise(N = sum(!is.na(size_end)), mean = mean(size_end, na.rm = TRUE), se = 2*sd(size_end, na.rm = TRUE)/sqrt(N))

# only for checking results
MeanSEAdapt %>% filter(trt %in% c("Control", "Wetter")) %>% 
  ggplot(aes(x = trt, y = mean, color = site, group = site)) +
  geom_point() +
  geom_line()


# Calculate difference between Control and Treatment for SE
SEData <- MeanSEAdapt %>% 
  ungroup() %>% 
  select(-mean, -N, -orig) %>% # remove origin, because it causes problems
  spread(key = trt, value = se) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = SE, -site, -Control) %>% # gather Treatments
  filter(!is.na(SE)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(site, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("RAM_Warmer", "VES_Warmer", "SKJ_Wetter", "VES_Wetter", "VES_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  select(SE, site, Treatment)

# Calculate difference between Control and Treatment for Mean
MeanDataAdapt <- MeanSEAdapt %>% 
  ungroup() %>% 
  select(-se, -N, -orig) %>% # remove orig, because it causes problems
  spread(key = trt, value = mean) %>% # spread Treatments
  mutate(Warmer = Warmer - Control, Wetter = Wetter - Control, WarmWet = WarmWet - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = Effect, -site, -Control) %>% # gather Treatments
  filter(!is.na(Effect)) %>% # remove e.g. Warmer in RAM, no such treatment
  mutate(newname = paste(site, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("RAM_Warmer", "VES_Warmer", "SKJ_Wetter", "VES_Wetter", "VES_WarmWet"), c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet"))) %>% 
  left_join(SEData, by = c("site" = "site", "Treatment" = "Treatment")) %>%  # join SE
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "Wetter", "WarmWet"), c("Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Wetter", "Warm & wet"))) 


LeafSizePlotAdapt <- ggplot(MeanDataAdapt, aes(x = newname, y = Effect, color = Treatment, ymax = Effect + SE, ymin = Effect - SE)) +
  geom_hline(yintercept=0, color = "gray") +
  geom_point(size = 1.8) +
  labs(x = "", y = "Treatment - dest.\n control for leaf size in cm") +
  scale_colour_manual(name = "", values = c("red", "blue", "purple")) +
  ylim(-2.5, 3) +
  geom_errorbar(width=0.2) +
  theme(text = element_text(size = 9), axis.text = element_text(size = 9))


#print(LeafSizePlotAdapt)
save_plot("LeafSizePlotAdapt.jpeg", LeafSizePlotAdapt,base_aspect_ratio = 1.2)

library("cowplot")
LeafSizes <- plot_grid(LeafSizePlot, LeafSizePlotAdapt, labels = c("1)", "2)"), nrow = 2, align = "v")
save_plot("LeafSizes.jpeg", LeafSizes,base_aspect_ratio = 1.3)
