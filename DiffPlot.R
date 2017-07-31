

## ----EventDiffPlot
### PLOT FOR ONSET OF STAGE BETWEEN TREATMENT AND CONTROL - PLASTICITY

xAxis1 <- data_frame(
  Treatment = c(rep("Warmer", 3), rep("LaterSM", 3), rep("WarmLate", 3)),
  orig = c("Alpine-late", "Alpine-early", "Subalpine-early", "Alpine-early", "Subalpine-early", "Alpine-late", "Alpine-late", "Alpine-early", "Subalpine-early"),
  newname = c("late", "early", NA, "alpine", "subalpine", NA, NA, "early & alpine", NA)
)


# Snowmelt date for site
# Rename to orig site to join and calculate diff in sm for transplants
SM <- Ranunculus %>% select(site, sm) %>% rename(smOrig = sm) %>% distinct()

SMDiff <- Ranunculus %>% 
  select(trt, site, orig, sm) %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  rename(Treatment = trt) %>%
  distinct() %>% 
  left_join(SM, by = c("orig" = "site")) %>% 
  mutate(smDiff = sm - smOrig) %>% 
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late")))


EventDiffData <- Ranunculus %>% 
  #select(-value) %>% 
  #rename(value = CumTempAfterSM) %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit != "doy") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"), c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit"))) %>%
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>%
  mutate(orig = factor(orig, levels = c("Alpine-early", "Alpine-late", "Subalpine-early"))) %>%
  group_by(trt, site, orig, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))

EventDiff <- EventDiffData %>% 
  ungroup(site) %>% 
  select(-site,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = trt, value = united) %>% 
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
  gather(key = Treatment, value = united, -orig, -pheno.stage, -pheno.unit) %>% 
  #spread(key = pheno.unit, value = united) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  #separate(col = days, into = c("days_mean", "days_se"), sep = "_", convert = TRUE) %>% 
  #separate(col = dogs, into = c("dogs_mean", "dogs_se"), sep = "_", convert = TRUE) %>% 
  mutate(mean = replace(mean, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  mutate(se = replace(se, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  #mutate(dogs_mean = replace(dogs_mean, c(pheno.stage == "Fruit" & Treatment == "Wetter"), NA)) %>%
  #mutate(dogs_se = replace(dogs_se, c(pheno.stage == "Fruit" & Treatment == "Wetter"), NA)) %>% 
  left_join(xAxis1, by = c("Treatment", "orig")) %>% 
  filter(!is.na(mean)) %>%
  filter(pheno.unit != "days" | pheno.stage != "Bud") %>% # remove intervall SM-Bud because same as Bud
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Flower", "Bud-", ""), pheno.stage)) %>% 
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Fruit", "Flower-", ""), pheno.stage)) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit", "Bud-Flower", "Flower-Fruit")))


EventDiffN <- EventDiffData %>% 
  ungroup() %>% 
  select(trt, orig, pheno.stage, pheno.unit, N) %>% 
  rename(Treatment = trt) %>% 
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Flower", "Bud-", ""), pheno.stage)) %>% 
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Fruit", "Flower-", ""), pheno.stage))


ann_text1 <- data.frame(x = c(5,5), y = c(13, -25), lab = c("late", "early"),
                        pheno.stage = factor("Fruit",levels = c("Bud","Flower","Fruit")))

EventDiffPlot <- EventDiff %>% 
  filter(pheno.stage %in% c("Bud", "Flower", "Fruit")) %>% 
  left_join(EventDiffN, by = c("orig", "pheno.stage", "pheno.unit", "Treatment")) %>%
  left_join(SMDiff, by = c("orig", "Treatment")) %>% 
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "early & alpine"), c("Early SM", "Late SM", "Cold", "Warm", "Early SM & cold"))) %>%
  mutate(newname = factor(newname, levels = c("Early SM", "Late SM", "Cold", "Warm", "Early SM & cold"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = smDiff, y = mean, color = Treatment, shape = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(17,16,15)) +
  labs(y = "Difference in onset of stage [days] after SMT \n between treatment and origin-control", x = "Difference in SMT between origin and destination site [days]", title = "Phenotypic plasticity: origin-control") +
  geom_errorbar(width=0.18) +
  geom_point(size = 3) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme(legend.position="none") +
  panel_border(colour = "black", remove = FALSE) +
  annotate(geom = "text", x = 25, y = 13, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -25, label = "earlier", color = "grey20") +
  #geom_text(data = ann_text1, aes(x = x, y = y, label = lab), inherit.aes = FALSE, color = "grey40") + 
  facet_grid(~ pheno.stage)
ggsave(EventDiffPlot, filename = "EventDiffPlot.pdf", height = 4.5)

 ann_text2 <- data.frame(x = c(5,5), y = c(-12, 13), lab = c("shorter", "longer"),
                        pheno.stage = factor("Flower-Fruit",levels = c("Bud-Flower","Flower-Fruit")))

EventIntervallPlot <- EventDiff %>% 
  filter(pheno.stage %in% c("Bud-Flower", "Flower-Fruit")) %>% 
  left_join(EventDiffN, by = c("orig", "pheno.stage", "pheno.unit", "Treatment")) %>%
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "early & alpine"), c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(newname = factor(newname, levels = c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = newname, y = mean, color = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  labs(y = "Difference (treatment - control)in intervall\n of stage in days", x = "") +
  geom_errorbar(width=0.18) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("dashed", "solid"), guide = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top") +
  geom_text(data = ann_text2, aes(x = x, y = y, label = lab), inherit.aes = FALSE, color = "grey40") +
  facet_grid(~ pheno.stage)

PlasticPlot <- plot_grid(EventDiffPlot, EventIntervallPlot, ncol = 2, rel_widths = c(3, 2))



## ----IntervallDiffPlot
### PLOT FOR INTERVALL BETWEEN SM AND STAGE - PLASTICITY

xAxis2 <- data_frame(
  Treatment = c(rep("Control", 3), rep("Warmer", 3), rep("Wetter", 3), rep("WarmWet", 1)),
  orig = c("Alpine-dry", "Subalpine-dry", "Alpine-wet", "Alpine-dry", "Subalpine-dry", "Alpine-wet", "Alpine-dry", "Subalpine-dry", "Alpine-wet", "Alpine-dry"),
  newname = c("C", "C", "C", "dry", NA, "wet", "alpine", "subalpine", NA, "warm & wet")
)

  
###############################################################################
#### ADAPTATION ####
###############################################################################


SMDiffAdapt <- Ranunculus %>% 
  select(trt, site, orig, sm) %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Veskre
  rename(Treatment = trt) %>%
  distinct() %>% 
  left_join(SM, by = c("orig" = "site")) %>% 
  mutate(smDiff = sm - smOrig) %>% 
  mutate(site = plyr::mapvalues(site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late")))


## ----EventDiffAdaptPlot
### PLOT FOR ONSET OF STAGE BETWEEN TREATMENT AND CONTROL - ADAPTATION

xAxis3 <- data_frame(
  Treatment = c(rep("Warmer", 3), rep("LaterSM", 3), rep("WarmLate", 3)),
  site = c(rep(c("Subalpine-early", "Subalpine-late", "Alpine-late"), 3)),
  newname = c("early", "late", NA, NA, "subalpine", "alpine", NA, "late & subalpine", NA)
)

EventDiffAdaptData <- Ranunculus %>% 
  #select(-value) %>% 
  #rename(value = CumTempAfterSM) %>%
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  filter(pheno.unit != "doy") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"), c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit"))) %>%
  mutate(site = plyr::mapvalues(site, c("RAM", "VES", "SKJ"), c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  mutate(site = factor(site, levels = c("Subalpine-early", "Subalpine-late", "Alpine-late"))) %>%
  group_by(trt, site, orig, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))

EventDiffAdapt <- EventDiffAdaptData %>%   
  ungroup(orig) %>% 
  select(-orig,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = trt, value = united) %>% 
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
  gather(key = Treatment, value = united, -site, -pheno.stage, -pheno.unit) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% 
  mutate(mean = replace(mean, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  mutate(se = replace(se, c(pheno.stage == "Fruit" & Treatment == "LaterSM"), NA)) %>% 
  left_join(xAxis3, by = c("Treatment", "site")) %>% 
  filter(pheno.unit != "days" | pheno.stage != "Bud") %>% # remove intervall SM-Bud because same as Bud
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Flower", "Bud-", ""), pheno.stage)) %>% 
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Fruit", "Flower-", ""), pheno.stage)) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit", "Bud-Flower", "Flower-Fruit")))


EventDiffAdaptN <- EventDiffAdaptData %>% 
  ungroup() %>% 
  select(trt, site, pheno.stage, pheno.unit, N) %>% 
  rename(Treatment = trt) %>% 
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Flower", "Bud-", ""), pheno.stage)) %>% 
  mutate(pheno.stage = paste0(ifelse(pheno.unit == "days" & pheno.stage == "Fruit", "Flower-", ""), pheno.stage))



ann_text3 <- data.frame(x = c(5, 5), y = c(25, -28), lab = c("late", "early"),
                       pheno.stage = factor("Fruit",levels = c("Bud","Flower","Fruit")))

EventDiffAdaptPlot <- EventDiffAdapt %>% 
  filter(pheno.stage %in% c("Bud", "Flower", "Fruit")) %>% 
  left_join(EventDiffAdaptN, by = c("site", "pheno.stage", "pheno.unit", "Treatment")) %>% 
  left_join(SMDiffAdapt, by = c("site", "Treatment")) %>% 
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "late & subalpine"), c("SM-", "SM+", "T-", "T+", "SM+T+"))) %>%
  mutate(newname = factor(newname, levels = c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Pvalue = c(rep(1, 12), rep(2,3))) %>% 
  mutate(Pvalue = factor(Pvalue)) %>% 
  ggplot(aes(x = smDiff, y = mean, color = Treatment, shape = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(17, 16, 0)) +
  labs(y = "Difference in onset of stage [days] after SMT \n between treatment and destination-control", x = "Difference in SMT between origin and destination site [days]", title = "Genetic differentiation: destination-control") +
  geom_errorbar(width=0.18) +
  geom_point(size = 3) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme(legend.position="none") +
  annotate(geom = "text", x = 25, y = 27, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -31, label = "earlier", color = "grey20") +
  panel_border(colour = "black", remove = FALSE) +
  #geom_text(data = ann_text3, aes(x = x, y = y, label = lab), inherit.aes = FALSE, color = "grey40") +
  facet_grid(~ pheno.stage)
ggsave(EventDiffAdaptPlot, filename = "EventDiffAdaptPlot.pdf", height = 4.5)



ann_text4 <- data.frame(x = c(5, 5), y = c(-14, 14), lab = c("shorter", "longer"),
                        pheno.stage = factor("Flower-Fruit",levels = c("Bud-Flower","Flower-Fruit")))

EventIntervallAdaptPlot <- EventDiffAdapt %>% 
  filter(pheno.stage %in% c("Bud-Flower", "Flower-Fruit")) %>% 
  left_join(EventDiffAdaptN, by = c("site", "pheno.stage", "pheno.unit", "Treatment")) %>% 
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "late & subalpine"), c("SM-", "SM+", "T-", "T+", "SM+T+"))) %>%
  mutate(newname = factor(newname, levels = c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = newname, y = mean, color = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  labs(y = "Difference (treatment - control) in intervall\n of stage in days", x = "") +
  geom_errorbar(width=0.18) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("dashed", "solid"), guide = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top") +
  geom_text(data = ann_text4, aes(x = x, y = y, label = lab), inherit.aes = FALSE, color = "grey40") +
  facet_grid(~ pheno.stage)


AdaptPlot <- plot_grid(EventDiffAdaptPlot, EventIntervallAdaptPlot, ncol = 2, rel_widths = c(3, 2))





## ----IntervallDiffAdaptPlot
### PLOT FOR INTERVALL BETWEEN SM AND STAGE - ADAPTATION

IntervallSMEventAddaptPlot <- Ranunculus %>% 
  filter(orig != "GUD" | trt != "Control") %>% # remove Control at Gudmedalen
  filter(pheno.unit == "days") %>% 
  mutate(site = plyr::mapvalues(site, c("RAM", "VES", "SKJ"), c("Subalpine-dry", "Subalpine-wet", "Alpine-wet"))) %>%
  mutate(site = factor(site, levels = c("Subalpine-dry", "Subalpine-wet", "Alpine-wet"))) %>%
  mutate(orig = factor(orig, levels = c("Alpine-dry", "Alpine-wet", "Subalpine-dry"))) %>%
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = 1.96*sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(treat.stage = paste(trt, pheno.stage)) %>% 
  mutate(treat.stage = factor(treat.stage, levels = c("WarmWet SM-Bud", "WarmWet SM-Flower", "WarmWet SM-Fruit", "Wetter SM-Bud", "Wetter SM-Flower", "Wetter SM-Fruit", "Warmer SM-Bud", "Warmer SM-Flower", "Warmer SM-Fruit", "Control SM-Bud", "Control SM-Flower", "Control SM-Fruit"))) %>% 
  mutate(mean = replace(mean, c(pheno.stage == "SM-Fruit" & trt == "Wetter"), NA)) %>% 
  mutate(se = replace(se, c(pheno.stage == "SM-Fruit" & trt == "Wetter"), NA)) %>% 
  ggplot(aes(x = mean, y = treat.stage, shape = pheno.stage, color = trt, group = trt, xmax = mean + se, xmin = mean - se)) + 
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0.2) +
  geom_line(linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "blue", "purple")) +
  scale_shape_manual(name = "Stage", values = c(16, 17, 15)) +
  scale_y_discrete(breaks = c("Control SM-Flower", "Warmer SM-Flower", "Wetter SM-Flower", "WarmWet SM-Flower"), labels = c("Control", "Warmer", "Wetter", "Warm & wet")) +
  labs(x = "Mean intervall between SM and stage in days", y = "") +
  facet_grid(~ site)
