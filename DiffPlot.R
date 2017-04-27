## ----EventDiffPlot
### PLOT FOR ONSET AND DURATION OF EVENTS BETWEEN CONTROL AND TREATMENT

xAxis <- data_frame(
  Treatment = c(rep("Warmer", 3), rep("Wetter", 3), rep("Warm & wet", 3)),
  orig = c("Alpine-wet", "Alpine-dry", "Subalpine-dry", "Alpine-dry", "Subalpine-dry", "Alpine-wet", "Alpine-wet", "Alpine-dry", "Subalpine-dry"),
  newname = c("wet", "dry", NA, "alpine", "subalpine", NA, NA, "warm & wet", NA)
)

EventDiff <- Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit != "doy") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Fruit", "SM-Bud", "Bud-Flower", "Flower-Fruit"), c("Bud", "Flower", "Fruit", "Bud", "Flower", "Fruit"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Fruit"))) %>%
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-dry", "Subalpine-dry", "Alpine-wet"))) %>%
  mutate(orig = factor(orig, levels = c("Alpine-dry", "Alpine-wet", "Subalpine-dry"))) %>%
  group_by(trt, site, orig, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  ungroup(site) %>% 
  select(-site,-N) %>% 
  unite(united, mean, se, sep = "_") %>% 
  spread(key = trt, value = united) %>% 
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("Warmer_mean", "Warmer_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Wetter, into = c("Wetter_mean", "Wetter_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmWet, into = c("WarmWet_mean", "WarmWet_se"), sep = "_", convert = TRUE) %>% 
  # calculate difference between control and treatment in mean
  mutate(Warmer_mean = Warmer_mean - Control_mean, Wetter_mean = Wetter_mean - Control_mean, WarmWet_mean = WarmWet_mean - Control_mean) %>% 
  # calculate SE for difference
  mutate(Warmer_se = sqrt(Control_se^2 + Warmer_se^2), Wetter_se = sqrt(Control_se^2 + Wetter_se^2), WarmWet_se = sqrt(Control_se^2 + WarmWet_se^2)) %>% 
  select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(Wetter, Wetter_mean, Wetter_se, sep = "_") %>% 
  unite(WarmWet, WarmWet_mean, WarmWet_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -orig, -pheno.stage, -pheno.unit) %>% 
  spread(key = pheno.unit, value = united) %>% 
  separate(col = days, into = c("days_mean", "days_se"), sep = "_", convert = TRUE) %>% 
  separate(col = dogs, into = c("dogs_mean", "dogs_se"), sep = "_", convert = TRUE) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "Wetter", "WarmWet"), c("Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(days_mean = replace(days_mean, c(pheno.stage == "Fruit" & Treatment == "Wetter"), NA)) %>% 
  mutate(days_se = replace(days_se, c(pheno.stage == "Fruit" & Treatment == "Wetter"), NA)) %>% 
  mutate(dogs_mean = replace(dogs_mean, c(pheno.stage == "Fruit" & Treatment == "Wetter"), NA)) %>% 
  mutate(dogs_se = replace(dogs_se, c(pheno.stage == "Fruit" & Treatment == "Wetter"), NA)) %>% 
  left_join(xAxis, by = c("Treatment", "orig")) %>% 
  filter(!is.na(newname)) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Wetter", "Warm & wet"))) %>% 
  mutate(newname = factor(newname, levels = c("dry", "wet", "alpine", "subalpine", "warm & wet")))



ggplot(EventDiff, aes(x = newname, y = dogs_mean, color = Treatment, group = Treatment, shape = pheno.stage, ymax = dogs_mean + 1.96*dogs_se, ymin = dogs_mean - 1.96*dogs_se)) +
  geom_hline(yintercept = 0, color = "grey") +
  scale_colour_manual(name = "Treatment", values = c("red", "blue", "purple")) +
  labs(y = "Onset of event in days after snowmelt", x = "") +
  geom_errorbar(width=0.18) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~ pheno.stage, scales = "free_x")


Ranunculus %>% 
  filter(orig != "VES" | trt != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "days") %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("SM-Bud", "SM-Flower", "SM-Fruit"))) %>%
  mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-dry", "Subalpine-dry", "Alpine-wet"))) %>%
  mutate(orig = factor(orig, levels = c("Alpine-dry", "Alpine-wet", "Subalpine-dry"))) %>%
  group_by(trt, site, orig, pheno.stage) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = 1.96*sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  mutate(treat.stage = paste(trt, pheno.stage)) %>% 
  mutate(treat.stage = factor(treat.stage, levels = c("WarmWet SM-Bud", "WarmWet SM-Flower", "WarmWet SM-Fruit", "Wetter SM-Bud", "Wetter SM-Flower", "Wetter SM-Fruit", "Warmer SM-Bud", "Warmer SM-Flower", "Warmer SM-Fruit", "Control SM-Bud", "Control SM-Flower", "Control SM-Fruit"))) %>% 
  ggplot(aes(x = mean, y = treat.stage, shape = pheno.stage, color = trt, group = trt, xmax = mean + se, xmin = mean - se)) + 
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0.2) +
  geom_line(linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "blue", "purple")) +
  scale_y_discrete(breaks = c("Control SM-Flower", "Warmer SM-Flower", "Wetter SM-Flower", "WarmWet SM-Flower"), labels = c("Control", "Warmer", "Wetter", "Warm & wet")) +
  labs(x = "Mean intervall between SM and stage in days", y = "") +
  facet_grid(~ orig)

