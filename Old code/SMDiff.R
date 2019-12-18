



PlasticPlotGGD <- EventDiff %>% 
  filter(pheno.stage %in% c("Bud", "Flower", "Fruit")) %>% 
  left_join(EventDiffN, by = c("orig", "pheno.stage", "pheno.unit", "Treatment")) %>%
  left_join(SMDiff, by = c("orig", "Treatment")) %>% 
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "early & alpine"), c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(newname = factor(newname, levels = c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = smDiff, y = mean, color = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  #geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  labs(y = "Difference in onset of stage in GDD after SM \n between treatment and control", x = "Difference in SM between transplant and control site", title = "Phenotypic plasticity - origin control") +
  geom_errorbar(width=0.18) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  annotate(geom = "text", x = -40, y = 195, label = "higher", color = "grey20") +
  annotate(geom = "text", x = -40, y = -230, label = "lower", color = "grey20") +
  theme(legend.position="none") +
  panel_border(colour = "black", remove = FALSE) +
  facet_grid(~ pheno.stage)
ggsave(PlasticPlotGGD, filename = "PlasticPlotGGD.pdf")





####### ADAPTATION ########


AdaptPlotGDD <- EventDiffAdapt %>% 
  filter(pheno.stage %in% c("Bud", "Flower", "Fruit")) %>% 
  left_join(EventDiffAdaptN, by = c("site", "pheno.stage", "pheno.unit", "Treatment")) %>% 
  mutate(sm = ifelse(site == "Subalpine-early", 167, 
                     ifelse(site == "Subalpine-late", 174, 224))) %>% 
  mutate(sm = ifelse(sm == 174 & Treatment == "Warmer", 173,
                     ifelse(sm == 174 & Treatment == "WarmLate", 175, sm))) %>% 
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "late & subalpine"), c("SM-", "SM+", "T-", "T+", "SM+T+"))) %>%
  mutate(newname = factor(newname, levels = c("SM-", "SM+", "T-", "T+", "SM+T+"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  ggplot(aes(x = sm, y = mean, color = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  xlim(160, 230) +
  scale_colour_manual(name = "Treatment", values = c("red", "blue", "purple")) +
  labs(y = "Difference in onset of stage in GDD after SM \n between treatment and control", x = "Snowmelt in days of year", title = "Genetic differenciation - destination control") +
  geom_errorbar(width=0.18) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme(legend.position="none") +
  geom_text(data = ann_text3, aes(x = x, y = y, label = lab), inherit.aes = FALSE, color = "grey40") +
  annotate(geom = "text", x = 150, y = 100, label = "higher", color = "grey20") +
  annotate(geom = "text", x = 150, y = -370, label = "lower", color = "grey20") +
  panel_border(colour = "black", remove = FALSE) +
  facet_grid(~ pheno.stage)
ggsave(AdaptPlotGDD, filename = "AdaptPlotGDD.pdf")
