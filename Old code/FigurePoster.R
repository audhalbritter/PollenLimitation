

FlowerAdapt <- EventDiffAdapt %>% 
  filter(pheno.stage %in% c("Flower")) %>% 
  left_join(EventDiffAdaptN, by = c("site", "pheno.stage", "pheno.unit", "Treatment")) %>% 
  left_join(SMDiffAdapt, by = c("site", "Treatment")) %>% 
  mutate(newname = plyr::mapvalues(newname, c("early", "late", "alpine", "subalpine", "late & subalpine"), c("SM-", "SM+", "T-", "T+", "SM+T+"))) %>%
  mutate(newname = factor(newname, levels = c("SM-", "SM+", "T-", "T+", "SM-T-"))) %>%
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Pvalue = c(rep(1, 4), rep(2,1))) %>% 
  mutate(Pvalue = factor(Pvalue)) %>% 
  ggplot(aes(x = smDiff, y = mean, color = Treatment, shape = Treatment, group = Treatment, ymax = mean + 1.96*se, ymin = mean - 1.96*se, linetype = N > 3), shape = pheno.stage) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_colour_manual(name = "Treatment", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Treatment:", values = c(17, 16, 0)) +
  labs(y = "Diff. in onset of flowering after SMT \n[treatment - destination-control]", x = "Difference in SMT between origin and destination site [days]") +
  geom_errorbar(width=0.18) +
  geom_point(size = 5) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme(legend.position="none") +
  annotate(geom = "text", x = 25, y = 27, label = "later", color = "grey20") +
  annotate(geom = "text", x = 25, y = -31, label = "earlier", color = "grey20") +
  panel_border(colour = "black", remove = FALSE)
