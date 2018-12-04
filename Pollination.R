### Pollination
library("ggforce")

Pollination17 <- Pollination %>% 
  filter(Year == 2017) %>% 
  # remove Second Flowers
  filter(!Pollination == "")


PollinationPlot <- Pollination17 %>% 
  filter(Species == "LEO", Site %in% c("SKJ", "VES")) %>% 
  filter(Variable == "RepOutput") %>% 
  mutate(Pollination = recode(Pollination, "control" = "natural-pollinated", "pollination" = "hand-pollinated")) %>% 
  mutate(Site = recode(Site, "SKJ" = "Alpine - late", "VES" = "Subalpine - late")) %>%  
  ggplot(aes(x = Pollination, y = value, fill = Pollination)) +
  geom_jitter(colour = "grey30") +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(name = "Pollination", values = c("grey", "yellow")) +
  labs(x = "", y = "Reproductive output in g") +
  facet_grid(Treatment ~ Site) +
  theme(axis.text.x = element_text(size = 10))
ggsave(PollinationPlot, filename = "FinalFigures/PollinationPlot.jpg", height = 5, width = 8)




dfPoll <- Pollination17 %>% 
  filter(Species == "LEO", Site %in% c("VES", "SKJ")) %>% 
  filter(Variable == "RepOutput") %>% 
  group_by(Site) %>% 
  do(fit = lm(value ~ Pollination * Treatment, data = .))

tidy(dfPoll, fit) %>% 
  mutate(estimate = (round(estimate, 2)), std.error = round(std.error, 2), statistic = round(statistic, 2), p.value = round(p.value, 3))
