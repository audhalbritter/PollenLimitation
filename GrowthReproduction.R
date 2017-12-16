# Is plant size and nr of flowers correlated
Pollination %>% 
  filter(Year == 2017, Variable %in% c("EndSize"), Species == "LEO") %>% 
  ggplot(aes(x = value, y = Flowering, color = Treatment)) +
  geom_point() +
  labs(x = "Longest leaf", y = "Nr of flowers") +
  facet_grid(~ Site)
 

# Is there a corrleation/trade-off between growth and reproduction?
Pollination %>% 
  filter(Year == 2017, Variable %in% c("EndSize", "RepOutput")) %>% 
  spread(key = Variable, value = value) %>% 
  ggplot(aes(x = EndSize, y = RepOutput, color = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(Species ~ Site) 

Pollination %>% 
  filter(Year == 2017, Variable %in% c("Growth", "RepOutput")) %>% 
  spread(key = Variable, value = value) %>% 
  ggplot(aes(x = Growth, y = RepOutput, color = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(Species ~ Site) 

dd <- Pollination %>% 
  filter(Year == 2017, Variable %in% c("EndSize", "RepOutput"), Species == "LEO", Treatment %in% c("Control", "Warmer")) %>% 
  spread(key = Variable, value = value)

summary(lm(log(RepOutput) ~ EndSize * Treatment * OrigPLevel, dd))
fit <- lm(log(RepOutput) ~ EndSize * Treatment * OrigPLevel, dd)
plot(fit)
