### Easy plots

Pollination %>% 
  filter(Year == "2017", pheno.stage == "Flower", Species == "LEO") %>% 
  ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Origin)




Pollination %>% 
  filter(Year == "2017", pheno.stage == "Bud", Species == "RAN") %>% 
  ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Origin)

Pollination %>% 
  filter(Year == "2017", pheno.stage == "Flower", Species == "LEO") %>% 
  ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Origin)

Pollination %>% 
  filter(Year == "2017", pheno.stage == "Seed", Species == "RAN") %>% 
  ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Origin)

Pollination %>% 
  filter(Year == "2017", pheno.stage == "Flower", Species == "LEO") %>% 
  ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Origin)

Pollination %>% 
  filter(Year == "2017", pheno.stage == "RepOutput", Species == "RAN") %>% 
  ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values=c("grey", "red", "blue", "purple")) +
  facet_wrap(~ Origin)