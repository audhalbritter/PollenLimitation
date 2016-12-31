# Seeds

library("readxl")

seed <- read_excel("data_pollenlimitaiton_Sept16.xlsx", sheet = 6, col_names = TRUE)
head(seed)
seed <- seed %>% 
  select(Site, Origin, ID, ind, TOT_Weight) %>% 
  mutate(trt = NA) %>% 
  mutate(trt = replace(trt, which(Site == Origin), "Control")) %>% 
  mutate(trt = ifelse(Origin %in% c("GUD") & Site %in% c("RAM"), "Warm", trt)) %>% 
  mutate(trt = ifelse(Origin %in% c("SKJ") & Site %in% c("VES"), "Warm", trt)) %>% 
  mutate(trt = ifelse(Origin %in% c("GUD") & Site %in% c("VES"), "WarmWet", trt)) %>% 
  mutate(trt = ifelse(Origin %in% c("GUD") & Site %in% c("SKJ"), "Wet", trt)) %>% 
  mutate(trt = ifelse(Origin %in% c("RAM") & Site %in% c("VES"), "Wet", trt))

 
  
  
seed%>% 
  ggplot() +
  ggtitle("Seed set") +
  geom_boxplot(aes(x= Origin, y = TOT_Weight, fill = trt)) +
  scale_fill_manual(values=c("white", "red", "purple", "blue"))
  


dat <- seed %>% 
  #filter(Origin == "GUD") %>%
  group_by(trt) %>% 
  summarise(n = n())
  
hist(log(dat$TOT_Weight))
fit <- lm(log(TOT_Weight) ~ trt, dat)
summary(fit)


### Initial size
initial.size <- read_excel(path = "~/Dropbox/Pollen limitation/Pollen limitation 2015 DATA/DataSheet_2015_Aud.xlsx", sheet = 2, col_names = TRUE)
head(initial.size)
initial.size <- initial.size %>% filter(Species == "RAN") %>% 
  group_by(Origin) %>% 
  summarise(n = n(), mena = mean(initial_size))

  
hist(initial.size$initial_size)

fit <- lm(initial_size ~ Origin, data = initial.size)
par(mfrow=c(2,2))
plot(fit)
summary(fit)
