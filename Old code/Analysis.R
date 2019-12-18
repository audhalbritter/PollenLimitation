#### ANALYSIS ####
head(Ranunculus)
library("MuMIn")

Ranunculus %>% filter(site == "GUD", pheno.stage == "Bud", pheno.unit == "dogs")


Plastic <- Ranunculus %>% 
  filter(pheno.unit == "dogs", pheno.stage == "Flower") %>%
  # remove Veskre Control
  mutate(OrigSite = paste(orig, site, sep = "_")) %>% 
  filter(OrigSite != "VES_VES") %>% 
  filter(!is.na(value))
  
fit <- lm(value ~ trt * OrigTempLevel * OrigPrecLevel, data = Plastic, na.action = "na.fail")
summary(fit)
dredge(fit)

fit <- glm(value ~ trt + trt:OrigTempLevel + trt:OrigPrecLevel, data = Plastic)
summary(fit)


ggplot(Plastic, aes(x = OrigSite, y = value, fill = trt)) + geom_boxplot() + geom_point(position = position_jitter(width = 0.2))


ggplot(Ranunculus, aes(x = value)) + geom_histogram() + facet_grid(pheno.stage ~ pheno.unit, scales = "free")


library(lme4)
library(lmerTest)
fit <- lmer(value ~ trt + trt:OrigTempLevel + trt:OrigPrecLevel + (1|block), data = Plastic)
summary(fit)
