# load library
library("lubridate")
library("magrittr")
library("tidyverse")
library("Hmisc")


# Import data
load(file = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Daily.Temperature_2008-2017.RData", verbose = TRUE)


### ONLY temp30cm AT ALL SITES
dat <- dailyTemperature %>% 
  select(logger, site, date, value) %>% 
  filter(year(date) > 2009) %>% 
  unite(col = Log_Site, c("logger", "site"), sep = "_") %>% 
  spread(key = Log_Site, value = value) %>% 
  as.tibble()

#impute_vars <- aregImpute(formula = ~ Skjellingahaugen + Gudmedalen + Lavisdalen + Ulvehaugen + Veskre + Rambera + Hogsete + Alrust + Ovstedalen + Arhelleren + Vikesland + Fauske, data = dat, n.impute = 5)
impute_vars <- aregImpute(formula = ~ temp200cm_Alrust + temp200cm_Arhelleren + temp200cm_Fauske + temp200cm_Gudmedalen + temp200cm_Hogsete + temp200cm_Lavisdalen + temp200cm_Ovstedalen + temp200cm_Rambera + temp200cm_Skjellingahaugen + temp200cm_Ulvehaugen + temp200cm_Veskre + temp200cm_Vikesland + temp30cm_Alrust + temp30cm_Arhelleren + temp30cm_Fauske + temp30cm_Gudmedalen + temp30cm_Hogsete + temp30cm_Lavisdalen + temp30cm_Ovstedalen + temp30cm_Rambera + temp30cm_Skjellingahaugen + temp30cm_Ulvehaugen + temp30cm_Veskre + temp30cm_Vikesland + tempabove_Alrust + tempabove_Arhelleren + tempabove_Fauske + tempabove_Gudmedalen + tempabove_Hogsete + tempabove_Lavisdalen + tempabove_Ovstedalen + tempabove_Rambera + tempabove_Skjellingahaugen + tempabove_Ulvehaugen + tempabove_Veskre + tempabove_Vikesland + tempsoil_Alrust + tempsoil_Arhelleren + tempsoil_Fauske + tempsoil_Gudmedalen + tempsoil_Hogsete + tempsoil_Lavisdalen + tempsoil_Ovstedalen + tempsoil_Rambera + tempsoil_Skjellingahaugen + tempsoil_Ulvehaugen + tempsoil_Veskre + tempsoil_Vikesland, data = dat, n.impute = 5)

# R squared
impute_vars$rsq

DailyTemperature_Imputed <- 1:5 %>% 
  map(~ impute.transcan(impute_vars, imputation=., data=dat, list.out=TRUE, pr=FALSE, check=FALSE)) %>% 
  map(map_df, extract) %>% 
  map_df(bind_cols, dat %>% select(date)) %>% 
  group_by(date) %>% 
  summarise_all(mean) %>% 
  gather(key = Logger_Site, value = value, -date) %>% 
  bind_cols(dat %>% 
              gather(key = site, value = value, - date) %>% 
              select(-date) %>% 
              mutate(imputation = if_else(is.na(value), "imputed", "not imputed"))) %>% 
  select(-site, -value1) %>% 
  separate(Logger_Site, c("logger", "site"), sep = "_")

save(DailyTemperature_Imputed, file = "DailyTemperature_Imputed.Rdata")

ggplot(dat_new, aes(x = date, y = value, color = imputation)) +
  geom_line() +
  facet_wrap(~ site)

dat_new %>% 
  filter(logger == "temp30cm") %>% 
  ggplot(aes(x = date, y = value, color = imputation)) +
  geom_line() +
  facet_wrap(~ site)
