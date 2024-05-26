library(tidyverse)
library(gssr)
library(srvyr)

data("gss_all")


#
wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",             
             "wtssall",             
             "sampcode",           
             "sample",
             "year", 
             "id", 
             "ballot") 

analyses_vars <- c("relig",
                   "relig16",
                   "sprtprsn")

options(survey.lonely.psu = "adjust")
options(na.action="na.pass")


gsscumu <- gss_all %>%
  select(all_of(wt_vars),
         all_of(analyses_vars)) %>%
  tidyr::drop_na(relig, relig16, sprtprsn, wtssall) %>%
  mutate(same_faith = ifelse(relig == relig16,1,0)) %>%
  mutate(faith_shift = case_when(
    relig == relig16 & relig16 != 4 ~ "Same Faith",
    relig == relig16 & relig16 == 4 ~ "Same (Non)Faith",
    relig16 %in% c(1:3,5:13) & relig %in% c(1:3,5:13) & relig != relig16 ~ "Faithful -> New Faith",
    relig16 %in% c(1:3,5:13) & relig %in% 4 & relig != relig16 & sprtprsn %in% 1:3 ~ "Faithful -> No Faith (but spiritual)",
    relig16 %in% c(1:3,5:13) & relig %in% 4 & relig != relig16 & sprtprsn %in% 4 ~ "Faithful -> No Faith (not spiritual)",
    relig %in% c(1:3,5:13) & relig16 %in% 4 & relig != relig16 ~ "No Faith -> Faithful"
  ))

  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = wtssall,
                   nest = TRUE)
  

gsscumu %>%
  mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = wtssall,
                   nest = TRUE)
  group_by(year) %>%
  summarise(prop = survey_mean(same_faith, na.rm = T, vartype = 'ci'))

gsscumu %>%
  count(year, faith_shift, wt = wtssall) %>%
  group_by(year) %>%
  mutate(ysum = sum(n)) %>%
  mutate(props = n/ysum) %>%
  filter(year %in% c(1998,2018)) %>% View() 

