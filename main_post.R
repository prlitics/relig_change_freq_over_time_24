renv::install('kjhealy/gssr')
renv::install('tidyverse')
renv::install('srvyr')
renv::install('ggalluvial')
renv::install('brms')
renv::install("stan-dev/cmdstanr")
renv::install("marginaleffects")
renv::install("")



regress_data <- gss_etl(gss_dat) |>
  dplyr::mutate(same_faith = ifelse(relig == relig16,1,0)) |>
  dplyr::mutate(relig16_grouped = dplyr::case_when(
    relig16 %in% 4 ~ 0,
    relig16 %in% c(1,2,10,11) ~ 1,
    relig16 %in% c(3) ~ 2,
    relig16 %in% c(5:9,12:13) ~ 3,
    
  )) |>
  mutate(relig16_grouped = as.factor(relig16_grouped)) 

brm_mod <- brm(same_faith~relig16_grouped + (1 + relig16_grouped | year),
    data = regress_data,
    prior = prior(normal(0,2), class = "b"),
    cores = 8,
    file = file.path("data","model","relig_retention.rds"),
    backend = 'cmdstanr',
    family = 'bernoulli')




library(marginaleffects)

pred_draws <- predictions(brm_mod, datagrid(year = unique(regress_data$year),
                              relig16_grouped = c(0:3))) |>
  posterior_draws()




pd <- predictions(brm_mod, datagrid(year = unique(regress_data$year),
                              relig16_grouped = c(0:3)))



