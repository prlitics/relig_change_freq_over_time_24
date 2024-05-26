# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline


# Load packages required to define the pipeline:
library(targets)
library(htmltools)
#library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","gssr","tidyverse",
               "ggalluvial","brms","marginaleffects",
               "sysfonts","showtext","ggfittext",
               "ggplot2", "infixit",
               "ggdist") # Packages that your targets need for their tasks.

)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = gss_data,
    command = load_gss_data()
  ),
  tar_target(
    name = cleaned_gss_data,
    command = gss_etl(gss_data)
  ),
  tar_target(
    name = relig_change_props,
    command = religion_change_proportions(cleaned_gss_data),
  ),
  tar_target(
    name = faith_transitions,
    command = detailed_faith_transition_figures(cleaned_gss_data),
  ),
  tar_target(
    name = sank_data,
    command = sankey_data(cleaned_gss_data),
  ),
  tar_target(
    name = sank_plot,
    command = sankey_chart(sank_data),
  ),
  tar_target(
    name = regress_data,
    command = regression_data(cleaned_gss_data)
  ),
  tar_target(
    name = brm_mod,
    command = {brm(same_faith~relig16_grouped + (1 + relig16_grouped | year),
                    data = regress_data,
                    prior = prior(normal(0,2), class = "b"),
                    cores = 8,
                    file = file.path("data","model","relig_retention.rds"),
                    backend = 'cmdstanr',
                    family = 'bernoulli')}
  ),
  
  tar_target(
    name = pred_draws,
    command = {marginaleffects::posterior_draws(
      marginaleffects::predictions(brm_mod,
                                   marginaleffects::datagrid(year = unique(regress_data$year),
                                             relig16_grouped = c(0:3)))
      )}    

),

tar_target(
  name = relig_change_chart,
  command = relig_prop_chart(relig_change_props)
),

tar_target(
  name = posterior_fig,
  command = bayes_density_plot(pred_draws)
)

)
