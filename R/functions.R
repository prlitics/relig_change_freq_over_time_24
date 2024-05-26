options(rel_wt_vars = c("vpsu",
                        "vstrat",
                        "oversamp",
                        "formwt",             
                        "wtssall",             
                        "sampcode",           
                        "sample",
                        "year", 
                        "id", 
                        "ballot"),
        
        rel_analyses_vars = c("relig",
                              "relig16",
                              "sprtprsn"))


load_gss_data <- function(){
  
  on.exit(rm(gss_all, envir = globalenv()))
  
  data("gss_all", package = "gssr")
  return(globalenv()$gss_all)

}


gss_etl <- function(data){
  
  data <- data |>
    dplyr::select(dplyr::all_of(getOption('rel_wt_vars')),
                  dplyr::all_of(getOption('rel_analyses_vars'))) |>
    tidyr::drop_na(relig, relig16, sprtprsn, wtssall) |>
    dplyr::select(dplyr::all_of(getOption('rel_wt_vars')),
                  dplyr::all_of(getOption('rel_analyses_vars')))
    
  
  
  return(data)
  
  
}



religion_change_proportions <- function(cleaned_data){
  
  cleaned_data |>
   dplyr::mutate(same_faith = ifelse(relig == relig16,1,0)) |>
    dplyr::mutate(stratvar = interaction(year, vstrat)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(prop = weighted.mean(same_faith)) |>
    dplyr::mutate(remain = 1-prop)
  
  
}

detailed_faith_transition_figures <- function(cleaned_data){
  
  cleaned_data |>
    dplyr::mutate(faith_shift = dplyr::case_when(
      relig == relig16 & relig16 != 4 ~ "Same Faith",
      relig == relig16 & relig16 == 4 ~ "Same (Non)Faith",
      relig16 %in% c(1:3,5:13) & relig %in% c(1:3,5:13) & relig != relig16 ~ "Faithful -> New Faith",
      relig16 %in% c(1:3,5:13) & relig %in% 4 & relig != relig16 & sprtprsn %in% 1:3 ~ "Faithful -> No Faith (but spiritual)",
      relig16 %in% c(1:3,5:13) & relig %in% 4 & relig != relig16 & sprtprsn %in% 4 ~ "Faithful -> No Faith (not spiritual)",
      relig %in% c(1:3,5:13) & relig16 %in% 4 & relig != relig16 ~ "No Faith -> Faithful"
    )) |>
    dplyr::count(year, faith_shift, wt = wtssall) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(ysum = sum(n)) %>%
    dplyr::mutate(props = n/ysum) %>%
    dplyr::filter(year %in% c(1998,2018))
  
}

regression_data <- function(cleaned_data){
  
  cleaned_data|>
  dplyr::mutate(same_faith = ifelse(relig == relig16,1,0)) |>
    dplyr::mutate(relig16_grouped = dplyr::case_when(
      relig16 %in% 4 ~ 0,
      relig16 %in% c(1,2,10,11) ~ 1,
      relig16 %in% c(3) ~ 2,
      relig16 %in% c(5:9,12:13) ~ 3,
      
    )) |>
    mutate(relig16_grouped = as.factor(relig16_grouped)) 
  
  
  
}



sankey_data <- function(cleaned_data){
  
  cleaned_data |>
    haven::zap_formats()|>
    dplyr::mutate(dplyr::across(c(relig16, relig), as.numeric)) |>
    dplyr::mutate(faith_init = 
      dplyr::case_when(
      relig16 != 4 ~ "Religious",
      relig16 == 4 ~ "Non-\nReligious")) |>
    dplyr::mutate(faith_adult = dplyr::case_when(
      relig == relig16 & relig16 != 4 ~ "Same\nReligion",
      relig %in% c(1:3,5:13) & relig != relig16 ~ "Different\nReligion",
      relig %in% 4 & sprtprsn %in% 1:3 ~ "Non-\nReligious\n(Spiritual)",
      relig %in% 4 & sprtprsn %in% 4 ~ "Non-\nReligious\n(Non-Spiritual)")) |>
    dplyr::select(year, wtssall, faith_init, faith_adult) |>
    dplyr::mutate(faith_init = factor(faith_init, levels = c("Non-\nReligious","Religious")),
                  faith_adult = factor(faith_adult, levels = c("Non-\nReligious\n(Spiritual)",
                                                               "Non-\nReligious\n(Non-Spiritual)",
                                                               "Different\nReligion",
                                                               "Same\nReligion"
                                                               ))) |>
    dplyr::filter(year %in% c(1998,2018)) |>
    dplyr::count(year, faith_init, faith_adult, wt = wtssall) |>
    dplyr::group_by(year) |>
    dplyr::mutate(prop = n/sum(n),
                  test = as.factor(dplyr::row_number())) |>
    dplyr::ungroup() |>
    dplyr::select(-n)
  
}




sankey_chart <- function(sankey_data){
  
  sysfonts::font_add_google("Sora","sora")
  sysfonts::font_add_google("Bree Serif","bree")
  
  
  showtext::showtext_auto()
  
  sankey_data |>
    ggplot2::ggplot()+
    ggplot2::aes(axis1 = faith_init,
        axis2 = faith_adult,
        y = prop) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = test)) +
    ggalluvial::geom_stratum(fill = '#fffcf9',
                 color = 'gray') +
    ggfittext::geom_fit_text(stat = "stratum", 
                             width = 1,
                             color = "#4E555B",
                             ggplot2::aes(label = after_stat(stratum))) +
    ggplot2::scale_x_discrete(limits = c(1,2),
                     expand = c(.02,.02),
                     labels = (c("Youth\n","Adults\n"))) +
    ggplot2::ylab('')+
    ggplot2::scale_y_continuous(labels = scales::label_percent(1)) +
    ggplot2::labs(title = 'Most people remain in the faith that they were raised with.',
         subtitle = '\nHowever, this is less true in 2018 than in 1998, with the largest difference coming in\nthe growth of Non-Religious (but Spiritual) adults.\n',
         caption = 'Data Source: General Social Survey Cumulative File\nAnalysis & visualization by Peter Licari (@PRLPoliSci)')+
    ggplot2::guides(fill = 'none') +
    ggplot2::facet_wrap(~year) +
    ggplot2::theme_minimal()+
    ggplot2::theme(text = ggplot2::element_text(color = "#4E555B",
                              family = 'sora'),
          strip.text = ggplot2::element_text(size = 18, "#4E555B"),
          panel.border = ggplot2::element_rect(color = "#4E555B",
                                      fill = NA),
          panel.grid = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(fill = '#fffcf9', color = "#4E555B"),
          plot.background = ggplot2::element_rect(fill = '#fffcf9', color = "#4E555B"),
          plot.subtitle = ggplot2::element_text(size = 20, hjust = .5, vjust = 1,"#001021"),
          axis.text = ggplot2::element_text(size = 12, color = "#4E555B"),
          plot.title = ggplot2::element_text(size = 34, hjust = .5, vjust = .2, color = "#001021", face = 'bold',
                                    family = 'bree'
          ))
  

  
}


relig_prop_chart <- function(r_change_data){
  
  sysfonts::font_add_google("Sora","sora")
  sysfonts::font_add_google("Bree Serif","bree")
  
  
  showtext::showtext_auto()
  
  r_change_data |>
    tidyr::pivot_longer(!year) |>
    dplyr::mutate(name = factor(name, levels = c('remain','prop'))) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = value, x = 1, fill = name) +
    ggplot2::geom_bar(position = ggplot2::position_stack(), 
                      stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = round(value * 100,0) %+% "%"),
                       position = ggplot2::position_stack(vjust = .5),
                       family = 'sora', color = '#fffcf9',
                       fontface = 'bold') +
    ggplot2::coord_flip() + 
    ggplot2::facet_wrap(~year, ncol = 1, strip.position = "left") +
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    ggplot2::scale_fill_manual(values = c("#677db7","#f18f01"),
                              name = "",
                              labels = c("Different Faith",
                                         "Same Faith")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::theme(
      legend.position = 'top',
      legend.text = ggplot2::element_text(size = 12),
      text = ggplot2::element_text(color = "#4E555B",
                                   family = 'sora'),
      strip.text.y.left = ggplot2::element_text(size = 12, "#4E555B", face = 'bold',
                                         angle = 0),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = '#fffcf9'),
      strip.background = ggplot2::element_rect(fill = '#fffcf9'),
      plot.background = ggplot2::element_rect(fill = '#fffcf9', color = "#4E555B"),
      plot.subtitle = ggplot2::element_text(size = 16, hjust = .5, vjust = 1,"#001021"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 20, hjust = .5, vjust = .2, color = "#001021", face = 'bold',
                                         family = 'bree'
      )
    ) +
    ggplot2::labs(
      title = "The percentage of people who remain in the faith they grew up in\nremains high, but is down compared to 1998.",
      subtitle = "\nThough this appears to have more-or-less stabilized since 2010.\n",
      caption = 'Data Source: General Social Survey Cumulative File\nAnalysis & visualization by Peter Licari (@PRLPoliSci)')
  
}


bayes_density_plot <- function(draws){
  
  sysfonts::font_add_google("Sora","sora")
  sysfonts::font_add_google("Bree Serif","bree")
  
  
  showtext::showtext_auto()
  
  draws |>
    ggplot2::ggplot() +
    ggplot2::aes(x = draw, y = forcats::fct_rev(as.factor(year)),
                 group = year,
                 fill = relig16_grouped) +
    ggdist::stat_slabinterval() +
    ggplot2::facet_wrap(~relig16_grouped, 
                         labeller = ggplot2::as_labeller(
                           c(`0`="Nones",`1`="Christians",`2`="Jews",`3`="Other")
                         )) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(1))+
    ggplot2::scale_fill_manual(values = c("#677db7","#f18f01","#5e8c61",
                                          "#a23b72")) +
    ggplot2::guides(fill = 'none') +
    ggplot2::xlab("\nPredicted Percentage Remaining in Religion")+
    ggplot2::ylab("") +
    ggplot2::labs(title = "Christians(broadly) and Jews have been the most likely to \nremain in the faith they were raised in.",
                  subtitle = "\nMost faith traditions have been pretty stable in their retention over time,\nChristianity, seeing an erosion since 1998, being the exception.",
                  caption = 'Distributions reflect posterior predictive distributions.\nBars reflect 95% and 67% credible intervals.\nData Source: General Social Survey Cumulative File\nAnalysis & visualization by Peter Licari (@PRLPoliSci)') + 
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(color = "#4E555B",
                                                family = 'sora'),
                   strip.text = ggplot2::element_text(size = 18, "#4E555B"),
                   panel.border = ggplot2::element_rect(color = "#4E555B",
                                                        fill = NA),
                   strip.background = ggplot2::element_rect(fill = '#fffcf9', color = "#4E555B"),
                   plot.background = ggplot2::element_rect(fill = '#fffcf9', color = "#4E555B"),
                   plot.subtitle = ggplot2::element_text(size = 20, hjust = .5, vjust = 1,"#001021"),
                   axis.title = ggplot2::element_text(size = 14),
                   axis.text = ggplot2::element_text(size = 12, color = "#4E555B"),
                   plot.title = ggplot2::element_text(size = 34, hjust = .5, vjust = .2, color = "#001021", face = 'bold',
                                                      family = 'bree'
                   ))
  
  
}
