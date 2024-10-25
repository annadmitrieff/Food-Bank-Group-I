library(dplyr)
library(gganimate)
library(ggplot2)
library(tidyr)
library(sf)

# plot = gather(pop_data, "Legend", "value", rownames(correlation), length(pop_data)) %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend)) +
#   geom_point() + 
#   geom_line() + 
#   xlab("Year") +
#   ylab("Population Count") +
#   ggtitle("Positive Correlation")
# print(plot)

# plot = gather(pop_data, "Legend", "value", rownames(positive_correlation), length(pop_data)) %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend)) +
#   geom_point() + 
#   geom_line() + 
#   xlab("Year") +
#   ylab("Population Count") +
#   ggtitle("Positive Correlation")
# print(plot)

name = names[3]
#pop_data = exclude_percentages(get_all())
#all_cor = cor(pop_data)

if (FALSE)
{ 
  prop_data$Label = factor(prop_data$Label, unique(prop_data$Label))
  
  plot = prop_data %>% 
    filter(Year == 2022
           & Category %in% c("Positive", "All")
           & Label != "% of population food insecure") %>% 
    ggplot(aes(x = `County, State`, y = Percent, fill = Label)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_x_discrete(labels = c("Banks", "Barrow", "Clarke", "Elbert", "Franklin", "Habersham", "Hart", "Jackson", "Madison", "Oconee", "Oglethorpe", "Rabun", "Stephens", "Towns", "White", "Service Region")) +
    #scale_y_continuous(limits = c(0, 0.4)) +
    facet_wrap(~Label, scale = "free") + 
    theme(axis.text.x = element_text(angle = 270, vjust = 0, hjust=0), legend.position = "none",
          axis.title = element_blank()) +
    ggtitle("SNAP Participation and Positively Correlated Factors, 2022")
  print(plot)
  
  plot = prop_data %>% 
    filter(Year == 2022
           & Category %in% c("Negative", "All")
           & Label != "% of population food insecure") %>% 
    ggplot(aes(x = `County, State`, y = Percent, fill = Label)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_x_discrete(labels = c("Banks", "Barrow", "Clarke", "Elbert", "Franklin", "Habersham", "Hart", "Jackson", "Madison", "Oconee", "Oglethorpe", "Rabun", "Stephens", "Towns", "White", "Service Region")) +
    #scale_y_continuous(limits = c(0, 0.4)) +
    facet_wrap(~Label, scale = "free") + 
    theme(axis.text.x = element_text(angle = 270, vjust = 0, hjust=0), legend.position = "none",
          axis.title = element_blank()) +
    ggtitle("SNAP Participation and Negatively Correlated Factors, 2022")
  print(plot)
  
  plot = prop_data %>% 
    filter(Year == 2022
           & Category %in% c("Race / Ethnicity", "All")
           & Label != "% of population food insecure") %>% 
    ggplot(aes(x = `County, State`, y = Percent, fill = Label)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_x_discrete(labels = c("Banks", "Barrow", "Clarke", "Elbert", "Franklin", "Habersham", "Hart", "Jackson", "Madison", "Oconee", "Oglethorpe", "Rabun", "Stephens", "Towns", "White", "Service Region")) +
    #scale_y_continuous(limits = c(0, 0.4)) +
    facet_wrap(~Label, scale = "free") + 
    theme(axis.text.x = element_text(angle = 270, vjust = 0, hjust=0), legend.position = "none",
          axis.title = element_blank()) +
    ggtitle("SNAP Participation and Race / Ethnicity Factors, 2022")
  print(plot)
}

if (TRUE)
{
  
  geo_data = read_sf("./data/georgia-counties.json")
  geo_data = geo_data %>% filter(NAME %in% c("Banks", 
                                             "Barrow", 
                                             "Clarke", 
                                             "Elbert", 
                                             "Franklin", 
                                             "Habersham", 
                                             "Hart", 
                                             "Jackson", 
                                             "Madison", 
                                             "Oconee",
                                             "Oglethorpe",
                                             "Rabun",
                                             "Stephens",
                                             "Towns",
                                             "White"))
  
  for (i in 2012:2022)
  {
    temp_data = prop_data %>%
      mutate(County = sub(",.*", "", `County, State`)) %>% 
      filter(Label == "Average meal cost in dollars" & !County == "Service Region")
    
    temp_data = left_join(geo_data, temp_data, by = c("NAMELSAD" = "County"))
    
    temp_data = temp_data %>% filter(Year == i)
    
    map = ggplot(temp_data) +
      geom_sf(aes(fill = Percent, geometry = geometry)) +
      geom_sf_text(aes(label = NAME), color = "white") +
      ggtitle(paste("Average Meal Cost by County,", i, sep = " ")) + 
      theme_bw() +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      scale_fill_viridis_c(
        limits = c(2.5, 4.5),
        breaks = c(2.5, 3, 3.5, 4, 4.5),
        name = "Meal Cost in Dollars",
        guide = guide_legend(
          label.position = "bottom",
          title.position = "top",
          nrow = 1
        )
      )
    ggsave(paste(paste("./images/meal_cost_choropleth_", i, sep = " "), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  }
}



