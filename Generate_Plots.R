library(dplyr)
library(gganimate)
library(ggplot2)
library(tidyr)
library(sf)
library(gifski)
library(plotly)

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

fixer = function(gp) 
{
  lapply(1:length(gp$x$data), function(m) 
  {
    gp$x$data[[m]]$hoveron <<- "fills" # hover on the fill, not line
    if(length(gp$x$data[[m]]$text > 1)) {
      gp$x$data[[m]]$text <<- gp$x$data[[m]]$text[1] # only one tooltip per county
    }
  })
  gp
}

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

if (FALSE)
{
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

if (FALSE)
{
  temp_data = prop_data %>%
    mutate(County = sub(",.*", "", `County, State`)) %>% 
    mutate(Tooltip = paste(paste(paste("Average meal cost in ", County), ": "), Percent)) %>%
    filter(Label == "Average meal cost in dollars" & !County == "Service Region")
  
  temp_data = left_join(geo_data, temp_data, by = c("NAMELSAD" = "County"))
  
  map = ggplot(temp_data) +
    geom_sf(aes(fill = Percent, geometry = geometry, frame = Year, group = NAME)) +
    #geom_sf_text(aes(label = NAME), color = "white") +
    labs(title = 'Average Meal Cost by County, 2012-2022') + 
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
      name = "Meal Cost in Dollars"
    ) #+
    #transition_manual(as.numeric(Year))
  #animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
  ggplotly(map) %>% 
    animation_opts(transition = 0) %>% fixer()
  
}

generate_choropleth = function(geo, data, label, title, legend, year = 2022)
{
  temp_data = data %>%
    mutate(County = sub(",.*", "", `County, State`)) %>% 
    filter(Label == label & !County == "Service Region")
  
  temp_data = left_join(geo, temp_data, by = c("NAMELSAD" = "County"))
  
  i = year
  
  temp_data = temp_data %>% filter(Year == i)
  
  map = ggplot(temp_data) +
    geom_sf(aes(fill = Percent, geometry = geometry)) +
    geom_sf_text(aes(label = NAME), color = "white") +
    ggtitle(paste(title, i, sep = ", ")) + 
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
      name = legend,
      limits = c(min(temp_data$Percent) - 0.01, max(temp_data$Percent) + 0.01),
      breaks = c(round(min(temp_data$Percent), 2),
                 round((max(temp_data$Percent) - min(temp_data$Percent)) / 4 + min(temp_data$Percent), 2),
                 round((max(temp_data$Percent) - min(temp_data$Percent)) * 2 / 4 + min(temp_data$Percent), 2),
                 round((max(temp_data$Percent) - min(temp_data$Percent)) * 3 / 4 + min(temp_data$Percent), 2),
                 round(max(temp_data$Percent), 2)),
      guide = guide_legend(
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    )
  
  return(map)
}

generate_animated_choropleth = function(geo, data, label, title, legend)
{
  temp_data = data %>%
    mutate(County = sub(",.*", "", `County, State`)) %>% 
    filter(Label == label & !County == "Service Region")
  
  temp_data = left_join(geo, temp_data, by = c("NAMELSAD" = "County"))
  
  map = ggplot(temp_data) +
    geom_sf(aes(fill = Percent, geometry = geometry)) +
    geom_sf_text(aes(label = NAME), color = "white") +
    ggtitle(paste(title, "2012 to 2022", sep = ", ")) + 
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
      name = legend,
      limits = c(min(temp_data$Percent) - 0.01, max(temp_data$Percent) + 0.01),
      breaks = c(round(min(temp_data$Percent), 2),
                 round((max(temp_data$Percent) - min(temp_data$Percent)) / 4 + min(temp_data$Percent), 2),
                 round((max(temp_data$Percent) - min(temp_data$Percent)) * 2 / 4 + min(temp_data$Percent), 2),
                 round((max(temp_data$Percent) - min(temp_data$Percent)) * 3 / 4 + min(temp_data$Percent), 2),
                 round(max(temp_data$Percent), 2)),
      guide = guide_legend(
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    ) + 
    transition_manual(as.numeric(Year))
  
  return(map)
}

i = 2022

if (FALSE)
{
  map = generate_choropleth(geo_data, prop_data, "% of population unemployed", "Unemployment by County", "% of population")
  ggsave(paste(paste("./images/unemployment_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population that are veterans", "Veterans by County", "% of population")
  ggsave(paste(paste("./images/veterans_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population in kindergarten", "Kindergarteners by County", "% of population")
  ggsave(paste(paste("./images/kindergarten_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population under 100 percent of poverty level", "Poverty Rate by County", "% of population")
  ggsave(paste(paste("./images/poverty_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population in college or graduate school", "College / Graduate Students by County", "% of population")
  ggsave(paste(paste("./images/college_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  
  map = generate_choropleth(geo_data, prop_data, "% of population over 65 years", "Seniors by County", "% of population")
  ggsave(paste(paste("./images/seniors_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population with a disability", "Disability by County", "% of population")
  ggsave(paste(paste("./images/disability_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population divorced", "Individuals Currently Divorced by County", "% of population")
  ggsave(paste(paste("./images/divorce_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population in high school", "High Schoolers by County", "% of population")
  ggsave(paste(paste("./images/highschool_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population over 25 with only a high school diploma", "Individuals Over 25 With Only a High School Diploma by County", "% of population")
  ggsave(paste(paste("./images/diploma_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  
  map = generate_choropleth(geo_data, prop_data, "Average meal cost in dollars", "Average Meal Cost by County", "Meal cost in dollars")
  ggsave(paste(paste("./images/meal_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "Median earnings in dollars", "Median Earnings by County", "Median income in dollars")
  ggsave(paste(paste("./images/income_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4)
  map = generate_choropleth(geo_data, prop_data, "% of population participating in SNAP", "SNAP Participation by County", "% of population")
  ggsave(paste(paste("./images/snap_choropleth_", i, sep = ""), ".png", sep = ""), map, units = "in", width = 9.6, height = 5.4) 
}




map = generate_animated_choropleth(geo_data, prop_data, "% of population unemployed", "Unemployment by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/unemployed_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population that are veterans", "Veterans by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/veterans_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population in kindergarten", "Kindergarteners by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/kindergarten_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population under 100 percent of poverty level", "Poverty Rate by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/poverty_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population in college or graduate school", "College / Graduate Students by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/college_animated.gif", animation = last_animation())

map = generate_animated_choropleth(geo_data, prop_data, "% of population over 65 years", "Seniors by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/seniors_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population with a disability", "Disability by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/disability_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population divorced", "Individuals Currently Divorced by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/divorced_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population in high school", "High Schoolers by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/high_school_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population over 25 with only a high school diploma", "Individuals Over 25 With Only a High School Diploma by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)

map = generate_animated_choropleth(geo_data, prop_data, "Average meal cost in dollars", "Average Meal Cost by County", "Meal cost in dollars")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/meal_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "Median earnings in dollars", "Median Earnings by County", "Median income in dollars")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/income_animated.gif", animation = last_animation())
map = generate_animated_choropleth(geo_data, prop_data, "% of population participating in SNAP", "SNAP Participation by County", "% of population")
animate(map, renderer = gifski_renderer(), units = "px", width = 640, height = 360)
anim_save("./images/snap_animated.gif", animation = last_animation())