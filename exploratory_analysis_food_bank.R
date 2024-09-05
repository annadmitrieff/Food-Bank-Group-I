library(readxl)
library(dplyr)
library(ggplot2)

names = c("Banks County, Georgia", 
          "Barrow County, Georgia", 
          "Clarke County, Georgia",
          "Elbert County, Georgia",
          "Franklin County, Georgia",
          "Habersham County, Georgia",
          "Hart County, Georgia",
          "Jackson County, Georgia",
          "Madison County, Georgia",
          "Oconee County, Georgia",
          "Oglethorpe County, Georgia",
          "Rabun County, Georgia",
          "Stephens County, Georgia",
          "Towns County, Georgia",
          "White County, Georgia")

mtmg_xlsx = read_excel("./data/Map_The_Meal_Gap.xlsx")
federal_codes = read.table("./data/FederalCodes_GA.txt", 
                           sep = "|", 
                           header = TRUE,
                           quote = "")

mtmg_county = read_excel("./data/Map_The_Meal_Gap.xlsx", sheet = "County")
glimpse(mtmg_county)

mtmg_service_region = mtmg_county %>%
  filter((`Member 1 ID` == 324 | FIPS == 13105)) %>%
  select(!contains(c("Ratio", "Member", "Census", "Continuum"))) %>%
  group_by(`County, State`, Year)

georgia_federal_data = federal_codes %>%
  filter(census_class_code %in% c("H1", "H6")) %>%
  arrange(county_name) %>%
  mutate(FIPS = 13000 + as.numeric(county_numeric))

combined_data = mtmg_county %>% 
  filter(Year == 2021) %>%
  left_join(georgia_federal_data, by = "FIPS")

plot_data = function()
{
  ggplot(mtmg_service_region, aes(factor(Year), `Overall Food Insecurity Rate`, fill = factor(Year))) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.1) +
    facet_wrap(~ `County, State`)
  
  ggplot(combined_data, aes(prim_long_dec, prim_lat_dec, color = `Median Income (5 Yr ACS)`, size = `Total Population (5 Year ACS)`)) + 
    geom_point() + 
    coord_fixed() 
  
  for (i in 3:3)
  {
    plot = ggplot(S0101_data %>% group_by(Age), aes(factor(Year), get(names(S0101_data)[i]), group = factor(Age))) +
      geom_point() + 
      geom_line() + 
      ggtitle(names(S0101_data)[i]) +
      xlab("Year") +
      ylab("Population Count") + 
      facet_wrap(~ Age)
    print(plot)
  }
  
  for (i in 3:3)
  {
    plot = ggplot(S1501_data %>% group_by(Education), aes(factor(Year), get(names(S1501_data)[i]), group = factor(Education))) +
      geom_point() + 
      geom_line() + 
      ggtitle(names(S0101_data)[i]) +
      xlab("Year") +
      ylab("Population Count") + 
      facet_wrap(~ Education)
    print(plot)
  }
  
  for (i in 3:17)
  {
    snap = data.frame(Poverty = "Population participating in SNAP", SNAP_data)
    colnames(snap) = c("Poverty", "Year", names)
    plot = ggplot(S1701_data %>% rbind(snap) %>% group_by(Poverty), aes(factor(Year), get(names(S1501_data)[i]), group = factor(Poverty), color = factor(Poverty))) +
      geom_point() + 
      geom_line() + 
      ggtitle(names(S1701_data)[i]) +
      expand_limits(y = 0) +
      xlab("Year") +
      ylab("Population Count")
    print(plot)
  }
}