library(readxl)
library(dplyr)

S1701_folder = list.files("./data/S1701_Poverty_Status", pattern = "^ACSS", full.names = TRUE)
S1701_data = NULL

for (i in 2:4)
{
  temp_sheet = read_excel(S1701_folder[i - 1], sheet = "Data")
  temp_data = data.frame(c("Population under 100% of poverty line", 
                           "Population under 125% of poverty line"),
                         rep(2010 + i))
  colnames(temp_data) = c("Poverty", "Year")
  
  for (j in 1:15)
  {
    pop_100 = temp_sheet[3, 4 + 6 * (j - 1)]
    pop_100 = gsub(",", "", x = pop_100)
    pop_100 = as.numeric(pop_100)
    
    pop_125 = temp_sheet[44, 2 + 6 * (j - 1)]
    pop_125 = gsub(",", "", x = pop_125)
    pop_125 = as.numeric(pop_125)
    
    pop = data.frame(c(pop_100, pop_125))
    colnames(pop) = colnames(temp_sheet)[2 + 6 * (j - 1)]
    
    temp_data = temp_data %>%
      cbind(pop)
  }
  S1701_data = S1701_data %>% rbind(temp_data)
}

for (i in 5:12)
{
  temp_sheet = read_excel(S1701_folder[i - 1], sheet = "Data")
  temp_data = data.frame(c("Population under 100% of poverty line", 
                           "Population under 125% of poverty line"),
                         rep(2010 + i))
  colnames(temp_data) = c("Poverty", "Year")
  
  for (j in 1:15)
  {
    pop_100 = temp_sheet[3, 4 + 6 * (j - 1)]
    pop_100 = gsub(",", "", x = pop_100)
    pop_100 = as.numeric(pop_100)
    
    pop_125 = temp_sheet[48, 2 + 6 * (j - 1)]
    pop_125 = gsub(",", "", x = pop_125)
    pop_125 = as.numeric(pop_125)
    
    pop = data.frame(c(pop_100, pop_125))
    colnames(pop) = colnames(temp_sheet)[2 + 6 * (j - 1)]
    
    temp_data = temp_data %>%
      cbind(pop)
  }
  S1701_data = S1701_data %>% rbind(temp_data)
}