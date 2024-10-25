library(readxl)
library(dplyr)

S1701_folder = list.files("./data/S1701_Poverty_Status", pattern = "^ACSS", full.names = TRUE)
S1701_data = NULL

for (i in 2:4)
{
  temp_sheet = read_excel(S1701_folder[i - 1], sheet = "Data")
  temp_data = data.frame(c("Under 50% of poverty line",
                           "Under 100% of poverty line", 
                           "Under 125% of poverty line",
                           "Civilian labor force 16 years or older",
                           "Unemployed civilian labor force",
                           "Civilian labor force under poverty line",
                           "Unemployed civilian labor force under poverty line",
                           "Not in labor force"),
                         rep(2010 + i))
  colnames(temp_data) = c("Poverty", "Year")
  
  for (j in 1:15)
  {
    pop_50 = temp_sheet[43, 2 + 6 * (j - 1)]
    pop_50 = gsub(",", "", x = pop_50)
    pop_50 = as.numeric(pop_50)
    
    pop_100 = temp_sheet[3, 4 + 6 * (j - 1)]
    pop_100 = gsub(",", "", x = pop_100)
    pop_100 = as.numeric(pop_100)
    
    pop_125 = temp_sheet[44, 2 + 6 * (j - 1)]
    pop_125 = gsub(",", "", x = pop_125)
    pop_125 = as.numeric(pop_125)
    
    pop_civ = temp_sheet[30, 2 + 6 * (j - 1)]
    pop_civ = gsub(",", "", x = pop_civ)
    pop_civ = as.numeric(pop_civ)
    
    pop_civ_unemp = temp_sheet[34, 2 + 6 * (j - 1)]
    pop_civ_unemp = gsub(",", "", x = pop_civ_unemp)
    pop_civ_unemp = as.numeric(pop_civ_unemp)
    
    pop_100_civ = temp_sheet[31, 4 + 6 * (j - 1)]
    pop_100_civ = gsub(",", "", x = pop_100_civ)
    pop_100_civ = as.numeric(pop_100_civ)
    
    pop_100_civ_unemp = temp_sheet[34, 4 + 6 * (j - 1)]
    pop_100_civ_unemp = gsub(",", "", x = pop_100_civ_unemp)
    pop_100_civ_unemp = as.numeric(pop_100_civ_unemp)
    
    pop_not_civ = temp_sheet[38, 2 + 6 * (j - 1)]
    pop_not_civ = gsub(",", "", x = pop_not_civ)
    pop_not_civ = as.numeric(pop_not_civ)
    pop_not_civ = pop_not_civ - pop_civ
    
    pop = data.frame(c(pop_50, 
                       pop_100, 
                       pop_125, 
                       pop_civ, 
                       pop_civ_unemp, 
                       pop_100_civ, 
                       pop_100_civ_unemp, 
                       pop_not_civ))
    
    colnames(pop) = colnames(temp_sheet)[2 + 6 * (j - 1)]
    
    temp_data = temp_data %>%
      cbind(pop)
  }
  S1701_data = S1701_data %>% rbind(temp_data)
}

for (i in 5:12)
{
  temp_sheet = read_excel(S1701_folder[i - 1], sheet = "Data")
  temp_data = data.frame(c("Under 50% of poverty line",
                           "Under 100% of poverty line", 
                           "Under 125% of poverty line",
                           "Civilian labor force 16 years or older",
                           "Unemployed civilian labor force",
                           "Civilian labor force under poverty line",
                           "Unemployed civilian labor force under poverty line",
                           "Not in labor force"),
                         rep(2010 + i))
  colnames(temp_data) = c("Poverty", "Year")
  
  for (j in 1:15)
  {
    pop_50 = temp_sheet[47, 2 + 6 * (j - 1)]
    pop_50 = gsub(",", "", x = pop_50)
    pop_50 = as.numeric(pop_50)
    
    pop_100 = temp_sheet[3, 4 + 6 * (j - 1)]
    pop_100 = gsub(",", "", x = pop_100)
    pop_100 = as.numeric(pop_100)
    
    pop_125 = temp_sheet[48, 2 + 6 * (j - 1)]
    pop_125 = gsub(",", "", x = pop_125)
    pop_125 = as.numeric(pop_125)
    
    pop_civ = temp_sheet[34, 2 + 6 * (j - 1)]
    pop_civ = gsub(",", "", x = pop_civ)
    pop_civ = as.numeric(pop_civ)
    
    pop_civ_unemp = temp_sheet[38, 2 + 6 * (j - 1)]
    pop_civ_unemp = gsub(",", "", x = pop_civ_unemp)
    pop_civ_unemp = as.numeric(pop_civ_unemp)
    
    pop_100_civ = temp_sheet[34, 4 + 6 * (j - 1)]
    pop_100_civ = gsub(",", "", x = pop_100_civ)
    pop_100_civ = as.numeric(pop_100_civ)
    
    pop_100_civ_unemp = temp_sheet[38, 4 + 6 * (j - 1)]
    pop_100_civ_unemp = gsub(",", "", x = pop_100_civ_unemp)
    pop_100_civ_unemp = as.numeric(pop_100_civ_unemp)
    
    pop_not_civ = temp_sheet[42, 2 + 6 * (j - 1)]
    pop_not_civ = gsub(",", "", x = pop_not_civ)
    pop_not_civ = as.numeric(pop_not_civ)
    pop_not_civ = pop_not_civ - pop_civ
    
    pop = data.frame(c(pop_50, 
                       pop_100, 
                       pop_125, 
                       pop_civ, 
                       pop_civ_unemp, 
                       pop_100_civ, 
                       pop_100_civ_unemp, 
                       pop_not_civ))
    colnames(pop) = colnames(temp_sheet)[2 + 6 * (j - 1)]
    
    temp_data = temp_data %>%
      cbind(pop)
  }
  S1701_data = S1701_data %>% rbind(temp_data)
}