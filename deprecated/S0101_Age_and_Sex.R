library(readxl)
library(dplyr)

S0101_folder = list.files("./data/S0101_Age_and_Sex", pattern = "^ACSS", full.names = TRUE)
S0101_data = NULL

col_range = 5:22 # one less than the spreadsheet's index

for (i in 0:6)
{
  temp_sheet = read_excel(S0101_folder[i + 1], sheet = "Data")
  temp_data = data.frame(Age = unlist(temp_sheet[col_range, 1], use.names = FALSE), 
                         Year = rep(2010 + i)) 
  for (j in 1:15)
  {
    total_pop = as.numeric(gsub(",", "", x = temp_sheet[3, 2 + 6 * (j - 1)]))
    percent_pop = unlist(temp_sheet[col_range, 2 + 6 * (j - 1)], use.names = FALSE)
    percent_pop = gsub("%", "", x = percent_pop)
    percent_pop = as.numeric(percent_pop)
    
    pop = data.frame(round(percent_pop * total_pop / 100, digits = 0))
    colnames(pop) = colnames(temp_sheet)[2 + 6 * (j - 1)]
    
    temp_data = temp_data %>%
      cbind(pop)
  }
  S0101_data = S0101_data %>% rbind(temp_data)
}

for (i in 7:12)
{
  temp_sheet = read_excel(S0101_folder[i + 1], sheet = "Data")
  temp_data = data.frame(Age = unlist(temp_sheet[col_range, 1], use.names = FALSE), 
                         Year = rep(2010 + i))
  for (j in 1:15)
  {
    pop = unlist(temp_sheet[col_range, 2 + 12 * (j - 1)], use.names = FALSE)
    pop = gsub(",", "", x = pop)
    pop = as.numeric(pop)
    pop = data.frame(pop)
    
    colnames(pop) = colnames(temp_sheet)[2 + 12 * (j - 1)]
    
    temp_data = temp_data %>% 
      cbind(pop)
  }
  S0101_data = S0101_data %>% rbind(temp_data)
}