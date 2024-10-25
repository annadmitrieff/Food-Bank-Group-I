library(readxl)
library(dplyr)

S1501_folder = list.files("./data/S1501_Educational_Attainment", pattern = "^ACSS", full.names = TRUE)
S1501_data = NULL

for (i in 0:4)
{
  temp_sheet = read_excel(S1501_folder[i + 1], sheet = "Data")
  temp_data = data.frame(Education = unlist(temp_sheet[9:15, 1], use.names = FALSE), 
                         Year = rep(2010 + i)) 
  for (j in 1:15)
  {
    total_pop = as.numeric(gsub(",", "", x = temp_sheet[8, 2 + 6 * (j - 1)]))
    percent_pop = unlist(temp_sheet[9:15, 2 + 6 * (j - 1)], use.names = FALSE)
    percent_pop = gsub("%", "", x = percent_pop)
    percent_pop = as.numeric(percent_pop)
    
    pop = data.frame(round(percent_pop * total_pop / 100, digits = 0))
    colnames(pop) = colnames(temp_sheet)[2 + 6 * (j - 1)]
    
    temp_data = temp_data %>%
      cbind(pop)
  }
  S1501_data = S1501_data %>% rbind(temp_data)
}

for (i in 5:12)
{
  temp_sheet = read_excel(S1501_folder[i + 1], sheet = "Data")
  temp_data = data.frame(Education = unlist(temp_sheet[10:16, 1], use.names = FALSE), 
                         Year = rep(2010 + i))
  for (j in 1:15)
  {
    pop = unlist(temp_sheet[10:16, 2 + 12 * (j - 1)], use.names = FALSE)
    pop = gsub(",", "", x = pop)
    pop = as.numeric(pop)
    pop = data.frame(pop)
    
    colnames(pop) = colnames(temp_sheet)[2 + 12 * (j - 1)]
    
    temp_data = temp_data %>% 
      cbind(pop)
  }
  
  S1501_data = S1501_data %>% rbind(temp_data)
}