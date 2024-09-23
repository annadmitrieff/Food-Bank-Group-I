library(readxl)
library(dplyr)
library(stringr)

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

SNAP_folder = list.files("./data/SNAP_Participation", pattern = "^J", full.names = TRUE)
SNAP_data = NULL

for (i in 0:12)
{
  temp_sheet = read_excel(SNAP_folder[i + 1], skip = 3)
  str = str_split_fixed(temp_sheet$`Substate/Region`, " ", n = 7)[,1:4]
  temp_sheet[c("FIPS", "State", "EBT", "County")] = str
  temp_data = temp_sheet[c("State", "County", "Calc: SNAP Total PA and Non-PA Households")]
  temp_data = temp_data %>% 
    filter(State == "GA" 
         & County %in% c("BANKS",
                         "BARROW",
                         "CLARKE", 
                         "ELBERT",
                         "FRANKLIN",
                         "HABERSHAM",
                         "HART",
                         "JACKSON",
                         "MADISON",
                         "OCONEE",
                         "OGLETHORPE",
                         "RABUN",
                         "STEPHENS",
                         "TOWNS",
                         "WHITE"))
  
  pop = data.frame(Year = rep(2010 + i), t(temp_data$`Calc: SNAP Total PA and Non-PA Households`))
  colnames(pop) = c("Year", names)
  
  SNAP_data = SNAP_data %>% rbind(pop)
}

SNAP_data = SNAP_data %>% mutate(Label = rep("Calc: SNAP Total PA and Non-PA Households"))
SNAP_data = SNAP_data[c("Label", "Year", names)]