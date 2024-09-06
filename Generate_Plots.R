library(dplyr)
library(ggplot2)
library(tidyr)

plot = gather(pop_data, "Legend", "value", c("Population under 100% of poverty line", "Under 5 years", "40 to 44 years", 29)) %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend)) +
  geom_point() + 
  geom_line() + 
  xlab("Year") +
  ylab("Population Count") +
  ggtitle("Positive Correlation")
print(plot)

plot = gather(pop_data, "Legend", "value", c("65 to 69 years", "9th to 12th grade, no diploma", "75 to 79 years", 29)) %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend)) +
  geom_point() + 
  geom_line() + 
  xlab("Year") +
  ylab("Population Count") +
  ggtitle("Negative Correlation")
print(plot)

i = 29
plot = pop_data %>% cbind(Total = c(18381, 18061, 18900, 18708, 18510, 18363, 18336, 18336, 18317, 18333, 18250)) %>% gather("Legend", "value", c(30)) %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend)) +
  geom_point() + 
  geom_line() + 
  xlab("Year") +
  ylab("Population Count") +
  ggtitle("Total Population")
print(plot)