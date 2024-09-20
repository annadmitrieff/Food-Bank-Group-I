library(dplyr)
library(ggplot2)
library(tidyr)

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

plot_data = gather(pop_data, "Legend", "value", head(rownames(positive_correlation), n = 3), length(pop_data))
plot_data$Legend = factor(plot_data$Legend, levels = unique(plot_data$Legend))

print(plot_data)

plot = plot_data %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend, alpha = Legend)) +
  geom_point() + 
  geom_line() + 
  scale_alpha_manual(values = c(.2, .2, 1, 1)) +
  xlab("Year") +
  ylab("Population Count") +
  ggtitle("Characteristics positively correlated with SNAP participation")
print(plot)

# plot = gather(pop_data, "Legend", "value", rownames(negative_correlation), length(pop_data)) %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend)) +
#   geom_point() + 
#   geom_line() + 
#   xlab("Year") +
#   ylab("Population Count") +
#   ggtitle("Negative Correlation")
# print(plot)

plot_data = gather(pop_data, "Legend", "value", head(rownames(negative_correlation), n = 3), length(pop_data))
plot_data$Legend = factor(plot_data$Legend, levels = unique(plot_data$Legend))

plot = plot_data %>% ggplot(aes(factor(Year), value, group = Legend, color = Legend, alpha = Legend)) +
  geom_point() + 
  geom_line() + 
  scale_alpha_manual(values = c(.2, .2, 1, 1)) +
  xlab("Year") +
  ylab("Population Count") +
  ggtitle("Characteristics negatively correlated with SNAP participation")
print(plot)
