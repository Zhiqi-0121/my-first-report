#导入文件
pumpkins_data <- read.csv('\\\\wsl.localhost\\Ubuntu\\home\\jessicaye\\LIFE4138-coursework\\Pumpkins\\pumpkins_8.csv')
#step 1 Find the heaviest pumpkin: Determine the variety, origin and year of the heaviest pumpkin in all competitions.
library(dplyr)

#step 2 find the information
heaviest_pumpkin <- pumpkins_data %>%
  filter(weight_lbs == max(weight_lbs, na.rm = TRUE))

print(heaviest_pumpkin)

#step 3 change unit
# defind relationship between pound and kg
pounds_to_kilograms <- function(weight_in_pounds) {
  weight_in_kg <- weight_in_pounds * 0.45359237
  return(weight_in_kg)
}

# create new colunm
pumpkins_data <- pumpkins_data %>%
  mutate(weight_kg = pounds_to_kilograms(weight_lbs))

#step4 Plot the relationship between the estimated weight and actual weight

# create new column 'weight_class'
pumpkins_data <- pumpkins_data %>%
  mutate(weight_class = case_when(
    weight_kg < 500 ~ "Light",          # <500 light
    weight_kg >= 500 & weight_kg < 700 ~ "Medium",  # 500-700，medium
    weight_kg >= 700 ~ "Heavy",         # >700 heavy
    TRUE ~ NA_character_               # other situation
  ))
#step 5 print relationship plot
library(ggplot2)

plot <- ggplot(pumpkins_data, aes(x = weight_class, y = weight_kg, color = weight_class)) +
  geom_point(size = 3) + # Plot scatter
  labs(title = "Estimated vs Actual Weight of Pumpkins",
       x = "Estimated Weight (kg)",
       y = "Actual Weight (kg)",
       color = "Weight Class") + # set label and title
  scale_color_manual(values = c("Light" = "green", "Medium" = "orange", "Heavy" = "red"))  # colour
 

print(plot)

# Save the graphics to your computer
ggsave("estimated_vs_actual_weight.pdf", plot, width = 10, height = 8, dpi = 300)

#step 6 Filter out data for specified countries:"Canada", "USA", "UK"

filtered_data <- pumpkins_data %>%
  filter(country %in% c("Canada", "USA", "UK"))

# save as csv
write.csv(filtered_data, file = "filtered_data.csv", row.names = FALSE)

#step7 sata summary


# Calculate the average weight of pumpkins in each country
country_avg_weight <- pumpkins_data %>%
  group_by(country) %>%
  summarise(avg_weight = mean(weight_kg, na.rm = TRUE)) %>%
  arrange(desc(avg_weight))

# Find the countries with the highest average weight
highest_avg_weight_country <- country_avg_weight %>% slice(1)

print(country_avg_weight)
print(paste("high average weight country:", highest_avg_weight_country$country, "，hight avreage:", highest_avg_weight_country$avg_weight, "kg"))


# step 8 Box plotting
boxplot <- ggplot(filtered_data, aes(x = country, y = weight_kg, fill = country)) +
  geom_boxplot() + 
  labs(title = "Pumpkin weight distribution - Canada vs USA vs UK",
       x = "country",
       y = "weight (kg)",
       fill = "country") 


print(boxplot)

# save
ggsave("pumpkin_weight_distribution.pdf", plot = boxplot, width = 10, height = 8, dpi = 300)

#Step 9 Redraw your plot

facet_boxplot <- ggplot(filtered_data, aes(x = country, y = weight_kg, fill = country)) +
  geom_boxplot() + 
  labs(title = "Pumpkin weight distribution - Canada vs USA vs UK",
       x = "country",
       y = "weight(kg)",
       fill = "countrt") +
  facet_wrap(~ country, scales = "free_y")  # Each pumpkin variety is shown as an independent subgraph
 
# 显示图形
print(facet_boxplot)

# 将图形保存到电脑上
ggsave("pumpkin_weight_distribution_by_country.pdf", plot = facet_boxplot, width = 15, height = 10, dpi = 300)

getwd()
setwd("D:/Desktop")
