library(readr)
library(tidyverse)
getwd()
data = read_csv("../data/lab_sodra.csv")
summary(data)
#1 uzduotis
new_data = dplyr::filter(data, ecoActCode == 479100)
View(new_data)
#grafikas
ggplot(data = new_data)+ theme_minimal() + 
geom_histogram(mapping = aes(x=avgWage), fill= "blue", col = "black", bins = 100) + 
ggtitle("Vidutinis atlyginimas") + xlab("avgWage") +  scale_x_continuous(labels = scales::comma_format(accuracy = 1))
#2 uzduotis
#Atrenkam menesius
# 5 imoniu parinkimas
top5 = new_data %>%
  group_by(name) %>%
  summarize(avgWage = mean(avgWage)) %>%
  top_n(5, avgWage) %>%
  pull(name)

# Filtravimas
new_data2 = new_data %>%
  filter(name %in% top5) %>%
  mutate(month_value = as.integer(substr(month, 5, 7)))

# Grafikas
ggplot(new_data2, aes(x = month_value, y = avgWage, group = name, colour = name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "month", breaks = 1:12, limits = c(1, 12)) +
  labs(title = "Vidutinis darbo uzmokestis", y = "avgWage")
#3
# Darbuotoju parinkimas
top5 = new_data2 %>%
  group_by(name) %>%
  summarize(numInsured = sum(numInsured)) %>%
  top_n(5, numInsured)

# Grafikas
ggplot(top5, aes(x = reorder(name, -numInsured), y = numInsured, fill = name)) +
  geom_col() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Number of Insured Employees", x = "Company", y = "Count")

