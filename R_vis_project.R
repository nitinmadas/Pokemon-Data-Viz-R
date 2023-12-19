
# Installing and importing req. lib and Data

install.packages(c('tidyr', 'dplyr'  ,'ggplot2'))

library(ggplot2)
library(dplyr)
library(tidyr)
  pokemon_data = read.csv(file.choose())
  
View(pokemon_data)


pokemon_data = read.csv('D:/R analysis project/DAta/Pokemon.csv')

head(df)

par(mfrow = c(3,2))
  
# Plot 1: BAR PLOT: Pokemon Type Distribution

# Grouping data according to Type 1 and finding Count
type_wise_data <- pokemon_data %>%
  group_by(Type.1) %>%
  summarise(count = n()) 

ggplot(type_wise_data, aes(x = reorder(Type.1, count), y = count)) + 
  geom_bar(stat= "identity",fill = "lightblue",color = "black") +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Distribution of Pokemon Types (Type 1)",
       x = "Primay Type of Pokemon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),   # Remove y-axis ticks and text
        axis.ticks.y = element_blank(),  
        plot.title=element_text(hjust=.45), # Center Title 
        panel.grid.major = element_blank(), # Remove grid lines
        panel.grid.minor = element_blank())



# Plot 2: HIST PLOT:  HP Distribution

ggplot(pokemon_data, aes(x = HP)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
   labs(title = "Distribution of HP (Health Points)",
       x = "HP (Health Points)",
       y = "Frequency") +
  theme(plot.title=element_text(hjust=.45))


# Plot 3: Box Plot: Legendary vs Total Stats
ggplot(pokemon_data, aes(x = Legendary, y = Total, fill = Legendary)) +
  geom_boxplot() +
  labs(title = "Box Plot: Legendary vs Total Stats",
       x = "Legendary",
       y = "Total Stats") +
  theme_minimal() + 
  theme(plot.title=element_text(hjust=.45))

# Plot 4 : Scatter Plot: Attack vs Defense
ggplot(pokemon_data, aes(x = Attack, y = Defense)) +
  geom_point() +
  labs(title = "Scatter Plot: Attack vs Defense",
       x = "Attack",
       y = "Defense") +
  theme_minimal()

type_counts <- table(pokemon_data$Type.1)



selected_types <- c("Water", "Normal")
filtered_data <- pokemon_data %>%
  filter(Type.1 %in% selected_types)



# Plot 4: RADAR PLOT
selected_columns <- c("Type.1", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")
selected_data <- pokemon_data %>%
  filter(Type.1 %in% c("Water", "Fire", "Bug")) %>%
  select(all_of(selected_columns))

# Calculate average stats for each Pokemon type
avg_stats <- selected_data %>%
  group_by(Type.1) %>%
  summarise_all(mean)

# Melt the data for the radar chart
melted_data <- gather(avg_stats, key = "Stat", value = "Value", -Type.1)

# Radar Chart: Average Stats of Water and Fire Pokemon
ggplot(melted_data, aes(x = Stat, y = Value, group = Type.1, color = Type.1)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Radar Chart: Average Stats of Water, Fire and Bug Pokemon",
       x = "Stats",
       y = "Average") +
  theme_minimal() +
  theme(legend.position = "bottom") +  ylim(0, NA)


df = pokemon_data %>% filter(Legendary == "True")
View(df)
