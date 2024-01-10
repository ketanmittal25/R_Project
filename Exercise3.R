# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
load("ESS10FR.rdata")

# Extract the numeric decile from the 'hinctnta' variable
# This assumes the letters indicating deciles (e.g., J, R, C, etc.) are consistent and each letter corresponds to a unique decile.
# Create a named vector that maps these letters to their respective numeric deciles
decile_map <- setNames(1:10, c("J", "R", "C", "M", "F", "S", "K", "P", "D", "H"))

# Use string manipulation to extract the letter and then map it to the numeric decile
ESS10FR$hinctnta_numeric <- sapply(sub(" - .*", "", ESS10FR$hinctnta), 
                                   function(x) decile_map[x])

# Recode 'hinctnta_numeric' into broader income classes
ESS10FR$hinctnta_recode <- cut(
  ESS10FR$hinctnta_numeric, 
  breaks = c(0, 3.5, 7.5, 10), 
  labels = c("Faible revenu", "Revenu intermédiaire", "Revenu élevé"),
  include.lowest = TRUE
)

# Check the distribution of the new recoded variable
table(ESS10FR$hinctnta_recode)


# Create a box plot
ggplot(ESS10FR, aes(x = hinctnta_recode, y = wkhtot, fill = gndr)) + 
  geom_boxplot() +
  labs(title = "Relation entre la classe de revenu, le sexe et les heures travaillées",
       x = "Classe de revenu",
       y = "Heures travaillées par semaine") +
  scale_fill_brewer(palette = "Set1", name = "Sexe") +
  theme_minimal()

# Creating a scatter plot with facet wrap
ggplot(ESS10FR, aes(x = hinctnta_recode, y = wkhtot, color = gndr)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~hinctnta_recode) +
  labs(title = "Relation entre la classe de revenu, le sexe et les heures travaillées",
       x = "Classe de revenu",
       y = "Heures travaillées par semaine") +
  scale_color_brewer(palette = "Set1", name = "Sexe") +
  theme_minimal()

