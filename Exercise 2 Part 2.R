# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
load("ESS10FR.rdata")

# Recode 'jbprtfp' into three categories: 'Low Conflict', 'Moderate Conflict', and 'High Conflict'
ESS10FR$jbprtfp_recode_3cat <- factor(
  case_when(
    ESS10FR$jbprtfp %in% c("Never", "Hardly Ever") ~ "faible conflit",
    ESS10FR$jbprtfp == "Sometimes" ~ "conflit modéré",
    ESS10FR$jbprtfp %in% c("Often", "Always") ~ "conflit élevé",
    TRUE ~ NA_character_  # Exclude responses such as 'Don't have partner/family' or missing data
  )
)

# Filter out the NA values in the new recoded variable
filtered_data_2A <- ESS10FR %>% filter(!is.na(jbprtfp_recode_3cat))

# Check the distribution of the new recoded variable
table(filtered_data_2A$jbprtfp_recode_3cat)

# Create a cross-tabulation of work-life balance categories by gender
cross_tab_2B <- table(filtered_data_2A$gndr, filtered_data_2A$jbprtfp_recode_3cat)

# Print the cross-tabulation
print(cross_tab_2B)

# Visualize with a bar plot
ggplot(filtered_data_2A, aes(x = gndr, fill = jbprtfp_recode_3cat)) +
  geom_bar(position = "dodge") +
  labs(title = "Équilibre travail-vie personnelle par sexe",
       x = "Sexe",
       y = "Compte") +
  scale_fill_brewer(palette = "Pastel1", name = "Conflit travail-vie personnelle") +
  theme_minimal()



