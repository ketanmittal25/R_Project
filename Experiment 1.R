library(dplyr)
library(ggplot2)
load("ESS10FR.rdata")  


gender_column <- "gndr"
hours_worked_column <- "wkhtot"


summary_stats <- ESS10FR %>%
  filter(!is.na(!!sym(hours_worked_column)) & !is.na(!!sym(gender_column))) %>%
  group_by(!!sym(gender_column)) %>%
  summarise(
    Average = mean(!!sym(hours_worked_column), na.rm = TRUE),
    Median = median(!!sym(hours_worked_column), na.rm = TRUE),
    SD = sd(!!sym(hours_worked_column), na.rm = TRUE),
    IQR = IQR(!!sym(hours_worked_column), na.rm = TRUE)
  )
print(summary_stats)

# Boxplot
ggplot(ESS10FR, aes(x = !!sym(gender_column), y = !!sym(hours_worked_column), fill = !!sym(gender_column))) +
  geom_boxplot() +
  labs(title = "Répartition des heures de travail rémunérées par sexe",
       x = "Sexe",
       y = "Heures de travail rémunérées par semaine") +
  scale_fill_discrete(name = "Sexe") +
  theme(legend.position = "bottom")

#density plot
ggplot(ESS10FR, aes(x = wkhtot, fill = gndr)) +
  geom_density(alpha = 0.7) +
  labs(title = "Répartition des heures de travail rémunérées par sexe",
       x = "Heures de travail rémunérées par semaine",
       y = "Densité") +
  scale_fill_discrete(name = "Sexe") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Assuming 'Male' and 'Female' are the two levels you want to compare
filtered_data <- ESS10FR %>%
  filter(gndr %in% c("Male", "Female"))

# Now run the t-test on the filtered data
t_test_result <- t.test(wkhtot ~ gndr, data = filtered_data, na.action = na.exclude)
print(t_test_result)