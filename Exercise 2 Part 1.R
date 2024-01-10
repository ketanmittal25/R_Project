# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
load("ESS10FR.rdata")

# Assuming the dataset is already loaded
# Recode 'jbprtfp' into descriptive categories, excluding those who don't have a partner/family
ESS10FR$jbprtfp_recode <- factor(ESS10FR$jbprtfp, levels = c(1, 2, 3, 4, 5),
                                 labels = c("Never", "Hardly ever", "Sometimes", "Often", "Always"))



# Recoding 'wkhtot' into four categories
breakpoints <- c(0, 20, 40, 60, Inf)
labels <- c("Temps partiel", "Temps plein", "Temps plein prolongé", "Excessif")
ESS10FR$wkhtot_recode <- cut(ESS10FR$wkhtot, breaks = breakpoints, labels = labels, include.lowest = TRUE, right = FALSE)

# Filter out responses in 'jbprtfp' that are "Don't have partner/family"
filtered_data <- ESS10FR %>%
  filter(jbprtfp != "Je n'ai pas de partenaire/de famille")

# Cross-tabulation of 'wkhtot_recode' and 'jbprtfp'
cross_tab_1B <- table(filtered_data$wkhtot_recode, filtered_data$jbprtfp)
print(cross_tab_1B)

# Perform Fisher's Exact Test with Monte Carlo simulation
fisher_test_monte_carlo <- fisher.test(cross_tab_1B, simulate.p.value = TRUE, B = 20000)

# Print the result of Fisher's Exact Test with Monte Carlo simulation
print(fisher_test_monte_carlo)


