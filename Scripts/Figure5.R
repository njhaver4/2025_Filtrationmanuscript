rm(list = ls())

library(ggplot2)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(cowplot)
#install.packages("dunn.test")
library(dunn.test)

##To make bleach vs filtration comparisons for 8 replicates by three analysts

raw_data_1B<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/Filtration/Filtrationmanuscript/Raw_data/Figure5_rawdata.csv",header = TRUE,sep = ',')

# Define pairwise comparisons
comparisons <- list(
  c("Bleach", "Filtration")
)

# Calculate the maximum y-value of the Embryos per plate
max_y_value <- max(raw_data_1B$Embryos_per_plate, na.rm = TRUE)

# Set positions for the p-values
y_positions <- c(max_y_value + 500)

# Adjust the Analyst column
raw_data_1C <- raw_data_1B %>%
  mutate(Analyst = case_when(
    Person == "JC" ~ "Analyst 1",
    Person == "MM" ~ "Analyst 2",
    Person == "NJ" ~ "Analyst 3",
    TRUE ~ Person
  ))

# Custom colors for Analysts
Analyst_colors <- c("Analyst 1" = "#007bff", "Analyst 2" = "#ff8c00", "Analyst 3" = "#ffff00")

# Plot
p1 <- ggplot(data = raw_data_1C, mapping = aes(x = Method, y = Embryos_per_plate)) +
  # Jitter for individual data points
  geom_jitter(
    aes(color = Analyst),
    shape = 16,
    width = 0.15,
    size = 1.5,
    stroke = 0.2,
    alpha = 0.5
  ) +
  scale_color_manual(values = Analyst_colors) + # Apply custom colors for points
  
  # Box plot (rendered on top of points)
  geom_boxplot(
    width = 0.5,
    size = 0.3,
    outlier.shape = NA,
    alpha = 0.0
  ) +
  stat_boxplot(geom = 'errorbar', width = 0.4, linewidth = 0.3) +
  
  # Add Wilcoxon test with custom y-positions
 # stat_compare_means(
  #  comparisons = comparisons,
  #  method = "wilcox.test",
  #  paired = FALSE,
  #  label.y = y_positions,
  #  size = 2
#  ) +
  
  # Theme adjustments for cleaner aesthetics
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.right = element_text(angle = 0),
    axis.text.x = element_text(angle = 0, size = 10, family = "Times New Roman"),
    axis.text.y = element_text(size = 10, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.position = "none"
  ) +
  
  # Add title and axis labels
  labs(
    x = "Method of obtaining embryos",
    y = "Embryonic yield per plate"
  )

p1


#Save the figure:

#To save it in the size Plos one recommends:(They recommend max width of 5.2 inches - we are doing 2.6 by 2.6)
ggsave("/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Plots/Figure5.tiff", plot = p1, width = 2.6, height = 2.6, unit = "in", dpi = 600)

#Get median
medians <- raw_data_1B %>%
  group_by(Method) %>%
  summarise(median_value = median(Embryos_per_plate, na.rm = TRUE))

# View the results
print(medians)

#Get means
means <- raw_data_1B %>%
  group_by(Method) %>%
  summarise(mean_value = mean(Embryos_per_plate, na.rm = TRUE))

# View the results
print(means)

#Get standard deviation
standarddeviation <- raw_data_1B %>%
  group_by(Method) %>%
  summarise(sd_value = sd(Embryos_per_plate, na.rm = TRUE))

# View the results
print(standarddeviation)

##To determine if there is a statistical difference between the three people in bleach, 
#and if there is a statistical difference between the three people in filtration 
##Use the Kruskall Wallis test 
##Make a dataframe with only bleach and filtration first 

bleach_filtered_data <- raw_data_1B %>%
                         dplyr::filter(Method=="Bleach")  

filtration_filtered_data <- raw_data_1B %>%
                            dplyr::filter(Method=="Filtration")  
  
kruskal_test_result_bleach <-kruskal.test(Embryos_per_plate ~ Person, data = bleach_filtered_data)
print (kruskal_test_result_bleach)

kruskal_test_result_filtration <-kruskal.test(Embryos_per_plate ~ Person, data = filtration_filtered_data)
print (kruskal_test_result_filtration)

#Perform a Dunn test to test the significant differnce between pairs
dunn_test_result_bleach <- dunn.test(bleach_filtered_data$Embryos_per_plate, bleach_filtered_data$Person, method = "bonferroni")
print(dunn_test_result_bleach)

dunn_test_result_filtration <- dunn.test(filtration_filtered_data$Embryos_per_plate, filtration_filtered_data$Person, method = "bonferroni")
print(dunn_test_result_filtration)

