
rm(list = ls())

library(ggplot2)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
#install.packages("cowplot")
library(cowplot)
library(RColorBrewer)
library(ggpubr)

##The following is to plot delta
delta<-read.csv(file = "/vast/eande106/projects/Nikita/Filtration/Experiment_3/processed_data/20241014del.csv",header = TRUE,sep = ',')

delta_1 <- delta %>%
  mutate(new_column = case_when(
    concentration_um == 0 & drug == "Albendazole_F" ~ "Control_F",
    concentration_um == 0 & drug == "Albendazole_B" ~ "Control_B",
    concentration_um == 30 & drug == "Albendazole_F" ~ "Drug_F",
    concentration_um == 30 & drug == "Albendazole_B" ~ "Drug_B")) %>%
  tidyr::separate(new_column, into=c("Condition","Method"), sep = "_",remove = F) %>%
  dplyr::mutate(Method=ifelse(Method=="F","Filter","Bleach"))
  
##Assign colors to replicates:
replicate_colors <- c("NJR2"="#8f8f8f", "R3"= "#785ef0", "R4"= "#dc267f") ##648fff

##Set the comparisons you want to make
comparisons <- list(
  c("Bleach", "Filter"))

##Set positions for the p values
p_value_y_positions <- max(delta_1$median_wormlength_um_delta) + 90
  
###############################
##Separate the two conditions in two graphs so that variance can be seen clearly

Control <- delta_1 %>%
  dplyr::filter(Condition=="Control")  

Drug <- delta_1 %>%
  dplyr::filter(Condition=="Drug")  

##Plot durg and control together
p1 <- ggplot(data = delta_1, mapping = aes(x = Method, y = median_wormlength_um_delta)) +
  # Jitter for individual data points
  geom_jitter(
    shape = 16,
    width = 0.15,
    aes(color = Metadata_Experiment),  # Use color for jitter points
    size = 1.2,
    stroke = 0.1,
    alpha = 0.5
  ) +
  
  # Box plot (rendered on top of points)
  geom_boxplot(
    width = 0.5,
    size = 0.3,
    outlier.shape = NA,
    alpha = 0.0
  ) +
  stat_boxplot(geom = 'errorbar', width = 0.4, linewidth = 0.2) +
  
  # Apply custom colors for jitter points
  scale_color_manual(values = replicate_colors) +
  
  # Facet grid for strains and conditions
  facet_grid(strain ~ Condition) +
  
  # Control the number of Y-axis ticks (always 3)
  scale_y_continuous(breaks = function(x) pretty(x, n = 3)) +
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
    label.y = p_value_y_positions,
    size = 2
  ) +
  
  # Set a clean theme with Times New Roman font
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 10, family = "Times New Roman"),
    axis.text.y = element_text(size = 10, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman"),
    strip.text = element_text(size = 10, family = "Times New Roman"),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman")
  ) +
  
  # Dynamically set the Y-axis limits
  coord_cartesian(ylim = c(NA, max(delta_1$median_wormlength_um_delta) + 200)) +
  
  # Add axis labels
  labs(
    y = expression("Normalized animal length" ~(mu * m))
  )

p1

ggsave("/vast/eande106/projects/Nikita/Filtration/Experiment_3/plots/20241120.tiff", plot = p1, width = 5.2, height = 5.2, unit = "in", dpi = 600)

  # Create the plot ####Only for Control condition
p2 <- ggplot(data = Control, mapping = aes(x = Method, y = median_wormlength_um_delta)) +
  geom_boxplot(width = 0.5, size = 0.3, outlier.shape = NA, alpha = 0.5) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, linewidth = 0.2) + 
  geom_jitter(shape = 16, width = 0.15,
              aes(color = Metadata_Experiment),  # Use color for jitter points, not fill
              size = 1.2, stroke = 0.1, alpha = 0.8) +
              scale_color_manual(values = replicate_colors) +  # Apply custom colors for points
  facet_wrap(~ strain, ncol = 2, scales = "free_y") +  # Arrange two strains per row
  scale_y_continuous(breaks = function(x) pretty (x, n = 3)) + # controls the nubmer of Y-axis ticks to always be 3
  
    # Add Wilcoxon test with custom y-positions
  stat_compare_means(comparisons = comparisons, method = "wilcox.test",
                     label.y = p_value_y_positions, size = 3) +  # Adjust p-value label size
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #strip.background = element_blank(),
        legend.position = "NA",
        # strip.text.x.top = element_blank(),
        #strip.text = element_blank(),
        #strip.placement = "outside",
        #strip.text.y.right = element_text(angle = 0), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  
  
  # Increase y-axis limit to give more space for p-values
  coord_cartesian(ylim = c(NA, max(delta_1$median_wormlength_um_delta) +200)) + 
  
  # Add title and axis labels
  #(x = expression("Drug and method of obtaining embryos"),
  labs(y = expression("Normalized worm length" ~(mu*m))) +

# Dynamically set Y-limits per facet using coord_cartesian
#coord_cartesian(ylim = function(x) get_y_limits(unique(Drug$strain)[1]))  # Apply different y-limits for each strain

p2

ggsave("/vast/eande106/projects/Nikita/Filtration/Experiment_3/plots/20241113_1.png", plot = p2, width = 5.2, height = 5.2, unit = "in", dpi = 600)


# Create the plot #### Only for Drug 
p3 <- ggplot(data = Drug, mapping = aes(x = Method, y = median_wormlength_um_delta)) +
  geom_boxplot(width = 0.5, size = 0.3, outlier.shape = NA, alpha = 0.5) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, linewidth = 0.2) + 
  geom_jitter(shape = 16, width = 0.15,
              aes(color = Metadata_Experiment),  # Use color for jitter points, not fill
              size = 1.2, stroke = 0.1, alpha = 0.8) +
  facet_wrap(~ strain, ncol = 2, scales = "free_y") +  # Arrange two strains per row
  scale_y_continuous(breaks = function(x) pretty (x, n = 3)) + # controls the nubmer of Y-axis ticks to always be 3
  scale_color_manual(values = replicate_colors) +  # Apply custom colors for points
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(comparisons = comparisons, method = "wilcox.test",
                     label.y = p_value_y_positions, size = 3) +  # Adjust p-value label size
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #strip.background = element_blank(),
        legend.position = "NA",
        # strip.text.x.top = element_blank(),
        #strip.text = element_blank(),
        #strip.placement = "outside",
        #strip.text.y.right = element_text(angle = 0), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  
  
  # Increase y-axis limit to give more space for p-values
  coord_cartesian(ylim = c(NA, max(delta_1$median_wormlength_um_delta) +200)) + 
  
  # Add title and axis labels
  #(x = expression("Drug and method of obtaining embryos"),
  labs(y = expression("Normalized worm length" ~(mu*m)))


p3

ggsave("/vast/eande106/projects/Nikita/Filtration/Experiment_3/plots/20241113_2.png", plot = p3, width = 5.2, height = 5.2, unit = "in", dpi = 600)


##To calculate variance between the eight plates for each replicate

#First step is to filter by replicate
replicate_1 <- delta_1 %>%
  dplyr::filter(Metadata_Experiment=="NJR2")  

replicate_2 <- delta_1 %>%
  dplyr::filter(Metadata_Experiment=="R3")  

replicate_3 <- delta_1 %>%
  dplyr::filter(Metadata_Experiment=="R4") 


##Kruskall-wallis test
kruskal_test_result_replicate_1 <-kruskal.test(median_wormlength_um ~ Metadata_Plate, data = replicate_1)
print (kruskal_test_result_replicate_1)
#Kruskal-Wallis chi-squared = 2.5132, df = 2, p-value = 0.2846

kruskal_test_result_replicate_2 <-kruskal.test(median_wormlength_um ~ Metadata_Plate, data = replicate_2)
print (kruskal_test_result_replicate_2)
#Kruskal-Wallis chi-squared = 14.547, df = 2, p-value = 0.0006936

kruskal_test_result_replicate_3 <-kruskal.test(median_wormlength_um ~ Metadata_Plate, data = replicate_3)
print (kruskal_test_result_replicate_3)
#Kruskal-Wallis chi-squared = 14.724, df = 2, p-value = 0.0006349

#Perform a Dunn test to test the significant differnce between pairs
dunn_test_result_replicate_1 <- dunn.test(replicate_1$median_wormlength_um, replicate_1$Metadata_Plate, method = "bonferroni")
print(dunn_test_result_replicate_1)

#Perform a Dunn test to test the significant differnce between pairs
dunn_test_result_replicate_2 <- dunn.test(replicate_2$median_wormlength_um, replicate_2$Metadata_Plate, method = "bonferroni")
print(dunn_test_result_replicate_2)

#Perform a Dunn test to test the significant differnce between pairs
dunn_test_result_replicate_3 <- dunn.test(replicate_3$median_wormlength_um, replicate_3$Metadata_Plate, method = "bonferroni")
print(dunn_test_result_replicate_3)

###################################################
##Have a plot for Control and Drug - comparing the two methods:

##Set conditions to compare:
##Set the comparisons you want to make
comparisons <- list(
  c("Bleach", "Filter"))

# Create the plot ####Facet by Condition
p3 <- ggplot(data = delta_1, mapping = aes(x = Method, y = median_wormlength_um_delta)) +
  geom_boxplot(width = 0.5, size = 0.3, outlier.shape = NA, alpha = 0.5) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, linewidth = 0.2) + 
  geom_jitter(shape = 16, width = 0.15,
              aes(color = strain),  # Use color for jitter points, not fill
              size = 1.2, stroke = 0.1, alpha = 0.8) +
  facet_grid(strain~Condition, scales = "free_y") +  # use free Y-axis acales for each plot
  scale_y_continuous(breaks = function(x) pretty (x, n = 3)) + # controls the nubmer of Y-axis ticks to always be 3
  scale_color_manual(values = replicate_colors) +  # Apply custom colors for points
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(comparisons = comparisons, method = "wilcox.test",
                     label.y = p_value_y_positions, size = 3) +  # Adjust p-value label size
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #strip.background = element_blank(),
        legend.position = "NA",
        # strip.text.x.top = element_blank(),
        #strip.text = element_blank(),
        #strip.placement = "outside",
        #strip.text.y.right = element_text(angle = 0), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  
  
  # Increase y-axis limit to give more space for p-values
  coord_cartesian(ylim = c(NA, max(delta_1$median_wormlength_um_delta) +200)) + 
  
  # Add title and axis labels
  #(x = expression("Drug and method of obtaining embryos"),
  labs(y = expression("Normalized worm length" ~(mu*m)))


p3

ggsave("/vast/eande106/projects/Nikita/Filtration/Experiment_3/plots/20241113_3.png", plot = p2, width = 7.5, height = 7.5* .75, unit = "in", dpi = 600)

