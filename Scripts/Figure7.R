
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
delta<-read.csv(file = "/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Processed_data/Figure7_processed_data.csv",header = TRUE,sep = ',')

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

ggsave("/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Plots/Figure7.tiff", plot = p1, width = 5.2, height = 5.2, unit = "in", dpi = 600)

