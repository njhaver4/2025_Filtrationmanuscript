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
#install.packages("dunn.test")
library(dunn.test)

data<-read.csv(file = "/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Processed_data/Figure6_proessed_data.csv",header = TRUE,sep = ',')

filtered_data_1 <- data %>%
  select(median_wormlength_um, concentration_um, drug, assay_bleach, Metadata_Plate) %>%
  dplyr::mutate(Name = case_when(
    drug == "Bleach_JB" ~ "JB",
    drug == "Bleach_MM" ~ "MM",
    drug == "Bleach_NJ" ~ "NJ"
  ))

Filtered_data_2 <- filtered_data_1 %>%
  dplyr::mutate(concentration_um = as.character(concentration_um)) %>%  # Convert to character first
  dplyr::mutate(concentration_um = case_when
                (
                  concentration_um == "0" & assay_bleach == "JMNR1_1" ~ "Bleach_1",
                  concentration_um != "0" & assay_bleach == "JMNR1_1" ~ "Filtration_1",
                  concentration_um == "0" & assay_bleach == "JMNR2_1" ~ "Bleach_2",
                  concentration_um != "0" & assay_bleach == "JMNR2_1" ~ "Filtration_2",
                  concentration_um == "0" & assay_bleach == "JMNHTA_1" ~ "Bleach_3",
                  concentration_um != "0" & assay_bleach == "JMNHTA_1" ~ "Filtration_3",
                  TRUE ~ concentration_um  # in case there are other cases
  ))

Filtered_data_3 <- Filtered_data_2 %>%
  mutate(Analyst = case_when(
    Name == "JB" ~ "Analyst 1",
    Name == "MM" ~ "Analyst 2",
    Name == "NJ" ~ "Analyst 3",
    TRUE ~ Name))


Filtered_data_4 <- Filtered_data_3 %>%
  mutate(
    Replicates = case_when(
      concentration_um %in% c("Bleach_1", "Filtration_1") ~ "Replicate_1",
      concentration_um %in% c("Bleach_2", "Filtration_2") ~ "Replicate_2",
      concentration_um %in% c("Bleach_3", "Filtration_3") ~ "Replicate_3",
      TRUE ~ "Other"  # Default case if no conditions are met
    )
  )

Analyst_colors <- c("Analyst 1"="#007bff", "Analyst 2"="#ff8c00", "Analyst 3"="#ffff00")

# Define pairwise comparisons
comparisons <- list(
  c("Bleach_1", "Filtration_1"),
  c("Bleach_2", "Filtration_2"),
  c("Bleach_3", "Filtration_3")
)

#Calculate the maximum y-value of the mediam_wormlength_um
max_y_value <- max(Filtered_data_4$median_wormlength_um, na.rm = TRUE)

##Set positions for the p values
y_positions <- c(
  max_y_value + 20, #First comparison
  max_y_value + 40, #Second comparison
  max_y_value + 60 #Third comparison
)

# Plot
p1 <- ggplot(data = Filtered_data_4 %>% tidyr::separate(concentration_um,into=c("type","num"),remove = F) %>% dplyr::mutate(Replicates=gsub("_"," ",Replicates)), mapping = aes(x = type, y = median_wormlength_um)) +
  # Jitter for individual data points
  geom_jitter(
    aes(color = Analyst),
    shape = 16,
    width = 0.15,
    size = 1.5,
    stroke = 0.2,
    alpha = 0.5
  ) +
  scale_color_manual(values = Analyst_colors) +  # Apply custom colors for points
  
  # Box plot (rendered on top of points)
  geom_boxplot(
    width = 0.5,
    size = 0.3,
    outlier.shape = NA,
    alpha = 0.0
  ) +
  stat_boxplot(geom = 'errorbar', width = 0.4, linewidth = 0.3) +
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(
    #comparisons = comparisons,
    method = "wilcox.test",
    label.y = y_positions,
    size = 2,
    family = "Times New Roman"
  ) +
  
  # Theme adjustments for cleaner aesthetics
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, family = "Times New Roman"),
    strip.text.y.right = element_text(angle = 0, family = "Times New Roman"),
    axis.text.x = element_text(size = 10, family = "Times New Roman"),
    axis.text.y = element_text(size = 10, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.position = "none"
  ) +
  
  # Add title and axis labels
  labs(
    x = expression("Method of obtaining embryos"),
    y = expression("Animal length" ~(mu * m))
  ) +

# Facet by Replicates
facet_wrap(~ Replicates, ncol = 3,scales="free_x")  # Adjust `ncol` as needed

p1

#Save the figure:
#To save it in the size Plos one recommends:(They recommend max width of 5.2 inches)
ggsave("/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Plots/Figure6.tiff", plot = p1, width = 5.2, height = 5.2, unit = "in", dpi = 600)

