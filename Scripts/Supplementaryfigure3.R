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
  c("Control", "Drug"))

##Removing the outliers##
 delta_2 <-delta_1 %>%
   filter(!FileName_RawBF %in% c("20240920-R3-p005-m2x_A01.TIF", "20240926-R4-p006-m2x_H03.TIF"))

##Filtering for Bleach
poo_1 <- delta_2 %>%
  dplyr::filter(Method=="Bleach")

poo_1 <- poo_1 %>%
  dplyr::mutate(class=ifelse(strain %in% c("CX11271", "MY16"), "R", "S"))



###Plot control (panel 1)
p1_c1 <- ggplot(data = poo_1 %>% dplyr::filter(class=="R"), mapping = aes(x = Condition, y = median_wormlength_um_delta)) +
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
  facet_grid(strain ~ Method) +
  
  # Control the number of Y-axis ticks (always 3)
  scale_y_continuous(breaks = c(-50,0,50)) +
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
    #aes(label.y = p_value_y_positions),
    #label.y = 60 * .85, 
    label.y = 50,
    size = 2
  ) +
  
  # Set a clean theme with Times New Roman font
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, family = "Times New Roman"),
    axis.title.x =element_blank(),
    axis.title.y = element_blank (), #text(size = 12, family = "Times New Roman"),
    strip.text = element_text(size = 10, family = "Times New Roman"),
    strip.text.y = element_blank(),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    plot.margin = unit(c(0,0.5,0.2,0), "lines"))  +
  
  # Dynamically set the Y-axis limits
  coord_cartesian(ylim = c(-65,70)) 
p1_c1

  
#Plot control (panel 2)
p1_c2 <- ggplot(data = poo_1 %>% dplyr::filter(class=="S"), mapping = aes(x = Condition, y = median_wormlength_um_delta)) +
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
  facet_grid(strain ~ Method) +
  
  # Control the number of Y-axis ticks (always 3)
  scale_y_continuous(breaks=c(-350,-150,50)) +
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
   # aes(label.y = p_value_y_positions),
   #label.y = 100 * .85,
   label.y = 50,
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
    axis.title.y = element_blank (), #text(size = 12, family = "Times New Roman"),
    strip.text = element_text(size = 10, family = "Times New Roman"),
    strip.text.y = element_blank(),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0,0.5,0,0), "lines")) +
  coord_cartesian(ylim = c(-400,120)) 

p1_c2


#plot all Bleach panels
p1 <- cowplot::plot_grid(p1_c1,p1_c2,nrow = 2,ncol=1,rel_heights = c(1,2),align="hv",rel_widths = c(1,1))

p1

###Filtering only Filtration####
poo_2<- delta_2 %>%
  dplyr::filter(Method=="Filter")

###Filtering in the drug for CB4856, ECA36, JU775, and N2 (those strains that have reduced length)
poo_3 <- poo_2 %>%
  dplyr::filter(strain %in% c("CB4856", "ECA36", "JU775", "N2"))

poo_2 <- poo_2 %>%
  dplyr::mutate(class=ifelse(strain %in% c("CX11271", "MY16"), "R", "S"))


###Plot filtration (panel 1)
p2_c1 <- ggplot(data = poo_2 %>% dplyr::filter(class=="R"), mapping = aes(x = Condition, y = median_wormlength_um_delta)) +
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
  facet_grid(strain ~ Method) +
  
  # Control the number of Y-axis ticks (always 3)
  scale_y_continuous(breaks = c(-50,0,50)) +
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
   # aes(label.y = p_value_y_positions),
   label.y = 50,
    size = 2
  ) +
  
  # Set a clean theme with Times New Roman font
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, family = "Times New Roman"),
    axis.title.x =element_blank(),
    axis.title.y = element_blank (), #text(size = 12, family = "Times New Roman"),
    strip.text = element_text(size = 10, family = "Times New Roman"),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    plot.margin = unit(c(0,0,0.2,0), "lines"))  +
  
  # Dynamically set the Y-axis limits
  coord_cartesian(ylim = c(-65,70)) 
  
  # Add axis labels
 # labs(
  #  y = expression("Normalized animal length" ~(mu * m))
 # )
p2_c1

#plot drug (panel 2)
p2_c2 <- ggplot(data = poo_2 %>% dplyr::filter(class=="S"), mapping = aes(x = Condition, y = median_wormlength_um_delta)) +
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
  facet_grid(strain ~ Method) +
  
  # Control the number of Y-axis ticks (always 3)
  scale_y_continuous(breaks=c(-350,-150,50)) +
  
  # Add Wilcoxon test with custom y-positions
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
    #aes(label.y = p_value_y_positions),
    label.y = 50,
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
    axis.title.y = element_blank (), #text(size = 12, family = "Times New Roman"),
    strip.text = element_text(size = 10, family = "Times New Roman"),
    plot.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    strip.text.x = element_blank(),
    plot.margin = unit(c(0,0,0,0), "lines")) +
  coord_cartesian(ylim = c(-400,120)) 

p2_c2

#plot all drug panels
p2 <- cowplot::plot_grid(p2_c1,p2_c2,nrow = 2,ncol=1,rel_heights = c(1,2),align="hv",rel_widths = c(1,1))
p2

##Combine the two plots
combined_plot <- plot_grid(p1, p2, 
                           ncol=2,
                           labels = c("A", "B"),
                           rel_heights =c(1,1))
combined_plot

combined_plot_1 <- ggdraw() +
  #draw_grob(grid::rectGrob(width = 0.3, height = 1, gp = grid::gpar(fill = "white", col = NA)), x = 0.03, y = 0.5) + 
  draw_label(expression("Normalized animal length" ~(mu * m)),angle=90,x=0.02,y=0.5,size=12,fontfamily = "times") +
  draw_plot(combined_plot,x=0.04,y=0,width = 0.95,height = 1)

combined_plot_1

ggsave("/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Plots/Supplementaryfigure3.tiff", plot = combined_plot_1, width = 5.2, height = 5.2, unit = "in", dpi = 600)

