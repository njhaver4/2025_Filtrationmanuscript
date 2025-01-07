rm(list = ls())

library(ggplot2)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(cowplot)

#The following data is to plot M9 vs 3% PEG in M9 on X axis and embryo yield on Y axis (SupplementaryFigure 2A)
raw_data_1A<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/Filtration/Filtrationmanuscript/Raw_data/Supplementaryfigure2A_rawdata.csv",header = TRUE,sep = ',')

# convert wide format into long format and add temperature info
data_long_1A <- tidyr::pivot_longer(raw_data_1A, cols = everything())
colnames(data_long_1A)<-c("Wash_solution","Embryo_yield")

# Plot
p1 <- ggplot(data = data_long_1A, mapping = aes(x = Wash_solution, y = Embryo_yield)) +
  # Jitter for individual data points (on bottom)
  geom_jitter(
    shape = 16, 
    width = 0.15,
    color = "black",
    size = 1.5,
    stroke = 0.2,
    alpha = 0.5
  ) +
  # Box plot (on top)
  geom_boxplot(
    width = 0.5,
    size = 0.3, 
    outlier.shape = NA,
    alpha = 0.0
  ) +
  # Errorbar for the box plot
  stat_boxplot(geom = 'errorbar', 
               width = 0.4, 
               linewidth = 0.3) +
  # Customize x-axis labels
  scale_x_discrete(labels = c("X3..PEG.in.M9" = expression("3%"~PEG~"in M9"),
                              "M9" = "M9")) +
  # Add axis labels
  labs(
    x = expression("Wash solution"),
    y = expression("Embryo yield per plate")
  ) +
  # Theme adjustments
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.y.right = element_text(angle = 0, family = "Times"),
    axis.text.x = element_text(angle = 0, size = 10, family = "Times"),
    axis.text.y = element_text(size = 10, family = "Times"),
    axis.title.x = element_text(size = 12, family = "Times"),
    axis.title.y = element_text(size = 12, family = "Times"),
    legend.text = element_text(size = 10, family = "Times"),
    plot.title = element_text(size = 14, family = "Times"),
    legend.position = "top"
  )

p1

##########################
#The following data is to plot M9 vs 3% PEG in M9 on X axis and % embryonic lethality on Y axis (SupplementaryFigure 2B)
raw_data_1B<-read.csv(file = "/home/njhaver4/vast-eande106/projects/Nikita/Filtration/Filtrationmanuscript/Raw_data/Supplementaryfigure2B_rawdata.csv",header = TRUE,sep = ',')
# convert wide format into long format and add temperature info
data_long_1B <- tidyr::pivot_longer(raw_data_1B, cols = everything())
colnames(data_long_1B)<-c("Wash_solution","Embryonic_lethality")

p2 <- ggplot(data = data_long_1B, mapping = aes(x = Wash_solution, y = Embryonic_lethality)) +
  # Jitter for individual data points (on bottom)
  geom_jitter(
    shape = 16, 
    width = 0.15,
    color = "black",
    size = 1.5,
    stroke = 0.2,
    alpha = 0.5
  ) +
  # Box plot (on top)
  geom_boxplot(
    width = 0.5,
    size = 0.3, 
    outlier.shape = NA,
    alpha = 0.0
  ) +
  # Errorbar for the box plot
  stat_boxplot(geom = 'errorbar', 
               width = 0.4, 
               linewidth = 0.3) +
  # Customize x-axis labels
  scale_x_discrete(labels = c("X3..PEG.in.M9" = expression("3%"~PEG~"in M9"),
                              "M9" = "M9")) +
  # Add axis labels
  labs(
    x = expression("Wash solution"),
    y = expression("Percent embryonic lethality")
  ) +
  # Theme adjustments
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.y.right = element_text(angle = 0, family = "Times"),
    axis.text.x = element_text(angle = 0, size = 10, family = "Times"),
    axis.text.y = element_text(size = 10, family = "Times"),
    axis.title.x = element_text(size = 12, family = "Times"),
    axis.title.y = element_text(size = 12, family = "Times"),
    legend.text = element_text(size = 10, family = "Times"),
    plot.title = element_text(size = 14, family = "Times"),
    legend.position = "top"
  )

p2

p3 <- p1 +
  stat_compare_means(method = "wilcox.test", size = 3, label.y.npc = "top",label.x.npc = "left",geom = 'text')  #, label.y = 10) #, label.x = 2) 

p3

p4 <- p2 +stat_compare_means(method = "wilcox.test", size = 3, label.y.npc = "top",label.x.npc = "left",geom = 'text') #label.y = 10.7, label.x = 1.4)
p4


Combined_plot <- plot_grid(p1, p2, ncol = 2,label_fontfamily = "Times",
                           labels = c("A", "B"))
Combined_plot


ggsave("/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Plots/Supplementaryfigure2.tiff", plot = Combined_plot, width = 5.2, height = 5.2, unit = "in", dpi = 600)

#Get median
medians <- data_long_1A %>%
  group_by(Wash_solution) %>%
  summarise(median_value = median(Embryo_yield, na.rm = TRUE))

# View the results
print(medians)

#Get means
means <- data_long_1A %>%
  group_by(Wash_solution) %>%
  summarise(mean_value = mean(Embryo_yield, na.rm = TRUE))

# View the results
print(means)

#Get standard deviation
standarddeviation <- data_long_1A %>%
  group_by(Wash_solution) %>%
  summarise(sd_value = sd(Embryo_yield, na.rm = TRUE))

# View the results
print(standarddeviation)

#Get means
means_1 <- data_long_1B %>%
  group_by(Wash_solution) %>%
  summarise(mean_value = mean(Embryonic_lethality, na.rm = TRUE))

# View the results
print(means_1)
