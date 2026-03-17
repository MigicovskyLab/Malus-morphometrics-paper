#Apple_metadata <- read_csv("Apple_metadata.csv")
library(ggplot2)

desired_order <- c("Malus angustifolia", "Malus coronaria", "Malus ioensis","Malus fusca", 
                   "Malus baccata",
                   "Malus domestica",
                   "Malus floribunda",
                   "Malus halliana",
                   "Malus hupehensis",
                   "Malus kansuensis",
                   "Malus prunifolia",
                   "Malus x robusta",
                   "Malus sargentii",
                   "Malus sieversii",
                   "Malus sikkimensis",
                   "Malus toringo",
                   "Malus toringoides",
                   "Malus transitoria")
Apple_metadata$genotype <- factor(Apple_metadata$genotype, levels = desired_order)

#####Figure 6: PC1 and PC3 genotype boxplots#####

#PC1 boxplot
Apple_metadata %>%
  ggplot(aes(
    x = factor(genotype, levels = desired_order),  
    y = PC1,                                      
    fill = genotype                             
  )) +
  xlab("Species") +
  ylab("PC1 (49.0%)") +
  geom_boxplot() +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  legend.position = "none") +
  geom_vline(xintercept = c(4.5))

#ggsave("PC1Figure6".pdf",plot=last_plot())


#PC3 boxplot
Apple_metadata %>%
  ggplot(aes(
    x = factor(genotype, levels = desired_order),  
    y = PC3,                                      
    fill = genotype                             
  )) +
  xlab("Species") +
  ylab("PC1 (49.0%)") +
  geom_boxplot() +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  legend.position = "none") +
  geom_vline(xintercept = c(4.5))

#ggsave("PC3Figure6",plot=last_plot())

#Plot edits and merging box plots were done in Affinity


### Table S4

#Looking at mean and variation
library(dplyr)
meancheck<- Apple_metadata %>%
  #group_by(genotype) %>%
  summarise(mean_length = mean(length, na.rm = TRUE))

varcheck<- erm %>%
  group_by(genotype) %>%
  summarise(mean_width=mean(width, na.rm=TRUE),
            var_width=var(width, na.rm=TRUE),
            mean_length = mean(length, na.rm = TRUE),
            var_length=var(length, na.rm=TRUE),
            mean_area=mean(area, na.rm=TRUE),
            var_area=var(area, na.rm=TRUE),
            mean_asymmetry=mean(asymmetry, na.rm=TRUE),
            var_asymmetry=var(asymmetry, na.rm=TRUE),
            mean_solidity=mean(solidity, na.rm=TRUE),
            var_solidity=var(solidity, na.rm=TRUE),
            mean_aspect=mean(aspect, na.rm=TRUE),
            var_aspect=var(aspect, na.rm=TRUE))
#write.csv(varcheck, "TableS4.csv")



