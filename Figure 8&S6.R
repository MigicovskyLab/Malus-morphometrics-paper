##Label: Figure 8 and Figure S6
library(lme4)
library(emmeans)
library(multcomp)
library(multcompView)
library("ggthemes")
Trait_meta<- read.csv("Malus_ploidy_metadata.csv")
#PC1
model1<- lmer(PC1~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#PC3
model2<- lmer(PC3~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#PC4
model3<- lmer(PC4~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#PC5
model4<- lmer(PC1~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#area
model5<- lmer(area~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#aspect
model6<- lmer(aspect~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#length
model7<- lmer(length~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#solidity
model8<- lmer(solidity~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#width
model9<- lmer(width~Coded.Value + (1|genotype), data=Trait_meta)
summary(model1)
anova(model1, tye="III")
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#####Figure S6#####
ggplot(long_data, aes(x = Coded.Value, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Trait, scales = "free_y") + 
  theme_minimal() +
  labs(x = "Ploidy Level", y = "Trait Values")+
  theme(axis.text.x = element_text(size = 7, angle=45),
        panel.grid = element_blank(),
        axis.line = element_line())
#Further plot edits were completed in Affinity
#ggsave("FigureS6", plot=last_plot(), height=8, width=12)


#####Figure 8#####
library(cowplot)
p1 <- filtered_plot %>% 
  filter(Trait == "PC1") %>% 
  ggplot(aes(x = Coded.Value, y = Value, fill=Coded.Value)) +
  geom_boxplot() +
  labs(title = "PC1",x= "Ploidy level", y = "Value") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-0.4, 0.4)) +
  scale_fill_manual(values = c("#dcdcdb", "#bdbdbd", "#969696", "#4d4e4c"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


p2 <- filtered_plot %>% 
  filter(Trait == "aspect") %>% 
  ggplot(aes(x = Coded.Value, y = Value, fill=Coded.Value)) +
  geom_boxplot() +
  labs(title = "aspect", x = "Ploidy Level", y = "Value") +
  coord_cartesian(ylim = c(0, 1.2)) +
  scale_fill_manual(values = c("#dcdcdb", "#bdbdbd", "#969696", "#4d4e4c"))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(panel.grid = element_blank())

p3 <- filtered_plot %>% 
  filter(Trait == "PC3") %>% 
  ggplot(aes(x = Coded.Value, y = Value, fill=Coded.Value)) +
  geom_boxplot() +
  labs(title = "PC3", x= "Ploidy level", y = "Value") +
  coord_cartesian(ylim = c(-0.2, 0.3)) +
  scale_fill_manual(values = c("#dcdcdb", "#bdbdbd", "#969696", "#4d4e4c"))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(panel.grid = element_blank())


plot_grid(p1, p2, p3, nrow = 1, align = 'v')
#ggsave("Figure 8", plot=last_plot(), height=8, width=12)

#Further plot edits were completed in Affinity


