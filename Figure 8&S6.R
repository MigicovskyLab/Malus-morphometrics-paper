##Label: Figure 8 and Figure S6
library(lme4)
library(emmeans)
library(multcomp)
library(multcompView)
library("ggthemes")
#Tree_ploid<- left_join(Tree_level_metadata,ploid)
Tree_ploid<- read.csv("Tree_level_metadata.csv")

#Should only remove 2 rows
Excluding_pent<- Tree_ploid %>% filter(Coded.Value!= "Pentaploid")



#PC1
model1<- lmer(PC1~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model1)
anova(model1)
emm<-emmeans(model1,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#PC3
model2<- lmer(PC3~Coded.Value + (1|genotype), data= Excluding_pent)
summary(model2)
anova(model2)
emm<-emmeans(model2,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#PC4
model3<- lmer(PC4~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model3)
anova(model3)
emm<-emmeans(model3,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#PC5
model4<- lmer(PC5~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model4)
anova(model4)
emm<-emmeans(model4,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#area
model5<- lmer(area~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model5)
anova(model5)
emm<-emmeans(model5,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#aspect
model6<- lmer(aspect~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model6)
anova(model6)
emm<-emmeans(model6,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#length
model7<- lmer(length~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model7)
anova(model7)
emm<-emmeans(model7,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#solidity
model8<- lmer(solidity~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model)
emm<-emmeans(model8,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#width
model9<- lmer(width~Coded.Value + (1|genotype), data=Excluding_pent)
summary(model9)
anova(model9)
emm<-emmeans(model9,~ Coded.Value)
pairs(emm, adjust="tukey")
cld(emm, Letters = letters)

#####Figure S6#####
long_data<- Excluding_pent%>% pivot_longer(cols=c("PC1","PC3","PC4","PC5", "aspect","width", "length","area", "solidity"), names_to = c("Trait"), values_to = "Value")
long_data<- long_data %>% filter(Coded.Value %in% c("Diploid", "Triploid", "Tetraploid"))
desired_order<- c("Diploid", "Triploid", "Tetraploid")

ggplot(long_data, aes(x = factor(Coded.Value, levels = desired_order), y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Trait, scales = "free_y") + 
  theme_minimal() +
  labs(x = "Ploidy Level", y = "Trait Values")+
  theme(axis.text.x = element_text(size = 7, angle=45),
        panel.grid = element_blank(),
        axis.line = element_line())
#Further plot edits were completed in Affinity
ggsave("FigureS6.pdf", plot=last_plot())


#Further plot edits were completed in Affinity


