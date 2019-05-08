library(tidyverse)
library(ggrepel)
library(ggthemes)
library(beyonce)
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


WorldBankClasses<-read_csv("WorldBankClasses.csv")
ratios<-student_ratio%>%select(student_ratio, year, indicator, country)%>%rename("Country"=country)
income<-left_join(WorldBankClasses, ratios, by="Country") %>% drop_na()                                                             


income<-income %>% group_by(Country, IncomeStatus, indicator) %>% summarise(avg_student_ratio = mean(student_ratio))
income<-income %>% group_by(IncomeStatus, indicator) %>%
  mutate(Outlier= ifelse((avg_student_ratio > (quantile(avg_student_ratio)[[4]]+1.5*IQR(avg_student_ratio)))| (avg_student_ratio < (quantile(avg_student_ratio)[[2]]-1.5*IQR(avg_student_ratio))),'Outlier',"not"))

income$IncomeStatus<- factor(income$IncomeStatus, levels= c("Low income", "Lower middle income","Upper middle income", "High income"))
levels(income$Country)<- c("Low income", "Lower middle income", "Upper middle income", "High income")

income$indicator<- factor(income$indicator, levels= c("Pre-Primary Education", "Primary Education", "Lower Secondary Education", "Secondary Education", "Upper Secondary Education", "Tertiary Education", "Post-Secondary Non-Tertiary Education"))
levels(income$indicator)<- c("Pre-Primary", "Primary", "Lower Secondary", "Secondary", "Upper Secondary", "Tertiary","Post-Secondary Non-Tertiary")

ggplot(income, aes(x=IncomeStatus, y=avg_student_ratio, label=Country, color=IncomeStatus))+
  geom_boxplot()+
  geom_text_repel(data=filter(income, Outlier == "Outlier"),show.legend=FALSE,fontface="bold",ylim=c(30,Inf), segment.alpha=0.35,nudge_x=0.25,nudge_y = 0.75, size=2.5)+
  xlab("Education Level")+
  ylab("Student to Teacher Ratio")+
  facet_grid(indicator~., scales="free_x", switch="both")+coord_flip()+scale_color_manual(values=beyonce_palette(18))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        strip.text.y = element_text(angle = 180, size=10),
        strip.background = element_blank(),
        legend.position= c(0.87,0.87),
        legend.background = element_blank(),
        legend.text=element_text(size=7),
        legend.title= element_text(size=7),
        legend.box.background = element_blank())+
        guides(color=guide_legend(title="Country Income Status: "))



