
library(tidyverse)
library(gganimate)
library(ggthemes)
library(ggrepel)
library(lubridate)

#Load in Data
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")




birthday<- select(player_dob, name, date_of_birth)
names(birthday)<-c("name", "birthdate")

df<-left_join(grand_slams, birthday, by="name")
df <- mutate(df, playerAge = (interval(start = birthdate, end = tournament_date) /duration(num = 1, units = "years")))


top_winners<-filter(df, rolling_win_count >5)
top_winners<-unique(top_winners$name)

df<- df%>% filter(name %in% top_winners)

X<-ggplot(df, aes(x=playerAge, y=rolling_win_count, color=name))+geom_line()+ 
  geom_point(size = 2)+ xlim(15,42)+ylab('Grand Slam Wins')+
  geom_label_repel(label = df$name, hjust=1, direction="y",xlim=c(37,NA),segment.color='gray81')+
  facet_grid(gender~., ncol=1)+xlab('Age')+
  theme(legend.position = "None")+ggtitle("Career trajectories of players with more than 5 wins")+
  transition_reveal(playerAge)

X<-X+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.line=element_line(colour = "black"), strip.background = element_blank(), strip.text.y=element_text(size=14), legend.position="None", text=element_text(size=14))

X
anim_save('ouput.gif')
