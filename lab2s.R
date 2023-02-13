#### Optional Module _ Dara Visualization #####
install.packages("tidyverse")
library(tidyverse)
install.packages("Lahman")
library(Lahman)
library(here)

# change scale attributes
Batting2019 <- filter(Batting, yearID == 2019) # note that this plot uses the Batting2019 (instead of Batting 2017)
plot0 <- ggplot(Batting2019, aes(AB, HR, color = lgID, size = HBP)) +
  geom_point(alpha = 0.99, fill = "#FFFFF0", shape = 1, stroke = 1) +   # alpha modifies the transparency; size modifies the size of marks; shape=1 is the circle; stroke modifies the thickness
  labs(title = "Distribution of Homerun and At-Bats in 2019 Season", x = "At Bats", y = "Home Run", color = "Leagues", size = "Hit by Pitch")+
  theme_classic()
plot0

Batting2019 <- filter(Batting, yearID == 2019) # note that this plot uses the Batting2019 (instead of Batting 2017)
plot1 <- ggplot (Batting2019, aes(AB, HR, color = lgID, size = HBP)) +
  geom_point(alpha = 0.99, fill = "#FFFFF0", shape = 1, stroke = 0.5) +   # alpha modifies the transparency; size modifies the size of marks; shape=1 is the circle; stroke modifies the thickness
  scale_x_continuous(name = "At Bats", limits = c(0, 700)) +            # you can also use labs(x="At Bats") here; Don't forget to add the quotation mark if you're using character strings (i.e., texts)
  scale_y_continuous(name = "Home Run", limits = c(0, 70)) +
  scale_color_discrete(name = "Leagues", labels = c("American League", "National League")) +
  scale_size_continuous(name = "Hit by Pitch") +
  ggtitle("Distribution of Homerun and At-Bats in 2019 Season")+
  theme_classic()+
  theme(plot.title = element_text(color="#C3142D", size=16, face="bold"))
plot1

# interactive plot
library(plotly)

plot1.1 <- ggplotly(plot1, tooltip = c("x", "y", "color", "size"))
plot1.1

plot2 <- ggplot(Batting2019, aes(x = G))+
  geom_histogram()
plot2

plot2.1<-ggplotly(plot2)
plot2.1

#save interactive plot in html
library(htmlwidgets)
saveWidget(as_widget(plot2.1), "index.html", selfcontained=T)


# plot with team/player logo with two more packages:ggthemes and ggimage
if(!require(ggthemes)) install.packages("ggthemes") #Optional package for extra themes. See all themes here https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
library(ggthemes)
if(!require(ggimage)) install.packages("ggimage") #Optional package supports image files and graphic objects to be visualized in 'ggplot2' graphic system.
library(ggimage)

MLB_Team_Logo <- read.csv("team_logo.csv")
MLB_Team_Logo <- MLB_Team_Logo %>% filter(teamID == "CHN" | teamID == "CIN" | teamID == "MIL" |teamID == "PIT" |teamID == "ATL")
head(MLB_Team_Logo)


##??һ????ͼ??ͼƬ
Teams2019 <- Teams %>% filter(yearID ==2019)
Teams2019 <- inner_join(Teams2019, MLB_Team_Logo, by = "teamID")
head(Teams2019)
ggplot(Teams2019, aes(RA, R)) +
  geom_point()+
  labs(title = "Runs Allowed and Runs Scored of Each Team", subtitle = "2019 Season", x = "Runs Allowed", y = "Runs Scored")+
  geom_image(aes(image = logo), size = 0.05)+
  coord_cartesian()+
  theme_solarized()+
  theme(plot.title = element_text(size=16, face="bold"))

# animation plot
if(!require(gganimate)) install.packages("gganimate")
library(gganimate)

##??һ????ͼ?Ķ???
Teams00_20 <- Teams %>% filter(lgID == "NL" & divID == "C" & yearID >=2000)
Teams00_20 <- inner_join(Teams00_20, MLB_Team_Logo, by = "teamID")
head(Teams00_20)
plot <- ggplot(Teams00_20, aes(R, W)) +
  geom_point() +   # alpha modifies the transparency; size modifies the size of marks; shape=1 is the circle; stroke modifies the thickness
  geom_image(aes(image = logo), size = 0.05)+
  ggtitle("Distribution of Homerun and At-Bats in 2019 Season")+
  theme_classic()+
  theme(plot.title = element_text(color="#C3142D", size=12, face="bold"))+
  labs(title = 'Season: {frame_time}', x = "Runs Scored", y = "W") +
  transition_time(yearID) +
  ease_aes()
plot

animate(plot,
        duration = 5, # in seconds
        fps  =  10)

anim_save("MLB Win and Runs Scored.gif")

##?ڶ?????ͼ??ͼƬ
ggplot(Teams2019) +
  geom_point(aes(x = factor(yearID), y = R, colour = teamID)) +
  geom_line(aes(group = teamID)) +
  labs(title = "Team Performance in NLC 2000-2019",x = "Seasons", y = "Wins")+
  scale_y_continuous(position = "left")+ 
  theme_economist() +
  scale_colour_economist()


# animation plot
if(!require(gganimate)) install.packages("gganimate")
library(gganimate)


##?ڶ?????ͼ?Ķ???
Teams00_20 <- Teams %>% filter(lgID == "NL" & divID == "C" & yearID >=2000& yearID <=2019)
Teams00_20 <- inner_join(Teams00_20, MLB_Team_Logo, by = "teamID")
plot <- ggplot(Teams00_20) +
  geom_point(aes(x = yearID, y = R, colour = teamID)) +   # alpha modifies the transparency; size modifies the size of marks; shape=1 is the circle; stroke modifies the thickness
  geom_line(aes(group = teamID)) +
  ggtitle("Distribution of Homerun and At-Bats in 2019 Season")+
  theme_classic()+
  theme(plot.title = element_text(color="#C3142D", size=12, face="bold"))+
  labs(title = "Team Performance in NLC 2000-2019", x = "Seasons", y = "Wins") +
  transition_time(yearID) +
  ease_aes()+
  scale_y_continuous(position = "left")+ 
  theme_economist() +
  scale_colour_economist()
plot

animate(plot,
        duration = 5, # in seconds
        fps  =  10)

anim_save("Team Performance in NLC 2000-2019.gif")
