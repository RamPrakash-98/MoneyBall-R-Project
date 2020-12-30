#Batting Table----

# playerID       Player ID code
# yearID         Year
# stint          player's stint (order of appearances within a season)
# teamID         Team
# lgID           League
# G              Games
# G_batting      Game as batter
# AB             At Bats
# R              Runs
# H              Hits
# 2B             Doubles
# 3B             Triples
# HR             Homeruns
# RBI            Runs Batted In
# SB             Stolen Bases
# CS             Caught Stealing
# BB             Base on Balls
# SO             Strikeouts
# IBB            Intentional walks
# HBP            Hit by pitch
# SH             Sacrifice hits
# SF             Sacrifice flies
# GIDP           Grounded into double plays
# G_Old          Old version of games

#Reading the CSV File----

bat=read.csv("D:\\Batting.csv")

#Head----
head(bat)

#Structure----
str(bat)

#Head of at Bats----
head(bat$AB)


#Head of Doubles----
head(bat$X2B)


#Feature Engineering----

#1 Batting Average(BA)

bat$BA=bat$H/bat$AB

#2 Batting On Base Percentage

bat$OBP=(bat$H+bat$BB+bat$HBP)/(bat$AB+bat$BB+bat$HBP+bat$SF)

#3 Batting Slugging Percentage

bat$X1B=bat$H-bat$HR-bat$X2B-bat$X3B

bat$SLG=(bat$X1B+2*bat$X2B+3*bat$X3B+4*bat$HR)/bat$AB



#Reading Salaries Data----

sal=read.csv("D:\\Salaries.csv")


#Reassigning Batting data to yearID>1985----

bat<-subset(bat,yearID>=1985)

summary(bat)


#Combining Batting and Salary data by year----

combo<-merge(x=bat,y=sal,by=c('playerID','yearID'))
summary(combo)

#Lost Players----

lost_players<-subset(combo,playerID %in% c("damanjo01","giambja01","saenzol01"))

#Lost Players year 2001----

lost_players<-subset(lost_players,yearID==2001)

#Lost Players (Reduced Data)-----

lost_players<-lost_players[,c("playerID","H","X2B","X3B","HR","OBP","SLG","BA","AB")]


#Available Players----

available<-combo %>% filter(yearID==2001,salary<15000000,OBP>=mean(lost_players$OBP)) 


#Plotting Salaries vs Home Runs----
library(ggplot2)

pl<-ggplot(available,aes(x=salary,y=HR))+geom_point(aes(color=AB,size=H))+ggtitle("Home Runs vs Salaries",subtitle = "Available Players")+
  theme_light()+ylab("Home Runs")
pl
