batting <- read.csv("batting.csv")
head(batting)
str(batting)
summary(batting)
head(batting$AB,5)
print(colnames(batting))
head(batting$AB)
View(batting) #This callsu out the entire file in a different script
head(batting$X2B)
#Batting Average -> AVG = H/AB
#on the base percentage -> (H + BB + HBP)/(AB + BB + HBP + SF)
#slugging percentage -> SLG = ((1B)+(2*2B) + (3*3B) + (4 *HR))/AB
#1B Singles -> 1B = H-2B-3B-HR 

batting$BA <- batting$H / batting$AB
head(batting$BA)
tail(batting$BA,5)
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
tail(batting$OBP,5)
colnames(batting)
#1B Singles -> 1B = H-2B-3B-HR (for singles)

batting$X1B = batting$H - batting$X2B - batting$X3B - batting$HR
colnames(batting)
tail(batting$X1B,5)

#slugging percentage -> SLG = ((1B)+(2*2B) + (3*3B) + (4 *HR))/AB

batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB
tail(batting$SLG,5)
str(batting)

sal <- read.csv("Salaries.csv")
View(sal)

batting <- subset(batting,yearID >= 1985)
#load dpylr package to read distinct yearID

library(dplyr)
distinct(batting,yearID)
summary(batting)

combo <- merge(x = batting, y = sal, by = c('playerID','yearID'))
head(combo,5)

#analyzing lost players
#first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees, outfielder Johnny Damon (damonjo01) to the Boston Red Sox and infielder Rainer Gustavo "Ray" Olmedo ('saenzol01')

lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players
summary(lost_players)
lost_players <- subset(lost_players, yearID == 2001)

lost_players <- lost_players[,c('playerID', 'H', 'X2B', 'X3B', 'HR', 'OBP', 'SLG', 'BA', 'AB','salary')]
lost_players


#only interested in total AB, mean OBP and salary ( < $15 million for all 3)

#AB = 1469
#mean OBP = 0.364
#salary = $15,000,000/3

pl2001 <- filter(combo,yearID == 2001)
pl2001

importantplmetrics <-  subset(pl2001[,c("playerID","AB","salary",'OBP')])
importantplmetrics

#remove players with salary less than 5 million, AB < 300 and  OBP > 1

reviewdpl <- subset(importantplmetrics, salary <= 5000000 & AB >= 500 & OBP != 0)
reviewdpl

orderedresult <- reviewdpl[order(-reviewdpl$OBP),]


#MY PICKS
#69  berkmla01   2001 577  305000 0.4302326
#293 gonzalu01   2001 609 4833333 0.4285714
#661 pujolal01   2001 590  200000 0.4029630

Result <- subset(orderedresult, playerID %in% c('berkmla01','gonzalu01','pujolal01'))
print(Result)

library(ggplot2)
#only show metrics of OBP, AB and Salary
lost.players <- lost_players[,c('playerID','AB','salary','OBP')]

print(lost.players)

mergedresults <- rbind(Result,lost.players)
print(mergedresults) 

visuals <- ggplot(mergedresults,aes(x=playerID, y=salary),position_fill())
visuals + geom_col(aes(fill = factor(AB)))


                   