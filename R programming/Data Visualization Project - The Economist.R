#Data Visualization Project - The Economist 
library(ggplot2)
library(data.table)
library(ggthemes)
x = fread('Economist_Assignment_Data.csv')
head(x)
#skip first column to remove V1 as it is not required.

x = fread('Economist_Assignment_Data.csv', drop = 1)
x

baseplot <- ggplot(x,aes(x=CPI, y=HDI,color=Region))
print(baseplot)
y <- baseplot + geom_point(shape = 1, size = 5, stroke = 1.5) +
  geom_smooth(aes(group = 1), method = 'lm', se = FALSE, color ="red", formula = y~log(x))
print(y)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
print(pointsToLabel)

z <- y + geom_text(aes(label = Country), color = "gray20",data = subset(x, Country %in% pointsToLabel),check_overlap = TRUE)+
  theme_economist_white() +
  labs(title ="Corruption and Human Development", x = "Corruption Perceptions Index, 2011 (10=least corrupt)", y = "Human Development Index, 2011 (1=best)")+
  scale_x_continuous(limits = c(.9, 10.5),breaks=1:10)+
  scale_y_continuous(limits = c(0.2, 1.0))
z




