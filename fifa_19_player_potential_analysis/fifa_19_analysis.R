#------FIFA 19 Player Potential Analysis--------#
# source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset
# The purpose of this project is to use the FIFA 19 dataset to analyze what are
# the most important variables that contribute to a higher/lower player's potential in the
# FIFA 19 video game.

# FIFA 19 is a soccer video game created by EA Sports that allows the user of the game
# to play as any team/individual player from a first point of view. Each player has
# distinct ratings on a given set of player characteristics and attributes such as passing
# dribbling, shooting, speed, tackling, etc. One of the player's attributes is "Potential"
# which translates to a player's ability to reach a higher Overall rating. The higher 
# the Overall rating, then the better the player is.
#-----------------------------------------------#
library(readxl)
library(rJava)
library(tidyverse)
options(java.parameters = "-Xmx8g")
rm(list = ls())
mydata<- readxl::read_xlsx("fifa_19_player_potential_analysis/FIFA_19.xlsx")
mydata
summary(mydata)
str(mydata)

mydata<- rename(mydata, c("PFoot" = `Preferred Foot`,
                          "WFoot" = `Weak Foot`,
                          "WorkRate" = `Work Rate`,
                          "Skills" = `Skill Moves`,
                          "Intl_Rep" = `International Reputation`,
                          "Body_type" = `Body Type`))
summary(mydata)
mydata <- mydata %>%
  mutate_if(is.character,as.factor)

# Pick variables that do not h
myvars<- c("Name","Age", "Nationality", "Overall","Potential","Club",
           "Value", "Wage","Skills","WorkRate", "PFoot","Intl_Rep","WFoot","Finishing",
           "Body_type","Position","Height","Weight","ShortPassing","LongPassing","Dribbling","BallControl",
           "Crossing", "HeadingAccuracy","Volleys","Curve","FKAccuracy",
           "Acceleration",	"SprintSpeed",	"Agility",
           "Reactions","Balance","ShotPower","Jumping","Stamina",
           "Strength","LongShots","Aggression", "Interceptions",
           "Positioning","Vision","Penalties","Composure",
           "Marking",	"StandingTackle",	"SlidingTackle")

newdata<-mydata[myvars]
newdata
summary(newdata)
# Take out NA's
newdata <- na.omit(newdata)
hist(newdata$Skills)
unique(newdata$WorkRate)
hist(newdata$WorkRate)

summary(newdata)
ST_data <- newdata %>% 
  filter(Position == "ST")

# Cleaning the data  
library(tidyr)

ST_data <- separate(ST_data, WorkRate, into = c("AttWR", "DfWR"), sep = "/")
ST_data <- select(ST_data, -c(Name,Club, Value, Wage, Body_type, Position))
# Function that turns feet to inches the Height column
f1 <- function(x) {
  x1 <- as.numeric(sub("'.*", "", x))
  x2 <- as.numeric(sub(".*'", "", x))
  (x1 * 12) + x2
}
ST_data$Height <- f1(ST_data$Height)
ST_data$Weight <- as.numeric(sub("lbs","",ST_data$Weight))
str(ST_data)
ST_data$AttWR <- as.factor(ST_data$AttWR)
ST_data$DfWR <- as.factor(ST_data$DfWR)
library(caret)
library(glmnet)
# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(ST_data$Potential, p = 0.8, list = FALSE)
train_data <- ST_data[trainIndex, ]
test_data <- ST_data[-trainIndex, ]

# Build the model
model <- train(Potential ~ ., data = train_data, method = "glmnet")

# View the model summary
model

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Calculate the accuracy of the model
accuracy <- mean(predictions == test_data$Potential)

# View the accuracy
accuracy

# View the coefficients of the model
coef(model$finalModel)

# Plot the variable importance
varImp(model)



# New model without the nationality variables -----------------------------
ST_data_new <- select(ST_data, -c(Nationality))

trainIndex <- createDataPartition(ST_data_new$Potential, p = 0.8, list = FALSE)
train_data <- ST_data_new[trainIndex, ]
test_data <- ST_data_new[-trainIndex, ]

# Build the model
model <- train(Potential ~ ., data = train_data, method = "glmnet")

# View the model summary
model

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Calculate the accuracy of the model
accuracy <- mean(predictions == test_data$Potential)

# View the accuracy
accuracy

# View the coefficients of the model
coef(model$finalModel)

# Plot the variable importance
varImp(model)


# New model without international reputation and overall variables --------

ST_data_new <- select(ST_data, -c(Nationality, Overall, Intl_Rep))

trainIndex <- createDataPartition(ST_data_new$Potential, p = 0.8, list = FALSE)
train_data <- ST_data_new[trainIndex, ]
test_data <- ST_data_new[-trainIndex, ]

# Build the model
model <- train(Potential ~ ., data = train_data, method = "glmnet")

# View the model summary
model

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Calculate the accuracy of the model
accuracy <- mean(predictions == test_data$Potential)

# View the accuracy
accuracy

# View the coefficients of the model
coef(model$finalModel)

# Plot the variable importance
varImp(model)

# newdata$Nationality
# summary(newdata$Nationality)
# 
# plot(newdata$Potential,newdata$Overall)
# newdata$Nationality<- as.factor(newdata$Nationality)
# summary(newdata)
# ##England subset
# english_team <- newdata[newdata$Nationality=="England",]
# summary(english_team)
# english_team2<-na.omit(english_team)
# summary(english_team2)
# ##Germany
# Germans<- newdata[newdata$Nationality=="Germany",]
# summary(Germans)
# Deutsch<-na.omit(Germans)
# summary(Deutsch)
# ##Spain
# Spanish<- newdata[newdata$Nationality=="Spain",]
# summary(Spanish)
# Espanol<-na.omit(Spanish)
# summary(Espanol)
# ##Argentina
# Argentinians<- newdata[newdata$Nationality=="Argentina",]
# Argentinos<- na.omit(Argentinians)
# summary(Argentinos)
# ##France
# French<- newdata[newdata$Nationality=="France",]
# Frances<- na.omit(French)
# summary(Frances)
# ##Brazil
# Brazileros<- newdata[newdata$Nationality=="Brazil",]
# Verdeamarelos<- na.omit(Brazileros)
# summary(Verdeamarelos)


# library(corrplot)
# library(corrgram)
# library(psych)
# corrplot(corrgram(cr))
# pairs(cr)
# pairs(cr, labels = colnames(cr),
#       pch=21,
#       bg= rainbow(3),
#       col=rainbow(3),
#       main= "Top Countries",
#       row1attop = TRUE,
#       gap = 1,
#       cex.labels = NULL,
#       font.labels = 1)
# 
# hist(s1$Overall)
# 
# hist(s1$Overall, freq=FALSE,col = "red",xlab = "Overall",
#      main = "Players' Ratings", ylim = c(0,0.06))
# rug(jitter(s1$Overall))
# lines(density(s1$Overall),col="black", lwd=2)
# 
# plot(s1$Overall~s1$Potential, xlab="Potential", ylab="Overall", main="Linearity between Potential and Overall Ratings")
# summary(s1)
# reg1<- lm(s1$Overall~s1$Potential)
# summary(reg1)
# plot(reg1)
# regplot<- ggplot(s1, aes(x=Potential, y=Overall, color=Nationality))+
#   geom_point()
# regplot<- regplot + geom_smooth(method = "lm")
# regplot
# ##
# regplotx<- ggplot(s1, aes(x=Potential, y=Overall))+
#   geom_point()
# regplotx<- regplotx + geom_smooth(method = "lm")
# regplotx
# ##
# regplot<- regplot+ theme_bw()+ labs(title = "Player Potential as a function of Overall Rating")
# regplot
# regplot1<- regplot+ stat_regline_equation(regplotx,label.x = 50, label.y = 85)
# regplot1
# ggPredict(reg1, interactive = TRUE)
# 
# summary(reg1)
# 
# library(ggiraph)
# library(ggiraphExtra)
# library(ggeffects)
# ggPredict(s1, se=TRUE, interactive = TRUE)
# 
# 
# 
# #######Q2
# s1$PFoot<- as.factor(s1$PFoot)
# summary(s1)
# vars3<- c("Finishing","Skills","BallControl","ShortPassing")
# hist(s1$Potential)
# 
# hist(s1$Potential, freq=FALSE,col = "light blue",xlab = "Potential",
#      main = "Players' Potential", ylim = c(0,0.07))
# rug(jitter(s1$Potential))
# lines(density(s1$Potential),col="black", lwd=2)
# 
# #finishing
# plot(s1$Potential~s1$Finishing,col= factor(s1$PFoot),
#      xlab="Finishing", ylab="Potential",
#      main="Linearity between Finishing and Potential Ratings")
# legend(x=75, y=60, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##skills
# 
# plot(s1$Potential~s1$Skills,col= factor(s1$PFoot),
#      xlab="Skills", ylab="Potential",
#      main="Linearity between Skills and Potential Ratings")
# legend(x=4.25, y=58, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##ball control
# 
# 
# plot(s1$Potential~s1$BallControl,col= factor(s1$PFoot),
#      xlab="BallControl", ylab="Potential",
#      main="Linearity between BallControl and Potential Ratings")
# legend(x=80, y=58, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##shortpass
# 
# plot(s1$Potential~s1$ShortPassing,col= factor(s1$PFoot),
#      xlab="ShortPassing", ylab="Potential",
#      main="Linearity between ShortPassing and Potential Ratings")
# legend(x=75, y=60, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##regression Q2
# library(tidyr)
# newdata <- separate(newdata, WorkRate, into = c("Attacking_WR", "Defending_WR"), sep = "/")
# newdata$Attacking_WR <- as.factor(newdata$Attacking_WR)
# newdata$Defending_WR <- as.factor(newdata$Defending_WR)
# newdata$Height <- as.factor(newdata$Height)
# 
# df$height_in <- as.numeric(sub("(\\d+)'(\\d+)", "\\1*12+\\2", newdata$Height))
# 
# 
# s2<- sample(nrow(s1),500, replace = TRUE)
# s2
# fit1<- lm(s1$Potential~s1$Finishing+s1$Skills+s1$BallControl+s1$ShortPassing)
# summary(fit1)
# fit1<- lm(s1$Potential~s1$Finishing*s1$PFoot)
# summary(fit1)
# fit1
# 
# myvars3<- c("Name","Age", "Nationality", "Overall","Skills",
#            "Potential", "Club","WorkRate","Position", "PFoot","WFoot","Finishing",
#            "ShortPassing","BallControl")
# newdata3<-mydata[myvars3]
# newdata3
# summary(newdata3)
# newdata3$Position<-as.factor(newdata3$Position)
# summary(newdata3)
# samplex<- newdata3[newdata3$Position=="ST",]
# samplex
# str(samplex)
# samplex$PFoot<- as.factor(samplex$PFoot)
# summary(samplex)
# STsample<- na.omit(samplex)
# STsample
# summary(STsample)
# 
# hist(STsample$Potential)
# 
# hist(STsample$Potential, freq=FALSE,col = "light blue",xlab = "Potential",
#      main = "Strikers' Potential")
# rug(jitter(STsample$Potential))
# lines(density(STsample$Potential),col="black", lwd=2)
# 
# plot(STsample$Potential~STsample$Finishing,col= factor(STsample$PFoot),
#      xlab="Finishing", ylab="Potential",
#      main="Linearity between Finishing and Potential Ratings")
# legend(x=85, y=65, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##skills use as factor
# STsample$Skills<- as.numeric(STsample$Skills)
# plot(STsample$Potential~STsample$Skills,col= factor(STsample$PFoot),
#      xlab="Skills", ylab="Potential",
#      main="Linearity between Skills and Potential Ratings")
# legend(x=3.5, y=65, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##ball control
# plot(STsample$Potential~STsample$BallControl,col= factor(STsample$PFoot),
#      xlab="BallControl", ylab="Potential",
#      main="Linearity between BallControl and Potential Ratings")
# legend(x=80, y=65, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##shortpass
# 
# plot(STsample$Potential~STsample$ShortPassing,col= factor(STsample$PFoot),
#      xlab="ShortPassing", ylab="Potential",
#      main="Linearity between ShortPassing and Potential Ratings")
# legend(x=75, y=62, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# 
# ##sprintspeed
# 
# plot(STsample$Potential~STsample$SprintSpeed,
#      xlab="Speed", ylab="Potential",
#      main="Linearity between Speed and Potential Ratings")
# legend(x=75, y=62, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# ##strength
# 
# plot(STsample$Potential~STsample$Strength,
#      xlab="Strength", ylab="Potential",
#      main="Linearity between Strength and Potential Ratings")
# legend(x=75, y=62, c("Left Foot","Right Foot"),
#        col = c("black","red"),
#        pch=21)
# 
# myvars3<- c("Name","Age", "Nationality", "Overall","Skills",
#             "Potential", "SprintSpeed","Strength","Position", "PFoot","WFoot","Finishing",
#             "ShortPassing","BallControl")
# newdata3<-mydata[myvars3]
# newdata3
# summary(newdata3)
# newdata3$Position<-as.factor(newdata3$Position)
# summary(newdata3)
# samplex<- newdata3[newdata3$Position=="ST",]
# samplex
# str(samplex)
# samplex$PFoot<- as.factor(samplex$PFoot)
# summary(samplex)
# STsample<- na.omit(samplex)
# STsample
# summary(STsample)
# 
# reg2<- lm(STsample$Potential~STsample$Finishing+STsample$BallControl+STsample$SprintSpeed+STsample$Strength+STsample$ShortPassing)
# summary(reg2)
# 
# ##finishing +pote*pfoot
# reg4<- lm(STsample$Potential~STsample$Finishing+STsample$PFoot)
# summary(reg4)
# 
# regplot4<- ggplot(STsample, aes(x=Finishing, y=Potential, color=PFoot))+
#   geom_point()
# regplot4<- regplot2 + geom_smooth(method = "lm")
# regplot4
# regplot4<- regplot4+ theme_bw()+ labs(title = "Strikers' Finishing ability as a function of their Potential")
# regplot4
# regplot4<- regplot+ stat_regline_equation(reg4,aes(Finishing,Potential),label.x = 50, label.y = 85)
# regplot4
# 
# ###
# 
# reg3<- lm(STsample$Potential~STsample$Finishing*STsample$SprintSpeed+STsample$Finishing*STsample$Strength)
# summary(reg3)
# 
# ##speed
# regplot3<- ggplot(STsample, aes(x=Finishing, y=Potential, color=SprintSpeed))+
#   geom_point()
# regplot3<- regplot3 + geom_smooth(method = "lm")
# regplot3
# regplot3<- regplot3+ theme_bw()+ labs(title = "Strikers' Finishing ability (Speed) as a function of their Potential")
# regplot3
# 
# test$predicyion= predict(reg3, test, type="class")
# 
# 
# ##strength
# regplot5<-ggplot(STsample, aes(x=Finishing, y=Potential, color=Strength))+
#   geom_point()
# regplot5<- regplot5 + geom_smooth(method = "lm")
# regplot5
# 
# 
# regplot5<- regplot5+ theme_bw()+ labs(title = "Strikers' Finishing ability (Strength) as a function of their Potential")




plot(STsample$Potential~STsample$Finishing+STsample$BallControl+
       STsample$SprintSpeed+STsample$Strength+STsample$ShortPassing)

plotting.data<-expand.grid(
  Finishing = seq(min(STsample$Finishing), max(STsample$Finishing), length.out=30),
  BallControl=c(min(STsample$BallControl), mean(STsample$BallControl), max(STsample$BallControl)),
                Speed=c(min(STsample$SprintSpeed),mean(STsample$SprintSpeed),max(STsample$SprintSpeed)),
                        Strength=c(min(STsample$Strength),mean(STsample$Strength),max(STsample$Strength)),
                                   ShortPassing=c(min(STsample$ShortPassing),mean(STsample$ShortPassing),max(STsample$ShortPassing)))


plotting.data

regplot3<- ggplot(STsample, aes(x=Finishing, y=Potential))+
  geom_point()
regplot3
regplot3<- regplot3+geom_line(data = plotting.data)
regplot3


