install.packages("caret")
library(caret)
Dr.J.data <- data.frame(Dr_J_data)



#subset without crazy-Chile
CrazyChile <- subset(Dr.J.data, Group=="Chile") # doesnt follow the pattern, could confuse model

SubData <- subset(Dr.J.data, Group != "Chile", select =c(1:11))

#Data without Chile, unknowns, and empty columns
knownSubData <- subset(SubData, highland.lowland != "?")



#splitting data into test and training sets

smp_size <- floor(0.75 * nrow(knownSubData)) #rounds to next number
set.seed(123)
train_ind <- sample(seq_len(nrow(knownSubData)), size = smp_size)

train <- knownSubData[train_ind, ] #75% data
test <- knownSubData[-train_ind, ] #25% data

# Change the variable names
names(train)
names(train)[4] <- "GOL1"
names(test)[4] <- "GOL1"
names(train)[5] <- "NOL2"
names(test)[5] <- "NOL2"
names(train)[6] <- "WFB3"
names(test)[6] <- "WFB3"
names(train)[7] <- "EKB4"
names(test)[7] <- "EKB4"
names(train)[8] <- "DKS5"
names(test)[8] <- "DKS5"
names(train)[9] <- "GLS6"
names(test)[9] <- "GLS6"
names(train)[10] <- "FRC7"
names(test)[10] <- "FRC7"
names(train)[11] <- "PAC8"
names(test)[11] <- "PAC8"
names(train)


# ----Creating models------

# assign 1 if skull is highland in test & train set , 0 otherwise
train$IsHighland <- ifelse(train$highland.lowland=="highland",1,0)
test$IsHighland <- ifelse(test$highland.lowland=="highland",1,0)


## MODEL 1

model1.glm <- glm(IsHighland ~ GOL1+ NOL2 + WFB3 + EKB4 + DKS5 + GLS6 + FRC7 + PAC8, family=binomial(), data= train )
summary(model1.glm) # p-value < 0.0005, AIC 48.809

#create new variable to store prediction
test$Prob <- predict(model1.glm, newdata = test, type = 'response')

# assign 1 if prob > 0.7, 0 if prob < 0.3, NA otherwise ?
test$Predict <- ifelse(test$Prob > 0.7, 1, ifelse(test$Prob<0.3,0,"NA"))
test$Predict <- as.numeric(test$Predict) # change to numeric
hist(test$Predict)
hist(test$Prob)

#Remove unused "levels"  
#test$highland.lowland <- factor(test$highland.lowland)

# Predicted vs Actual
table(test$Predict, test$highland.lowland)
table(test$Predict)

## MODEL 2

model2.glm <- glm(IsHighland ~ GOL1 + NOL2 + GLS6 , family=binomial(), data=train )
summary(model2.glm)
Prob2 <- predict(model2.glm, newdata = test, type = 'response')

test$Predict2 <- ifelse(Prob2 > 0.7, 1, ifelse(Prob2<0.3,0,NA))
test$Predict2 <- as.numeric(test$Predict2)
hist(test$Predict2)

# Predicted vs Actual
table(test$Predict2, test$highland.lowland)
table(test$Predict2)


#MODEL 3

model3.glm <- glm(IsHighland ~ GOL1 + NOL2 + GLS6 + EKB4, family=binomial(), data=train )
summary(model3.glm)
Prob3 <- predict(model3.glm, newdata = test, type = 'response')

test$Predict3 <- ifelse(Prob3 > 0.7, 1, ifelse(Prob3<0.3,0,NA))
test$Predict3 <- as.numeric(test$Predict3)
hist(test$Predict3)

# Predicted vs Actual
table(test$Predict3, test$highland.lowland) #96.88% predicted
table(test$Predict3)


## MODEL 4

model4.glm <- glm(IsHighland ~ GOL1 + NOL2 + GLS6 + EKB4 + PAC8, family=binomial(), data=train )
summary(model4.glm)

Prob4 <- predict(model4.glm, newdata = test, type = 'response')

test$Predict4 <- ifelse(Prob4 > 0.7, 1, ifelse(Prob4<0.3,0,NA))
test$Predict4 <- as.numeric(test$Predict4)
hist(test$Predict4)

table(test$Predict4, test$highland.lowland) # 93.75 %
table(test$Predict4)

#Model 5

model5.glm <- glm(IsHighland ~ GOL1 + NOL2 + GLS6 + EKB4 + WFB3, family=binomial(), data=train )
summary(model5.glm)

Prob5 <- predict(model5.glm, newdata = test, type = 'response')

test$Predict5 <- ifelse(Prob5 > 0.7, 1, ifelse(Prob5<0.3,0,"NA"))
test$Predict5 <- as.numeric(test$Predict5)
hist(test$Predict5)


table(test$Predict5, test$highland.lowland) #94.12
table(test$Predict5)

#Model 6

model6.glm <- glm(IsHighland ~ GOL1 + NOL2 + GLS6 + EKB4 + DKS5, family=binomial(), data=train )
summary(model6.glm)

Prob6 <- predict(model6.glm, newdata = test, type = 'response')

test$Predict6 <- ifelse(Prob6 > 0.7, 1, ifelse(Prob6<0.3,0,NA))
test$Predict6 <- as.numeric(test$Predict6)
hist(test$Predict6)

#Remove unused "levels" 
test$highland.lowland <- factor(test$highland.lowland)

table(test$Predict6, test$highland.lowland) #96.97%
table(test$Predict6)

#Model 7

model7.glm <- glm(IsHighland ~ GOL1 + NOL2 + GLS6 + EKB4 + FRC7, family=binomial(), data=train )
summary(model7.glm)

test$Prob7 <- predict(model7.glm, newdata = test, type = 'response')

test$Predict7 <- ifelse(test$Prob7 > 0.7, 1, ifelse(test$Prob7<0.3,0,NA))
test$Predict7 <- as.numeric(test$Predict7)
hist(test$Predict7)

#Remove unused "levels" 
test$highland.lowland <- factor(test$highland.lowland)

table(test$Predict7, test$highland.lowland) # 96.77%
table(test$Predict7)


########################################



#Prediction

Unknown.df <- subset(Dr.J.data, highland.lowland == "?", select =c(1:11))
names(Unknown.df)
names(Unknown.df)[4] <- "GOL1"
names(Unknown.df)[5] <- "NOL2"
names(Unknown.df)[6] <- "WFB3"
names(Unknown.df)[7] <- "EKB4"
names(Unknown.df)[8] <- "DKS5"
names(Unknown.df)[9] <- "GLS6"
names(Unknown.df)[10] <- "FRC7"
names(Unknown.df)[11] <- "PAC8"

#Using model 1
Unknown.df$Prediction_Hi_Low <- predict(model1.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict <- ifelse( Unknown.df$Prediction_Hi_Low> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low<0.3,0,"NA"))

#Using model 2
Unknown.df$Prediction_Hi_Low2 <- predict(model2.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict2 <- ifelse( Unknown.df$Prediction_Hi_Low2> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low2<0.3,0,"NA"))

#Using model 3
Unknown.df$Prediction_Hi_Low3 <- predict(model3.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict3 <- ifelse( Unknown.df$Prediction_Hi_Low3> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low3<0.3,0,"NA"))

#Using model 4
Unknown.df$Prediction_Hi_Low4 <- predict(model4.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict4 <- ifelse( Unknown.df$Prediction_Hi_Low4> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low4<0.3,0,"NA"))

#Using model 5
Unknown.df$Prediction_Hi_Low5 <- predict(model5.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict5 <- ifelse( Unknown.df$Prediction_Hi_Low5> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low5<0.3,0,"NA"))

#Using model 6
Unknown.df$Prediction_Hi_Low6 <- predict(model6.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict6 <- ifelse( Unknown.df$Prediction_Hi_Low6> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low6<0.3,0,"NA"))

#Using model 7
Unknown.df$Prediction_Hi_Low7 <- predict(model7.glm, newdata = Unknown.df, type = 'response')
Unknown.df$Predict7 <- ifelse( Unknown.df$Prediction_Hi_Low7> 0.7, 1, ifelse(Unknown.df$Prediction_Hi_Low7<0.3,0,"NA"))

################################################
#tables

#model 1 table
table(test$Predict, test$highland.lowland)
table(test$Predict)

#model 2 table
table(test$Predict2, test$highland.lowland)
table(test$Predict2)

#model 3 table
table(test$Predict3, test$highland.lowland)
table(test$Predict3)

#model 4 table
table(test$Predict4, test$highland.lowland)
table(test$Predict4)

#model 5 table
table(test$Predict5, test$highland.lowland)
table(test$Predict5)
 
#model 6 table
table(test$Predict6, test$highland.lowland)
table(test$Predict6)

#model 7 table
table(test$Predict7, test$highland.lowland)
table(test$Predict7)

# Write predictions to file
write.csv(Unknown.df, file = "UnknownPreds.csv", row.names = FALSE)


