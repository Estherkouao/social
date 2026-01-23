library(tidyr)
library(readr)
library(tibble)
library(ggplot2)
library(caret)
library(dplyr)
library(MASS)


#separation des donnees

set.seed(123)

data <- createDataPartition(predata$GAD_7_Severity, p = 0.8, list = F)
train = predata[data,] |> mutate(
  across(.col=where(is.numeric), .fns=scale)
)
train[,c(2,6,)]
test = predata[-data,]

# Convert Target to Ordered Factor
train$GAD_7_Severity <- factor(train$GAD_7_Severity,
                            levels = c("Minimal", "Mild", "Moderate", 
                                     "Severe"),
                            ordered = TRUE)


#model glm

modelglm <- glm(GAD_7_Severity~Age + Gender + User_Archetype + 
                  Late_Night_Usage + Daily_Screen_Time_Hours + 
                  Sleep_Duration_Hours, data = train, family = "binomial")
summary(modelglm)

modelnull <- glm(GAD_7_Severity ~ Gender + User_Archetype + Late_Night_Usage + 
                   Daily_Screen_Time_Hours + Sleep_Duration_Hours, data = train, family = "binomial")
summary(modelnull)
#var selection

modelA <- stepAIC(modelglm)

summary(modelA$model)
modelA$formula


