install.packages("tidymodels")
install.packages("esquisse") 
install.packages("ggThemeAssist")
install.packages("corrplot")


library(tidyr)
library(readr)
library(tibble)
library(ggplot2)
library(caret)
library(tidymodels)
library(esquisse)
library(corrplot)
library(ggThemeAssist)

#chargement des donnees ####

rawdata <- read_csv("social_media_mental_health.csv")
summary(rawdata)

predata <- rawdata |> 
  mutate(
    across(
      .col = where(is.character) & !User_ID,
      .fns = as.factor
      ), 
    Late_Night_Usage = as.factor(Late_Night_Usage),
    Social_Comparison_Trigger = as.factor(Social_Comparison_Trigger)
  )
summary(predata)

#T test

t.test(data = predata, GAD_7_Score~Gender)

#anova
#Dominant_Content_Type
modelanova <- aov(data = predata, GAD_7_Score~Dominant_Content_Type)
summary(modelanova)

A <- TukeyHSD(modelanova)
A         

plot(A)

#User_Archetype
modelanova1 <- aov(data = predata, GAD_7_Score~User_Archetype)
summary(modelanova1)
B <- TukeyHSD(modelanova1)
B
plot(B)

                                                                                               
#Primary_Platform
modelanova2 <- aov(data = predata, GAD_7_Score~Primary_Platform)
summary(modelanova2)
C <- TukeyHSD(modelanova2)
C
plot(C)


#Activity_Type
t.test(data = predata, GAD_7_Score~Activity_Type)
summary(modelanova3)
D <- TukeyHSD(modelanova3)
D
plot(D)

#Echoue car ce ne sont pas des facteurs
#Late_Night_Usage
modelanova4 <- aov(data = predata, GAD_7_Score~Late_Night_Usage)
summary(modelanova4)
E <- TukeyHSD(modelanova4)
E
plot(E) 

#Social_Comparison_Trigger
modelanova5 <- aov(data = predata, GAD_7_Score~Social_Comparison_Trigger)
summary(modelanova5)
G <- TukeyHSD(modelanova5)
G
plot(G)

#Sleep_Duration_Hours
modelanova6 <- aov(data = predata, GAD_7_Score~Sleep_Duration_Hours)
summary(modelanova6)
I <- TukeyHSD(modelanova6)
I
plot(I)


ggplot(predata) +
  aes(x = Daily_Screen_Time_Hours) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()


#correlation
S <- predata |> select(where(is.numeric)) |> cor(use = "complete.obs")
corrplot(S, method= "circle")


#

