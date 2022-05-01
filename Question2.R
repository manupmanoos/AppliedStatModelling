library(ggplot2)
library(jsonlite)
library(dplyr)
library(plyr)
library(MCMCpack)
library(RColorBrewer)
library(caret)

df <- read.csv("analysis.csv")

#selecting only the required columns
df<- df[,c("Combined.ACPL","Year","White.Num.Moves","Black.Num.Moves","Black.Player_ID","White.Player_ID","PreDeepBlue")]#"White.ACPL","Black.ACPL", 


#Feature Selection (scaling and correlation)
df_new <- df%>%mutate_if(is.numeric,scale)
df_new$PreDeepBlue [df_new$PreDeepBlue == "TRUE"] <- 1
df_new$PreDeepBlue [df_new$PreDeepBlue == "FALSE"] <- 0
summary(df_new)
Coorelations <- cor(df_new)
Coorelations



df$Black.Player_ID <- factor(df$Black.Player_ID)
df$White.Player_ID <- factor(df$White.Player_ID)
df$PreDeepBlue <- factor(df$PreDeepBlue)
summary(df)

hist(df$Combined.ACPL)
pairs(subset(df, select = c(Year, Combined.ACPL)))
cor(subset(df, select = c(Year, Combined.ACPL)))

boxplot(Combined.ACPL ~ Year, data = df)

#1(a)


lm_year <- lm(Combined.ACPL~Year, df)
summary(lm_year)
plot(lm_year)


df_to_predict  <- data.frame ("Year"  = c(2022:2032))
df_to_predict$Predicted.ACPL = predict(lm1, newdata = df_to_predict)
print(df_to_predict)
plot(Predicted.ACPL~Year, data = df_to_predict)




#1(b)


boxplot(Combined.ACPL ~ PreDeepBlue, data = df)

lm_chess_engines <- lm(Combined.ACPL~PreDeepBlue, df)
summary(lm_chess_engines)
plot(lm_chess_engines)




