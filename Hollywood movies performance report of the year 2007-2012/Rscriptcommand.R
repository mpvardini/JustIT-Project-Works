df<- read.csv("/cloud/project/HollywoodsMostProfitableStories.csv")
View(df)
install.packages("tidyverse")
library(tidyverse)
str(df)
colSums(is.na(df)) 
na.omit(df)
head(df)
colSums(is.na(df))

df_clean <- df[complete.cases(df), ]
colSums(is.na(df_clean))
dim(df_clean[duplicated(df_clean$Film),])[1]
dim(df_clean[duplicated(df_clean$Film)])
df_clean$Film
df_clean[duplicated(df_clean$Film),]
dim(df_clean[duplicated(df_clean$Film),])
dim(df_clean[duplicated(df_clean$Film),])[1]
df_clean$Profitability <- round(df_clean$Profitability ,digit=2)
view(df_clean)
view(df)
df_clean$Worldwide.Gross <- round(df_clean$Worldwide.Gross ,digit=2)
view(df_clean)
dim(df_clean)
library(ggplot2)
ggplot(df_clean,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "blue",outlier.shape= 19)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,800))

Q1 <- quantile(df_clean$Profitability, .25)
Q3 <- quantile(df_clean$Profitability, .75)
IQR <- IQR(df_clean$Profitability)

no_outliers <- subset(df_clean, df_clean$Profitability> (Q1 - 1.5*IQR) & df_clean$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)

Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(df1)

summary(df1)
#bivariate analysis

ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

ggplot(df1, aes(x=Year)) + geom_bar()

write.csv(df1, "clean_df.csv")

View(df1)

ggplot(df1, aes(x=Film, y=Year)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(2006, 2012))+theme(axis.text.x = element_text(angle = 90))


ggplot(df1, aes(x=Profitability, y=Year)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(2006, 2012))+theme(axis.text.x = element_text(angle = 90))


ggplot(df1, aes(x=Profitability)) + geom_bar()

ggplot(df1, aes(x=Profitability, y=Year)) +geom_boxplot(outlier.colour= "blue",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(2006,2012))


ggplot(df1, aes(x=Film, y=Audience..score..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 100))+theme(axis.text.x = element_text(angle = 90))

view(no_outliers)

no_outliers %>%arrange(desc(Audience..score..))

topa_score <-no_outliers %>%arrange(desc(Audience..score..))

View(topa_score)

library(dplyr)

group_by(topa_score, Genre)

View(topa_score)

install.packages("janitor")
library(janitor)

get_dupes(topa_score, Genre)

install.packages(psych)
library(psych)
