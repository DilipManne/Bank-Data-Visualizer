#1st question 
url<-"https://github.com/SavioSal/datasets/raw/master/Bank%20Churn_Modelling.csv"
data<-read.csv(url)
View(data)
library(ggplot2)

#The plot shows total no of male and female customers 
ggplot(data,aes(Gender)) + geom_bar()

#The plot shows count of male and female in different countries
ggplot(data,aes(Geography, fill = `Gender`)) + geom_bar()

#The plot represents the frequencies of ages of the customers 
ggplot(data,aes(Age)) + geom_freqpoly()+facet_wrap(~`Geography`)

#The plot shows the count of customers having different credit scores across different countries
ggplot(data,aes(CreditScore, fill = `Geography`)) + geom_histogram() +facet_wrap(~`Geography`)


#This shows blalance as per gender in different countries 
ggplot(data=data,aes(Balance, fill = `Gender`)) + geom_histogram()+facet_wrap(~`Geography`)





#2nd Question
#a)What is the average credit score of females and males in France?
data%>% select(CreditScore, Gender, Geography) %>% filter(Geography == "France") %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(Gender_Average = mean(CreditScore))


#b)What is the average credit score of people in the age brackets 20-30,31-40,41-50?
data %>% select(CreditScore, Age) %>% mutate(agegroup = case_when(Age >= 41  & Age <= 50 ~ '3', Age >= 31  & Age <= 40 ~ '2', Age >= 20  & Age <= 30 ~ '1')) %>%
  filter(agegroup == "1" | agegroup == '2' | agegroup == '3') %>%
  dplyr::group_by(agegroup) %>%
  dplyr::summarise(Age_Average = mean(CreditScore))


#c)What is the correlation between credit score and estimated salary?  
data %>% select(CreditScore, EstimatedSalary) %>% cor()
model <- lm(CreditScore ~Gender+Age+EstimatedSalary, data = data)

#d)printing the model
print(model)
summary(model)