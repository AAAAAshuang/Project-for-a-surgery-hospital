library(dplyr)

cosmetic<-read.csv('Cosmetic_demographic.csv')
functional<-read.csv('Functional_demographic.csv')

summary(cosmetic$Race)
summary(functional$Race)

age_distribution<-cosmetic%>%
  filter(Race=='Asian')%>%
  group_by(Age)%>%
  summarise(number_age=n())


Asian<-cosmetic%>%
  filter(Race=='Asian')


demo = read.xlsx('ChartDemographics.xlsx', sheet = 1)
democb = read.xlsx('ChartDemographics.xlsx', sheet = 2)

service = read.xlsx('ServiceItemSummary.xlsx', sheet = 1)
servicecb = read.xlsx('ServiceItemSummary.xlsx', sheet = 2)