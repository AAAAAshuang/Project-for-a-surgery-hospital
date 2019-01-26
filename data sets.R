library(openxlsx)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)

demo = read.xlsx('ChartDemographics.xlsx', sheet = 1)
democb = read.xlsx('ChartDemographics.xlsx', sheet = 2)

service = read.xlsx('ServiceItemSummary.xlsx', sheet = 1)
servicecb = read.xlsx('ServiceItemSummary.xlsx', sheet = 2)

demo = distinct(demo, Per.Nbr, .keep_all = TRUE)
data = left_join(service, demo, by = c('Per.Nbr' = 'Per.Nbr'))
data$Per.Nbr = as.numeric(data$Per.Nbr)
subservice<-service[,1:10]
subservice$`Pat.Age.in.Years.(DOS)`=NULL

subservice2=subservice
subservice2$Encounter=NULL
subservice2$Enc.Dt=NULL
subservice2$Physician=NULL
subservice2$Fin.Class=NULL
subservice3<-subservice2%>%
  mutate(Loc_Name=ifelse(Loc.Name=='Encino','Encino Office',
                         ifelse(Loc.Name=='Pasadena','Pasadena Office',
                                ifelse(Loc.Name=='Santa Monica','Santa Monica Office',
                                       ifelse(Loc.Name=='Torrance','Torrance Office',
                                              ifelse(Loc.Name=='Valencia','Valencia Office',
                                                     Loc.Name))))))

subservice3$Loc.Name=NULL
subservice4=unique(subservice2[,2:5])
test<-data.frame(table(subservice4$Per.Nbr))

d2<-left_join(subservice4,demo,by=c("Per.Nbr"="Per.Nbr"))
test2<-data.frame(table(d2$Per.Nbr))
#通过test可猜测 serviceitemsummary中有些客户（14860减13497=1363名）是不在demo记录中的
d2<-d2%>%
  mutate(language=ifelse(Preferred.Language=='Spanish'|Preferred.Language=='Spanish;Castilian','Spanish',
                        ifelse(Preferred.Language=='Chinese'|Preferred.Language=='Mandarin', 'Chinese',
                               ifelse(Preferred.Language=='French','French',
                                      ifelse(Preferred.Language=='German','German',
                                             ifelse(Preferred.Language=='Japanese','Japanese',
                                                    ifelse(Preferred.Language=='Russian','Russian',
                                                           ifelse(Preferred.Language=='Korean','Korean',
                                                                  ifelse(Preferred.Language=='Arabic','Arabian',
                                                                         ifelse(Preferred.Language=='Thai','Thai',
                                                                                ifelse(Preferred.Language=='Somali','Somali',
                                                                                       ifelse(Preferred.Language=='Vietnamese','Vietnamese',
                                                                                              ifelse(Preferred.Language=='English','English',
                                                                                                     ifelse(Preferred.Language=='Central Khmer','Central Khmer',
                                                                                                            ifelse(is.na(Preferred.Language)==TRUE,'unknown',
                                                                                                                   'unknown')))))))))))))))


d2$Pat.Age=str_remove_all(d2$Pat.Age,' yrs.')
d2$Pat.Age=str_remove_all(d2$Pat.Age,' yr.')
d2$Pat.Age=ifelse(d2$Pat.Age=='10 mos.'|d2$Pat.Age=='11 mos.','1',d2$Pat.Age)

write.csv(d2,'demo2.csv')
#delete useless columns
write.csv(subservice3,'service.csv')

subservice5<-subservice3%>%
  group_by(Loc_Name)%>%
  summarise(count=n())

subservice5$zip=ifelse(subservice5$Loc_Name=='Encino Office','91316',
                       ifelse(subservice5$Loc_Name=='Valencia Office','91355',
                              ifelse(subservice5$Loc_Name=='Torrance Office','90505',
                                     ifelse(subservice5$Loc_Name=='Santa Monica Office','90403',
                                            ifelse(subservice5$Loc_Name=='Pasadena Office','91105',
                                                   ifelse(subservice5$Loc_Name=='Downtown Office'|subservice5$Loc_Name=='LA Office','90015',
                                                          'unknown'))))))

write.csv(subservice5,'service3.csv')

data$Migrant.Worker.Status = NULL
data$Homeless.Status = NULL
data$Language.Barrier= NULL
data$Church = NULL
data$Zip.y = NULL
data$Smk=NULL
data$Vet=NULL
# Only 8 people are self-identified as smokers. Others are unspecified as NA
# Only 2 people are self-identified as veteran. Others are unspecified as NA

# change types of columns to factor
data$Gen = as.factor(data$Gen)
data$Mar = as.factor(data$Mar)
data$Smk = as.factor(data$Smk)
data$Race =as.factor(data$Race)
data$Ethnicity = as.factor(data$Ethnicity)
data$Religion = as.factor(data$Religion)
data$Preferred.Language = as.factor(data$Preferred.Language)
data$Mrkt.Plan.Type.x = as.factor(data$Mrkt.Plan.Type.x)
data$Mrkt.Plan.Type.y = as.factor(data$Mrkt.Plan.Type.y)
data$Mrkt.Plan.Name.x = as.factor(data$Mrkt.Plan.Name.x)
data$Mrkt.Plan.Name.y = as.factor(data$Mrkt.Plan.Name.y)

data$Department = as.factor(data$Department)
data$Fin.Class = as.factor(data$Fin.Class)
data$Sv.It = as.factor(data$Sv.It)
data$Sv.It.Desc = as.factor(data$Sv.It.Desc)


data$Zip.x = as.numeric(data$Zip.x)

#office correction
data$Loc.Name<-ifelse(data$Loc.Name=='Encino','Encino Office',
                  ifelse(data$Loc.Name==''))
d3<-data%>%
  group_by(Per.Nbr)%>%
  summarise(totalpayment=sum(Pay.Amt))


############################################

service$Pat.Age=str_remove_all(service$Pat.Age,' yrs.')
service$Pat.Age=str_remove_all(service$Pat.Age,' yr.')
service$Pat.Age=ifelse(service$Pat.Age=='10 mos.'|service$Pat.Age=='11 mos.','1',service$Pat.Age)
service$Pat.Age=as.numeric(service$Pat.Age,na.rm=TRUE)
cosmetic_data<-service%>%
  filter(Type.of.Procedure=='Cosmetic')%>%
  group_by(Per.Nbr)%>%
  summarise(avg_pat.age=mean(Pat.Age,na.rm=TRUE),avg_dos.age=mean(`Pat.Age.in.Years.(DOS)`,na.rm = TRUE),count_of_service=n())

summary(cosmetic_data)


functional_data<-service%>%
  filter(Type.of.Procedure=='Functional')%>%
  group_by(Per.Nbr)%>%
  summarise(avg_pat.age=mean(Pat.Age,na.rm=TRUE),avg_dos.age=mean(`Pat.Age.in.Years.(DOS)`,na.rm = TRUE),count_of_service=n())
summary(functional_data)

write.csv(service,'only_service.csv')
