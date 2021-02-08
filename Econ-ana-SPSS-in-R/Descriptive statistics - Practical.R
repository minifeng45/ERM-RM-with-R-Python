#### Task 1 ####
install.packages("haven") # if you haven't install yet
library(haven)
survey2021 = read_sav("C:/Users/User/Downloads/survey2021.sav")
View(survey2021)

## What kind of variables do we have?
# use 'class' to know the data type
class(survey2021$nl_wtp) 
class(survey2021$car_wtp)

#### Task 2 ####
## Summarize variables
install.packages("tidyverse")
library(tidyverse)

survey2021 %>% summarise(nl_wtp_dummy =c(mean(nl_wtp_dummy),median(nl_wtp_dummy),sd(nl_wtp_dummy)),
                         car_wtp_dummy =c(mean(car_wtp_dummy),median(car_wtp_dummy),sd(car_wtp_dummy)),
                         nl_wtp =c(mean(nl_wtp,na.rm=TRUE),median(nl_wtp,na.rm=TRUE),sd(nl_wtp,na.rm=TRUE)), #na.rm =TRUE to ignore missing data
                         car_wtp =c(mean(car_wtp,na.rm=TRUE),median(car_wtp,na.rm=TRUE),sd(car_wtp,na.rm=TRUE)),
                         gender =c(mean(gender),median(gender),sd(gender)),
                         age =c(mean(age,na.rm=TRUE),median(age,na.rm=TRUE),sd(age,na.rm=TRUE)),
                         educ_primary =c(mean(educ_primary),median(educ_primary),sd(educ_primary)),
                         educ_secondary =c(mean(educ_secondary),median(educ_secondary),sd(educ_secondary)),
                         educ_vocational =c(mean(educ_vocational),median(educ_vocational),sd(educ_vocational)),
                         educ_college =c(mean(educ_college),median(educ_college),sd(educ_college)),
                         educ_university =c(mean(educ_university),median(educ_university),sd(educ_university)))

# Q: What is the sample size N? 
nrow(survey2021)

#### Task 3 ####
## a) bar graph for travel
install.packages("ggplot2")
library(ggplot2)
dat = survey2021 %>% group_by(imp_traffic) %>% summarise(wtp_mean = mean(nl_wtp,na.rm = TRUE)) #calculate the mean according to  all imp_traffic
ggplot(data = dat, aes(x= as.factor(imp_traffic), y = wtp_mean))+
  geom_bar(stat="identity")

## b) bar graph for six countries
#visit aruba
dat = survey2021 %>% group_by(visit_aruba) %>% summarise(wtp_mean = mean(car_wtp,na.rm = TRUE))
ggplot(data = dat, aes(x = as.factor(visit_aruba),y =  wtp_mean))+
         geom_bar(stat="identity")

#visit_bonaire
dat = survey2021 %>% group_by(visit_bonaire) %>% summarise(wtp_mean = mean(car_wtp,na.rm = TRUE))
ggplot(data = dat, aes(x = as.factor(visit_bonaire),y =  wtp_mean))+
  geom_bar(stat="identity")

#visit_curacao
dat = survey2021 %>% group_by(visit_curacao) %>% summarise(wtp_mean = mean(car_wtp,na.rm = TRUE))
ggplot(data = dat, aes(x = as.factor(visit_curacao),y =  wtp_mean))+
  geom_bar(stat="identity")

#visit_saba
dat = survey2021 %>% group_by(visit_saba) %>% summarise(wtp_mean = mean(car_wtp,na.rm = TRUE))
ggplot(data = dat, aes(x = as.factor(visit_saba),y =  wtp_mean))+
  geom_bar(stat="identity")

#visit_maarten
dat = survey2021 %>% group_by(visit_maarten) %>% summarise(wtp_mean = mean(car_wtp,na.rm = TRUE))
ggplot(data = dat, aes(x = as.factor(visit_maarten),y =  wtp_mean))+
  geom_bar(stat="identity")

#visit_eustaitus
dat = survey2021 %>% group_by(visit_eustaitus) %>% summarise(wtp_mean = mean(car_wtp,na.rm = TRUE))
ggplot(data = dat, aes(x = as.factor(visit_eustaitus),y =  wtp_mean))+
  geom_bar(stat="identity")

## c) histogram for WTP for nature in Caribbean Netherlands
ggplot(data = survey2021, aes(x = car_wtp)) +
  geom_histogram(bins=10) #bins = intervals, how many intervals for the data

## d) scatter plot for WTP for nature in Netherlands vs. how important people find nature and the environment
ggplot(data = survey2021, aes(x = imp_nature,y = nl_wtp)) +
  geom_point()

## calculate the correlation coefficient for two variables
# imp_nature
cor(survey2021$age,survey2021$nl_wtp,use="complete.obs") #complete.obs to avoid missing data
cor.test(survey2021$age,survey2021$nl_wtp,use="complete.obs")
# correlation between age and income
cor(survey2021$income,survey2021$age,use="complete.obs")
cor.test(survey2021$income,survey2021$age,use="complete.obs")
