#### Task 1 ####
library(haven)
survey2021 <- read_sav("C:/Users/User/Desktop/ERM-RM-with-R-Python/Econ-ana-SPSS-in-R/field/fielddata2021.sav")

## the distribution of age 
library(ggplot2)
ggplot(data = survey2021, aes(x=age))+
  geom_histogram(bins = 30)

## mean, median and standard deviation for certain variables

description = function(variable){
  mean = mean(variable,na.rm=T)
  median = median(variable,na.rm=T)
  std = sd(variable,na.rm=T)
  table = list(mean = mean,median = median,std = std)
  return(table)
}


description(survey2021$wtp_dummy)
description(survey2021$wtp)
description(survey2021$male_dummy)
description(survey2021$age)
description(survey2021$dutch_dummy)
description(survey2021$days_campus_before)
description(survey2021$days_campus_now)
description(survey2021$length_study)
description(survey2021$length_work)
description(survey2021$appreciate)
description(survey2021$student_dummy)
# calculate faculties
for (i in grep('faculty',colnames(survey2021))){
  print(colnames(survey2021[,i]))
  print(description(unlist(survey2021[,i])))
}

## a histogram for the WTP for installing green roofs on campus buildings
ggplot(data = survey2021, aes(x=wtp))+
  geom_histogram(bins = 30)

## a scatter plot WTP vs. spent on campus before COVID-19
ggplot(data = survey2021, aes(x=days_campus_before,y = wtp))+
  geom_point()

#### Task 2 two-way tables and comparing means ####
## a two-way table with gender and the WTP dummy
library(tidyverse)
survey2021 %>% select(male_dummy,wtp_dummy) %>% table()
chisq.test(survey2021$male_dummy,survey2021$wtp_dummy)

## a bar chart: the mean WTP for males and females.
dat = survey2021 %>% group_by(male_dummy) %>% summarise(mean = mean(wtp))
ggplot(dat, aes(x=(male_dummy+1) , y=mean)) +
  geom_bar(stat="identity", fill="steelblue")+
  scale_x_discrete(limits=c("female", "male"))

## the appropriate mean comparison test (WTP vs. gender)
wilcox.test(survey2021$wtp_dummy~survey2021$male_dummy) # where y is numeric and x is A binary factor

## a scatter plot mean WTP vs. student_dummy
ggplot(survey2021, aes(x=factor(student_dummy,labels=c("staff","student")),
                       y=wtp))+
  geom_point()+
  labs(x = "")

##  a difference in rating of different benefits between males and females

wilcox.test(survey2021$benefit_species~survey2021$male_dummy)
wilcox.test(survey2021$benefit_stress~survey2021$male_dummy)
wilcox.test(survey2021$benefit_social~survey2021$male_dummy)
wilcox.test(survey2021$benefit_sustain~survey2021$male_dummy)
wilcox.test(survey2021$benefit_flood~survey2021$male_dummy)
wilcox.test(survey2021$benefit_edible~survey2021$male_dummy)
wilcox.test(survey2021$benefit_insulation~survey2021$male_dummy)


#### Task 3 regressions ####
## WTP correlates with time spent on campus before COVID and now

lm(wtp~days_campus_before,data=survey2021) %>% summary()
lm(wtp~days_campus_now,data=survey2021) %>% summary()

## WTP correlates with all

lm(wtp~days_campus_before+
     days_campus_now+
     appreciate+
     male_dummy+
     dutch_dummy+
     benefit_species+
     benefit_stress+
     benefit_social+
     benefit_sustain+
     benefit_flood+
     benefit_edible+
     benefit_insulation+
     student_dummy+
     faculty_sbe+
     faculty_science+
     faculty_behav+
     faculty_human+
     faculty_law+
     faculty_social+
     faculty_medic
     ,data=survey2021) %>% summary()

# chi-square test
chisq.test(survey2021$gender, survey2021$educ_university)

## two-way table with university education and mean income
survey2021 %>% select(educ_university,income) %>% table()
# chi-square test
chisq.test(survey2021$educ_university, survey2021$income)

# Wilcoxon rank-sum
wilcox.test(survey2021$income[survey2021$educ_university==1], 
            survey2021$income[survey2021$educ_university==0], alternative = "two.sided")
#t.test:mean1 > mean2
t.test(survey2021$income[survey2021$educ_university==1], 
       survey2021$income[survey2021$educ_university==0],alternative = "greater")
#t.test:mean1 != mean2
t.test(survey2021$income[survey2021$educ_university==1], 
       survey2021$income[survey2021$educ_university==0],alternative = "two.sided")

## What is higher ¡Vthe mean WTP for nature in the Netherlands or in the Caribbean Netherlands?
library(ggplot2)
ggplot(survey2021, aes(x= nl_wtp))+
  geom_histogram(bins = 10)

ggplot(survey2021, aes(x= car_wtp))+
  geom_histogram(bins = 10)

wilcox.test(survey2021$nl_wtp, 
            survey2021$car_wtp, alternative = "two.sided",paired = T)

print(c(mean(survey2021$nl_wtp,na.rm = T),
        mean(survey2021$car_wtp,na.rm = T)))

## Compare WTP for nature in Caribbean Netherlands between those who have travelled to Aruba, Bonaire and Curacao and those who have not.
wilcox.test(survey2021$car_wtp[survey2021$visit_aruba==1],
            survey2021$car_wtp[survey2021$visit_aruba==0],alternative = "two.sided")

wilcox.test(survey2021$car_wtp[survey2021$visit_bonaire==1],
            survey2021$car_wtp[survey2021$visit_bonaire==0],alternative = "two.sided")

wilcox.test(survey2021$car_wtp[survey2021$visit_curacao==1],
            survey2021$car_wtp[survey2021$visit_curacao==0],alternative = "two.sided")

#### Task 4 regressions ####
##WTP for nature in Netherlands onto income
lm1 = lm(nl_wtp~income,data = survey2021)
summary(lm1)

#plot this relationship
ggplot(survey2021,aes(x = income,y = nl_wtp))+
  geom_point()+
  geom_smooth(method='lm',se = FALSE)

## WTP for nature in the Netherlands onto income, age, education dummies (take primary education as base scenario) and levels of importance (the first questions from the survey).
lm2 = lm(nl_wtp~income+age+
           imp_traffic+
           imp_defence+
           imp_social+
           imp_health+
           imp_immigration+
           imp_nature+
           imp_education+
           imp_aid+
           imp_transport+
           imp_employment+
           imp_security+
           imp_euro+
           educ_secondary+
           educ_vocational+
           educ_college+
           educ_university
           ,data = survey2021)
summary(lm2)

#fitted mean WTP for someone who has a high school diploma and is earning 1500 Euros permonth
cat("WTP is ", 1500*0.005625,"euro per month")

##WTP for nature in the Caribbean Netherlands onto income, age, education (take primary education as base scenario), levels of importance and visits to the islands dummies.
lm3 = lm(car_wtp~income+age+
           imp_traffic+
           imp_defence+
           imp_social+
           imp_health+
           imp_immigration+
           imp_nature+
           imp_education+
           imp_aid+
           imp_transport+
           imp_employment+
           imp_security+
           imp_euro+
           educ_secondary+
           educ_vocational+
           educ_college+
           educ_university+
           visit_aruba+
           visit_bonaire+
           visit_curacao+
           visit_saba+
           visit_maarten+
           visit_eustaitus
         ,data = survey2021)
summary(lm3)
cat("WTP is ", 0,"since selected variables are not significant")

## Add the survey versions (choose one version as base) as covariates to both regressions
lm2.1 = lm(nl_wtp~income+age+
           imp_traffic+
           imp_defence+
           imp_social+
           imp_health+
           imp_immigration+
           imp_nature+
           imp_education+
           imp_aid+
           imp_transport+
           imp_employment+
           imp_security+
           imp_euro+
           educ_secondary+
           educ_vocational+
           educ_college+
           educ_university+
           NL2dummy+
           UK1dummy+
           UK2dummy
         ,data = survey2021)
summary(lm2.1)

lm3.1 = lm(car_wtp~income+age+
           imp_traffic+
           imp_defence+
           imp_social+
           imp_health+
           imp_immigration+
           imp_nature+
           imp_education+
           imp_aid+
           imp_transport+
           imp_employment+
           imp_security+
           imp_euro+
           educ_secondary+
           educ_vocational+
           educ_college+
           educ_university+
           visit_aruba+
           visit_bonaire+
           visit_curacao+
           visit_saba+
           visit_maarten+
           visit_eustaitus+
           NL2dummy+
           UK1dummy+
           UK2dummy
         ,data = survey2021)
summary(lm3.1)

# WTP C-NL with the survey version dummy variables
summary(lm3.1)$adj.r.squared

# WTP C-NL without the survey version dummy variables
summary(lm3)$adj.r.squared

## Add the dummy variables for the surveyors
lm3.2 = lm(car_wtp~income+age+
             imp_traffic+
             imp_defence+
             imp_social+
             imp_health+
             imp_immigration+
             imp_nature+
             imp_education+
             imp_aid+
             imp_transport+
             imp_employment+
             imp_security+
             imp_euro+
             educ_secondary+
             educ_vocational+
             educ_college+
             educ_university+
             visit_aruba+
             visit_bonaire+
             visit_curacao+
             visit_saba+
             visit_maarten+
             visit_eustaitus+
             NL2dummy+
             UK1dummy+
             UK2dummy+
             surveyor2+
             surveyor3+
             surveyor4+
             surveyor5+
             surveyor6+
             surveyor7+
             surveyor8+
             surveyor9+
             surveyor10+
             surveyor11+
             surveyor12+
             surveyor13+
             surveyor14+
             surveyor15+
             surveyor16+
             surveyor17+
             surveyor18+
             surveyor19+
             surveyor20+
             surveyor21+
             surveyor22+
             surveyor23+
             surveyor24+
             surveyor25+
             surveyor26+
             surveyor27+
             surveyor28+
             surveyor29+
             surveyor30
           ,data = survey2021)
summary(lm3.2)
