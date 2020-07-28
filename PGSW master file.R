#####Prepare workspace#####
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#####Read in data#####
import <- drive_download(as_id('https://drive.google.com/file/d/1xOF84UYRhEuasZCM1QoRnWR0u_-V97JR/view?usp=sharing'), overwrite=TRUE)
1
read_1997 <- import('PGSW 1997.sav')
pgsw1997 <- tibble(1:2003)

import <- drive_download(as_id("https://drive.google.com/file/d/1TWrkWhPwbVLGHm3CPcOsxMPygcxWyC_0/view?usp=sharing"), overwrite=TRUE)
1
read_2001 <- import('PGSW 2001.sav')
pgsw2001 <- tibble(1:1794)

import <- drive_download(as_id("https://drive.google.com/file/d/1BwNXibqaKbI4xnGpf7AfGjaBi5vgeN7t/view?usp=sharing"), overwrite=TRUE)
1
read_2005 <- import('PGSW 2005.sav')
read_2005 <- read_2005[read_2005$wave==2,]
pgsw2005 <- tibble(1:1201)

import <- drive_download(as_id("https://drive.google.com/file/d/1rHjST-HBIgjEHxO4qE8dLeZiGqjLG5HN/view?usp=sharing"), overwrite=TRUE)
1
read_2007 <- import('PGSW 2007.sav')
pgsw2007 <- tibble(1:1817)

import <- drive_download(as_id("https://drive.google.com/file/d/1Y43Uyv5tSvlWij7N3OklHAo6xY-zNA4R/view?usp=sharing"), overwrite=TRUE)
1
read_2011 <- import('PGSW 2011.sav')
pgsw2011 <- tibble(1:1919)

import <- drive_download(as_id("https://drive.google.com/file/d/1Ua1tDfTlAXsXijPW8L2SYBHwlgRN4hcq/view?usp=sharing"), overwrite=TRUE)
1
read_2015 <- import('PGSW 2015.sav')
pgsw2015 <- tibble(1:1733)

import <- drive_download(as_id('https://drive.google.com/file/d/1Y7kjTpQkKyN_E0Ogj2yUfJu9gF4Dk98K/view?usp=sharing'), overwrite=TRUE)
1
read_2019 <- import('PGSW2019_CAPI.sav')
pgsw2019 <- tibble(1:2003)


#####General variables#####
colnames(pgsw1997) <- "n"
var_label(pgsw1997$n) <- "ID number"
pgsw1997$year <- 1997
pgsw1997$year <- as_factor(pgsw1997$year)
var_label(pgsw1997$year) <- "Year of election"
pgsw1997$weight <- read_1997$waga
var_label(pgsw1997$weight) <- "Weight"

colnames(pgsw2001) <- "n"
var_label(pgsw2001$n) <- "ID number"
pgsw2001$year <- 2001
pgsw2001$year <- as_factor(pgsw2001$year)
var_label(pgsw2001$year) <- "Year of election"
pgsw2001$weight <- read_2001$wghtsamp
var_label(pgsw2001$weight) <- "Weight"

colnames(pgsw2005) <- "n"
var_label(pgsw2005$n) <- "ID number"
pgsw2005$year <- 2005
pgsw2005$year <- as_factor(pgsw2005$year)
var_label(pgsw2005$year) <- "Year of election"
pgsw2005$weight <- read_2005$wght1
var_label(pgsw2005$weight) <- "Weight"

colnames(pgsw2007) <- "n"
var_label(pgsw2007$n) <- "ID number"
pgsw2007$year <- 2007
pgsw2007$year <- as_factor(pgsw2007$year)
var_label(pgsw2007$year) <- "Year of election"
pgsw2007$weight <- read_2007$waga
var_label(pgsw2007$weight) <- "Weight"

colnames(pgsw2011) <- "n"
var_label(pgsw2011$n) <- "ID number"
pgsw2011$year <- 2011
pgsw2011$year <- as_factor(pgsw2011$year)
var_label(pgsw2011$year) <- "Year of election"
pgsw2011$weight <- read_2011$waga
var_label(pgsw2011$weight) <- "Weight"

colnames(pgsw2015) <- "n"
var_label(pgsw2015$n) <- "ID number"
pgsw2015$year <- 2015
pgsw2015$year <- as_factor(pgsw2015$year)
var_label(pgsw2015$year) <- "Year of election"
pgsw2015$weight <- read_2015$WAGA
var_label(pgsw2015$weight) <- "Weight"

colnames(pgsw2019) <- "n"
var_label(pgsw2019$n) <- "ID number"
pgsw2019$year <- 2019
pgsw2019$year <- as_factor(pgsw2019$year)
var_label(pgsw2019$year) <- "Year of election"
pgsw2019$weight <- read_2019$waga
var_label(pgsw2019$weight) <- "Weight"


#####Socio-demographics#####
pgsw1997 <- mutate(pgsw1997, gender = factor(case_when(read_1997$sex==1 ~ "Male",
                                                       read_1997$sex==2 ~ "Female")))
pgsw1997$gender <- fct_relevel(pgsw1997$gender, "Male", "Female")
var_label(pgsw1997$gender) <- "Gender"

pgsw1997 <- mutate(pgsw1997, region = factor(case_when(read_1997$resid312==1 ~ "Village",
                                                       read_1997$resid312==2 ~ "Town < 19999",
                                                       read_1997$resid312==3 ~ "Town 20000 - 49999",
                                                       read_1997$resid312==4 ~ "Town 50000-99999",
                                                       read_1997$resid312==5 ~ "Town 100000 - 499999",
                                                       read_1997$resid312==6 ~ "Town > 500000")))
pgsw1997$region <- fct_relevel(pgsw1997$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw1997$region) <- "Size of region in which respondent lives"

pgsw1997 <- mutate(pgsw1997, age = 97-read_1997$age)
pgsw1997$age<- remove_all_labels(pgsw1997$age)
var_label(pgsw1997$age) <- "Age"

pgsw1997 <- mutate(pgsw1997, edlevel = factor(case_when(read_1997$educ==1 | read_1997$educ==2  ~ "Basic or none",
                                                        read_1997$educ==3 ~ "Basic vocational",
                                                        read_1997$educ==4 | read_1997$educ==5 | read_1997$educ==6 ~ "Secondary",
                                                        read_1997$educ==7 | read_1997$educ==8 | read_1997$educ==9  ~ "Higher")))
pgsw1997$edlevel <- fct_relevel(pgsw1997$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw1997$edlevel) <- "Level of education"

pgsw1997 <- mutate(pgsw1997, relig = factor(case_when(read_1997$churchat==1 | read_1997$churchat==2  ~ "Never",
                                                      read_1997$churchat==3 | read_1997$churchat==4  ~ "Seldom",
                                                      read_1997$churchat==5 | read_1997$churchat==6  ~ "Often",
                                                      read_1997$churchat==7 | read_1997$churchat==8  ~ "At least weekly")))
pgsw1997$relig <- fct_relevel(pgsw1997$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw1997$relig) <- "Religious attendance"

pgsw1997 <- mutate(pgsw1997, hincq = read_1997$hhincome) %>%
  mutate(hincq = replace(hincq, read_1997$hhincome %in% c(9700:9999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw1997$hincq) <- "Household income (quintile)"

pgsw1997 <- mutate(pgsw1997, occup = NA) %>%
  mutate(occup=replace(occup, read_1997$occup %in% c(41:93), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_1997$occup %in% c(22, 23, 24, 32, 33, 34), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_1997$occup %in% c(11, 12, 13, 21, 31), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_1997$occup %in% c(99), "Outside the labour market")) %>%
  as_factor(occup)
pgsw1997$occup <- fct_relevel(pgsw1997$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw1997$occup) <- "Occupation"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw1997, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(3,2,1))
# lc<-poLCA(f, data=pgsw1997, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "1997_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("1997_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw1997, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw1997$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw1997$ses <- fct_relevel(pgsw1997$ses, "Low", "Medium", "High")
var_label(pgsw1997$ses) <- "Socio-economic status"

pgsw1997 <- mutate(pgsw1997, union = NA) %>%
  mutate(union=replace(union, read_1997$tumemb97==4, "No")) %>%
  mutate(union=replace(union, read_1997$tumemb97 %in% c(2,3), "Yes")) %>%
  as_factor(union)
pgsw1997$union <- fct_relevel(pgsw1997$union, "No", "Yes")
var_label(pgsw1997$union) <- "Union membership"

pgsw2001 <- mutate(pgsw2001, gender = factor(case_when(read_2001$sex__310==1 ~ "Male",
                                                       read_2001$sex__310==2 ~ "Female")))
pgsw2001$gender <- fct_relevel(pgsw2001$gender, "Male", "Female")
var_label(pgsw2001$gender) <- "Gender"

pgsw2001 <- mutate(pgsw2001, region = factor(case_when(read_2001$resid350==1 ~ "Village",
                                                       read_2001$resid350==2 ~ "Town < 19999",
                                                       read_2001$resid350==3 ~ "Town 20000 - 49999",
                                                       read_2001$resid350==4 ~ "Town 50000-99999",
                                                       read_2001$resid350==5 ~ "Town 100000 - 499999",
                                                       read_2001$resid350==6 ~ "Town > 500000")))
pgsw2001$region <- fct_relevel(pgsw2001$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2001$region) <- "Size of region in which respondent lives"

pgsw2001 <- mutate(pgsw2001, age = 2001-read_2001$yrbir309)
var_label(pgsw2001$age) <- "Age"

pgsw2001 <- mutate(pgsw2001, edlevel = factor(case_when(read_2001$educ_311==1 | read_2001$educ_311==2 | read_2001$educ_311==3  ~ "Basic or none",
                                                        read_2001$educ_311==4 ~ "Basic vocational",
                                                        read_2001$educ_311 %in% c(5:9) ~ "Secondary",
                                                        read_2001$educ_311==10 | read_2001$educ_311==11  ~ "Higher")))
pgsw2001$edlevel <- fct_relevel(pgsw2001$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2001$edlevel) <- "Level of education"

pgsw2001 <- mutate(pgsw2001, relig = factor(case_when(read_2001$chrat365==1 | read_2001$chrat365==2  ~ "Never",
                                                      read_2001$chrat365==3 | read_2001$chrat365==4  ~ "Seldom",
                                                      read_2001$chrat365==5 | read_2001$chrat365==6  ~ "Often",
                                                      read_2001$chrat365==7 | read_2001$chrat365==8  ~ "At least weekly")))
pgsw2001$relig <- fct_relevel(pgsw2001$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2001$relig) <- "Religious attendance"

pgsw2001 <- mutate(pgsw2001, hincq = read_2001$hhinc343) %>%
  mutate(hincq = replace(hincq, read_2001$hhinc343 %in% c(99997:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2001$hincq) <- "Household income (quintile)"

pgsw2001 <- mutate(pgsw2001, occup = NA) %>%
  mutate(occup=replace(occup, read_2001$occup329 %in% c(41:93), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2001$occup329 %in% c(22, 23, 24, 32, 33, 34), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2001$occup329 %in% c(11, 12, 13, 21, 31), "Managers and professionals")) %>%
  mutate(occup=replace(occup, is.na(read_2001$occup329), "Outside the labour market")) %>%
  as_factor(occup)
pgsw2001$occup <- fct_relevel(pgsw2001$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2001$occup) <- "Occupation"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw2001, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(1,2,3))
# lc<-poLCA(f, data=pgsw2001, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "2001_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("2001_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw2001, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw2001$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw2001$ses <- fct_relevel(pgsw2001$ses, "Low", "Medium", "High")
var_label(pgsw2001$ses) <- "Socio-economic status"

pgsw2001 <- mutate(pgsw2001, union = NA) %>%
  mutate(union=replace(union, read_2001$tum01357==4, "No")) %>%
  mutate(union=replace(union, read_2001$tum01357 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2001$union <- fct_relevel(pgsw2001$union, "No", "Yes")
var_label(pgsw2001$union) <- "Union membership"

pgsw2005 <- mutate(pgsw2005, gender = factor(case_when(read_2005$m2==1 ~ "Male",
                                                       read_2005$m2==2 ~ "Female")))
pgsw2005$gender <- fct_relevel(pgsw2005$gender, "Male", "Female")
var_label(pgsw2005$gender) <- "Gender"

pgsw2005 <- mutate(pgsw2005, region = factor(case_when(read_2005$m27==1 ~ "Village",
                                                       read_2005$m27==2 ~ "Town < 19999",
                                                       read_2005$m27==3 ~ "Town 20000 - 49999",
                                                       read_2005$m27==4 ~ "Town 50000-99999",
                                                       read_2005$m27==5 ~ "Town 100000 - 499999",
                                                       read_2005$m27==6 ~ "Town > 500000")))
pgsw2005$region <- fct_relevel(pgsw2005$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2005$region) <- "Size of region in which respondent lives"

pgsw2005 <- mutate(pgsw2005, age = 2005-read_2005$m1)
var_label(pgsw2005$age) <- "Age"

pgsw2005 <- mutate(pgsw2005, edlevel = factor(case_when(read_2005$m3==1 | read_2005$m3==2 | read_2005$m3==3  ~ "Basic or none",
                                                        read_2005$m3==4 ~ "Basic vocational",
                                                        read_2005$m3 %in% c(5:9) ~ "Secondary",
                                                        read_2005$m3==10 | read_2005$m3==11  ~ "Higher")))
pgsw2005$edlevel <- fct_relevel(pgsw2005$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2005$edlevel) <- "Level of education"

pgsw2005 <- mutate(pgsw2005, relig = factor(case_when(read_2005$m36==1 | read_2005$m36==2  ~ "Never",
                                                      read_2005$m36==3 | read_2005$m36==4  ~ "Seldom",
                                                      read_2005$m36==5 | read_2005$m36==6  ~ "Often",
                                                      read_2005$m36==7 | read_2005$m36==8  ~ "At least weekly")))
pgsw2005$relig <- fct_relevel(pgsw2005$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2005$relig) <- "Religious attendance"

pgsw2005 <- mutate(pgsw2005, hincq = read_2005$m21) %>%
  mutate(hincq = replace(hincq, read_2005$m21 %in% c(99992:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2005$hincq) <- "Household income (quintile)"

pgsw2005 <- mutate(pgsw2005, occup = NA) %>%
  mutate(occup=replace(occup, read_2005$isco %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2005$isco %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2005$isco %in% c(2220, 2221, 2222, 2223, 2224, 2229, 2230, 2300, 
                                                    2310, 2320, 2350, 2351, 2359, 2420, 2421, 2422, 
                                                    2429, 2430, 2431, 2432, 2440, 2442, 2443, 2445, 
                                                    2451, 2460, 2330, 2331, 2332, 2340, 2352, 2444, 
                                                    2446, 2450, 2452, 2453, 2454, 2455, 3220, 3222, 
                                                    3223, 3224, 3226, 3229, 3230, 3231, 3232, 3300, 
                                                    3310, 3320, 3330, 3340, 3400, 3450, 3460, 3470, 
                                                    3472, 3473, 3474, 3475, 3480), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2005$isco %in% c(3221, 3225, 3227, 3228), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2005$isco==9999, "Outside the labour market")) %>%
  as_factor(occup)
pgsw2005$occup <- fct_relevel(pgsw2005$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2005$occup) <- "Occupation"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw2005, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(1,3,2))
# lc<-poLCA(f, data=pgsw2005, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "2005_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("2005_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw2005, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw2005$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw2005$ses <- fct_relevel(pgsw2005$ses, "Low", "Medium", "High")
var_label(pgsw2005$ses) <- "Socio-economic status"

pgsw2005 <- mutate(pgsw2005, union = NA) %>%
  mutate(union=replace(union, read_2005$m32==4, "No")) %>%
  mutate(union=replace(union, read_2005$m32 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2005$union <- fct_relevel(pgsw2005$union, "No", "Yes")
var_label(pgsw2005$union) <- "Union membership"

pgsw2007 <- mutate(pgsw2007, gender = factor(case_when(read_2007$m2==1 ~ "Male",
                                                       read_2007$m2==2 ~ "Female")))
pgsw2007$gender <- fct_relevel(pgsw2007$gender, "Male", "Female")
var_label(pgsw2007$gender) <- "Gender"

pgsw2007 <- mutate(pgsw2007, region = factor(case_when(read_2007$m33==1 ~ "Village",
                                                       read_2007$m33==2 ~ "Town < 19999",
                                                       read_2007$m33==3 ~ "Town 20000 - 49999",
                                                       read_2007$m33==4 ~ "Town 50000-99999",
                                                       read_2007$m33==5 ~ "Town 100000 - 499999",
                                                       read_2007$m33==6 ~ "Town > 500000")))
pgsw2007$region <- fct_relevel(pgsw2007$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2007$region) <- "Size of region in which respondent lives"

pgsw2007 <- mutate(pgsw2007, age = 2007-read_2007$m1)
pgsw2007$age<- remove_all_labels(pgsw2007$age)
var_label(pgsw2007$age) <- "Age"

pgsw2007 <- mutate(pgsw2007, edlevel = factor(case_when(read_2007$m3==1 | read_2007$m3==2 | read_2007$m3==3  ~ "Basic or none",
                                                        read_2007$m3==4 ~ "Basic vocational",
                                                        read_2007$m3 %in% c(5:9) ~ "Secondary",
                                                        read_2007$m3==10 | read_2007$m3==11  ~ "Higher")))
pgsw2007$edlevel <- fct_relevel(pgsw2007$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2007$edlevel) <- "Level of education"

pgsw2007 <- mutate(pgsw2007, relig = factor(case_when(read_2007$m42==1 | read_2007$m42==2  ~ "Never",
                                                      read_2007$m42==3 | read_2007$m42==4  ~ "Seldom",
                                                      read_2007$m42==5 | read_2007$m42==6  ~ "Often",
                                                      read_2007$m42==7 | read_2007$m42==8  ~ "At least weekly")))
pgsw2007$relig <- fct_relevel(pgsw2007$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2007$relig) <- "Religious attendance"

pgsw2007 <- mutate(pgsw2007, hincq = read_2007$m21) %>%
  mutate(hincq = replace(hincq, read_2007$m21 %in% c(99992:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2007$hincq) <- "Household income (quintile)"

pgsw2007 <- mutate(pgsw2007, occup = NA) %>%
  mutate(occup=replace(occup, read_2007$isco88 %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2007$isco88 %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2007$isco88 %in% c(2220, 2221, 2222, 2223, 2224, 2229, 2230, 2300, 
                                                      2310, 2320, 2350, 2351, 2359, 2420, 2421, 2422, 
                                                      2429, 2430, 2431, 2432, 2440, 2442, 2443, 2445, 
                                                      2451, 2460, 2330, 2331, 2332, 2340, 2352, 2444, 
                                                      2446, 2450, 2452, 2453, 2454, 2455, 3220, 3222, 
                                                      3223, 3224, 3226, 3229, 3230, 3231, 3232, 3300, 
                                                      3310, 3320, 3330, 3340, 3400, 3450, 3460, 3470, 
                                                      3472, 3473, 3474, 3475, 3480), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2007$isco88 %in% c(3221, 3225, 3227, 3228), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2007$isco88==9997, "Outside the labour market")) %>%
  as_factor(occup)
pgsw2007$occup <- fct_relevel(pgsw2007$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2007$occup) <- "Occupation"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw2007, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(3,2,1))
# lc<-poLCA(f, data=pgsw2007, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "2007_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("2007_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw2007, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw2007$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw2007$ses <- fct_relevel(pgsw2007$ses, "Low", "Medium", "High")
var_label(pgsw2007$ses) <- "Socio-economic status"

pgsw2007 <- mutate(pgsw2007, union = NA) %>%
  mutate(union=replace(union, read_2007$m38==4, "No")) %>%
  mutate(union=replace(union, read_2007$m38 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2007$union <- fct_relevel(pgsw2007$union, "No", "Yes")
var_label(pgsw2007$union) <- "Union membership"

pgsw2011 <- mutate(pgsw2011, gender = factor(case_when(read_2011$PLEC==1 ~ "Male",
                                                       read_2011$PLEC==2 ~ "Female")))
pgsw2011$gender <- fct_relevel(pgsw2011$gender, "Male", "Female")
var_label(pgsw2011$gender) <- "Gender"

pgsw2011 <- mutate(pgsw2011, region = factor(case_when(read_2011$KLM6==1 ~ "Village",
                                                       read_2011$KLM6==2 ~ "Town < 19999",
                                                       read_2011$KLM6==3 ~ "Town 20000 - 49999",
                                                       read_2011$KLM6==4 ~ "Town 50000-99999",
                                                       read_2011$KLM6==5 ~ "Town 100000 - 499999",
                                                       read_2011$KLM6==6 ~ "Town > 500000")))
pgsw2011$region <- fct_relevel(pgsw2011$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2011$region) <- "Size of region in which respondent lives"

pgsw2011 <- mutate(pgsw2011, age = read_2011$age)
var_label(pgsw2011$age) <- "Age"

pgsw2011 <- mutate(pgsw2011, edlevel = factor(case_when(read_2011$W1==1 | read_2011$W1==2 | read_2011$W1==3  ~ "Basic or none",
                                                        read_2011$W1==4 ~ "Basic vocational",
                                                        read_2011$W1 %in% c(5:9) ~ "Secondary",
                                                        read_2011$W1==10 | read_2011$W1==11  ~ "Higher")))
pgsw2011$edlevel <- fct_relevel(pgsw2011$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2011$edlevel) <- "Level of education"

pgsw2011 <- mutate(pgsw2011, relig = factor(case_when(read_2011$M42==1 | read_2011$M42==2  ~ "Never",
                                                      read_2011$M42==3 | read_2011$M42==4  ~ "Seldom",
                                                      read_2011$M42==5 | read_2011$M42==6  ~ "Often",
                                                      read_2011$M42==7 | read_2011$M42==8  ~ "At least weekly")))
pgsw2011$relig <- fct_relevel(pgsw2011$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2011$relig) <- "Religious attendance"

pgsw2011 <- mutate(pgsw2011, hincq = read_2011$M21) %>%
  mutate(hincq = replace(hincq, read_2011$M21 %in% c(99992:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2011$hincq) <- "Household income (quintile)"

pgsw2011 <- mutate(pgsw2011, occup = NA) %>%
  mutate(occup=replace(occup, read_2011$ISCO %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2011$ISCO %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2011$ISCO %in% c(2211, 2212, 2261, 2250, 2262, 2263, 2269, 1342, 2221, 
                                                    2222, 3221, 3222, 2310, 2320, 2330, 2341, 2342, 2352,
                                                    2351, 2353, 2354, 2355, 2356, 2359, 2611, 2612, 2619, 
                                                    2621, 2622, 2632, 2633, 2643, 2634, 2635, 2431, 2432, 
                                                    2641, 2642, 2651, 2652, 2653, 2654, 2655, 2636, 2263, 
                                                    3257, 2265, 2267, 3254, 2264, 2269, 3255, 3259, 2230, 
                                                    2266, 2267, 2269, 3259, 3221, 3222, 2230, 3230, 3413, 
                                                    2341, 2342, 2352, 2353, 2355, 2356, 2359, 3153, 3423, 
                                                    3435, 5165, 3355, 3411, 3412, 2642, 2656, 2652, 2653, 
                                                    2659, 3421, 3422, 3423, 3413), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2011$ISCO %in% c(2240, 3253, 3256, 3251, 3240, 3213), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2011$ISCO %in% c(9996:9999), "Outside the labour market")) %>%
  as_factor(occup)
pgsw2011$occup <- fct_relevel(pgsw2011$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2011$occup) <- "Occupation"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw2011, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(1,3,2))
# lc<-poLCA(f, data=pgsw2011, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "2011_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("2011_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw2011, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw2011$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw2011$ses <- fct_relevel(pgsw2011$ses, "Low", "Medium", "High")
var_label(pgsw2011$ses) <- "Socio-economic status"

pgsw2011 <- mutate(pgsw2011, union = NA) %>%
  mutate(union=replace(union, read_2011$M38==5, "No")) %>%
  mutate(union=replace(union, read_2011$M38 %in% c(1:4), "Yes")) %>%
  as_factor(union)
pgsw2011$union <- fct_relevel(pgsw2011$union, "No", "Yes")
var_label(pgsw2011$union) <- "Union membership"

pgsw2015 <- mutate(pgsw2015, gender = factor(case_when(read_2015$q12==1 ~ "Male",
                                                       read_2015$q12==2 ~ "Female")))
pgsw2015$gender <- fct_relevel(pgsw2015$gender, "Male", "Female")
var_label(pgsw2015$gender) <- "Gender"

pgsw2015 <- mutate(pgsw2015, region = factor(case_when(read_2015$q13==1 ~ "Village",
                                                       read_2015$q13==2 ~ "Town < 19999",
                                                       read_2015$q13==3 ~ "Town 20000 - 49999",
                                                       read_2015$q13==4 ~ "Town 50000-99999",
                                                       read_2015$q13==5 ~ "Town 100000 - 499999",
                                                       read_2015$q13==6 ~ "Town > 500000")))
pgsw2015$region <- fct_relevel(pgsw2015$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2015$region) <- "Size of region in which respondent lives"

pgsw2015 <- mutate(pgsw2015, age = 2015-read_2015$q14)
var_label(pgsw2015$age) <- "Age"

pgsw2015 <- mutate(pgsw2015, edlevel = factor(case_when(read_2015$q296==1 | read_2015$q296==2  ~ "Basic or none",
                                                        read_2015$q296==3 | read_2015$q296==4 | read_2015$q296==5 | read_2015$q296==7 ~ "Basic vocational",
                                                        read_2015$q296==6 | read_2015$q296==8 | read_2015$q296==9 ~ "Secondary",
                                                        read_2015$q296==10 | read_2015$q296==11 | read_2015$q296==12 ~ "Higher")))
pgsw2015$edlevel <- fct_relevel(pgsw2015$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2015$edlevel) <- "Level of education"

pgsw2015 <- mutate(pgsw2015, relig = factor(case_when(read_2015$q337==1 | read_2015$q337==2  ~ "Never",
                                                      read_2015$q337==3 | read_2015$q337==4  ~ "Seldom",
                                                      read_2015$q337==5 | read_2015$q337==6  ~ "Often",
                                                      read_2015$q337==7 | read_2015$q337==8  ~ "At least weekly")))
pgsw2015$relig <- fct_relevel(pgsw2015$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2015$relig) <- "Religious attendance"

pgsw2015 <- mutate(pgsw2015, income = case_when(read_2015$q326==1 | read_2015$q323>=0 & read_2015$q323<=300 ~ 1,
                                                read_2015$q326==2 | read_2015$q323>=301 & read_2015$q323<=500 ~ 2,
                                                read_2015$q326==3 | read_2015$q323>=501 & read_2015$q323<=750 ~ 3,
                                                read_2015$q326==4 | read_2015$q323>=751 & read_2015$q323<=1000 ~ 4,
                                                read_2015$q326==5 | read_2015$q323>=1001 & read_2015$q323<=1250 ~ 5,
                                                read_2015$q326==6 | read_2015$q323>=1251 & read_2015$q323<=1500 ~ 6,
                                                read_2015$q326==7 | read_2015$q323>=1501 & read_2015$q323<=1750 ~ 7,
                                                read_2015$q326==8 | read_2015$q323>=1751 & read_2015$q323<=2000 ~ 8,
                                                read_2015$q326==9 | read_2015$q323>=2001 & read_2015$q323<=2250 ~ 9,
                                                read_2015$q326==10 | read_2015$q323>=2251 & read_2015$q323<=2500 ~ 10,
                                                read_2015$q326==11 | read_2015$q323>=2501 & read_2015$q323<=2750 ~ 11,
                                                read_2015$q326==12 | read_2015$q323>=2751 & read_2015$q323<=3000 ~ 12,
                                                read_2015$q326==13 | read_2015$q323>=3001 & read_2015$q323<=3500 ~ 13,
                                                read_2015$q326==14 | read_2015$q323>=3501 & read_2015$q323<=4000 ~ 14,
                                                read_2015$q326==15 | read_2015$q323>=4001 & read_2015$q323<=4500 ~ 15,
                                                read_2015$q326==16 | read_2015$q323>=4501 & read_2015$q323<=5000 ~ 16,
                                                read_2015$q326==17 | read_2015$q323>=5001 & read_2015$q323<=6000 ~ 17,
                                                read_2015$q326==18 | read_2015$q323>=6001 & read_2015$q323<=8000 ~ 18,
                                                read_2015$q326==19 | read_2015$q323>=8001 & read_2015$q323<=10000 ~ 19,
                                                read_2015$q326==20 | read_2015$q323>=10000 & read_2015$q323<=35000 ~ 20))
var_label(pgsw2015$income) <- "Income"

pgsw2015 <- mutate(pgsw2015, hincq = read_2015$q323) %>%
  mutate(hincq, hincq = ntile(hincq, 5)) %>%
  mutate(hincq=replace(hincq, read_2015$q326 %in% c(1:6), 1)) %>%
  mutate(hincq=replace(hincq, read_2015$q326 %in% c(7:8), 2)) %>%
  mutate(hincq=replace(hincq, read_2015$q326 %in% c(9:12), 3)) %>%
  mutate(hincq=replace(hincq, read_2015$q326 %in% c(13:16), 4)) %>%
  mutate(hincq=replace(hincq, read_2015$q326 %in% c(17:20), 5)) %>%
  as_factor(hincq)
var_label(pgsw2015$hincq) <- "Household income (quintile)"

pgsw2015 <- mutate(pgsw2015, occup = NA) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_1 %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_1 %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_1 %in% c(2211, 2212, 261, 2250, 2262, 2263, 2269, 1342, 2221, 2222, 
                                                        3221, 3222, 2310, 2320, 2330, 2341, 2342, 2352, 2351, 2353, 
                                                        2354, 2355, 2356, 2359, 2611, 2612, 2619, 2621, 2622, 2632, 
                                                        2633, 2643, 2634, 2635, 2431, 2432, 2641, 2642, 2651, 2652, 
                                                        2653, 2654, 2655, 2636, 2263, 3257, 2265, 2267, 3254, 2264, 
                                                        2269, 3255, 3259, 2230, 2266, 2267, 2269, 3259, 3221, 3222, 
                                                        2230, 3230, 3413, 2341, 2342, 2352, 2353, 2355, 2356, 2359, 
                                                        3153, 3423, 3435, 5165, 3355, 3411, 3412, 2642, 2656, 2652, 
                                                        2653, 2659, 3421, 3422, 3423, 3413), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_1 %in% c(2240, 3253, 3256, 3251, 3240, 3213), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_2 %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_2 %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_2 %in% c(2211, 2212, 261, 2250, 2262, 2263, 2269, 1342, 2221, 2222, 
                                                        3221, 3222, 2310, 2320, 2330, 2341, 2342, 2352, 2351, 2353, 
                                                        2354, 2355, 2356, 2359, 2611, 2612, 2619, 2621, 2622, 2632, 
                                                        2633, 2643, 2634, 2635, 2431, 2432, 2641, 2642, 2651, 2652, 
                                                        2653, 2654, 2655, 2636, 2263, 3257, 2265, 2267, 3254, 2264, 
                                                        2269, 3255, 3259, 2230, 2266, 2267, 2269, 3259, 3221, 3222, 
                                                        2230, 3230, 3413, 2341, 2342, 2352, 2353, 2355, 2356, 2359, 
                                                        3153, 3423, 3435, 5165, 3355, 3411, 3412, 2642, 2656, 2652, 
                                                        2653, 2659, 3421, 3422, 3423, 3413), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2015$ISCO08_2 %in% c(2240, 3253, 3256, 3251, 3240, 3213), "Managers and professionals")) %>%
  mutate(occup=replace(occup, is.na(read_2015$ISCO08_1 | read_2015$ISCO08_2), "Outside the labour market")) %>%
  as_factor(occup)
var_label(pgsw2015$occup) <- "Occupation"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw2015, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(1,3,2))
# lc<-poLCA(f, data=pgsw2015, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "2015_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("2015_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw2015, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw2015$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw2015$ses <- fct_relevel(pgsw2015$ses, "Low", "Medium", "High")
var_label(pgsw2015$ses) <- "Socio-economic status"

pgsw2015 <- mutate(pgsw2015, union = NA) %>%
  mutate(union=replace(union, read_2015$q333==5, "No")) %>%
  mutate(union=replace(union, read_2015$q333 %in% c(1:4), "Yes")) %>%
  as_factor(union)
var_label(pgsw2015$union) <- "Union membership"

pgsw2019$month <- dplyr::recode_factor(read_2019$D01a, 
                                       `1` = "January", 
                                       `2` = "February", 
                                       `3` = "March", 
                                       `4` = "April", 
                                       `5` = "May",
                                       `6` = "June", 
                                       `7` = "July", 
                                       `8` = "August", 
                                       `9` = "September", 
                                       `10` = "October",
                                       `11` = "November", 
                                       `12` = "December")
var_label(pgsw2019$month) <- "Date of birth: month"

pgsw2019 <- mutate(pgsw2019, age = 2019-read_2019$D01b)
pgsw2019$age<- remove_all_labels(pgsw2019$age)
var_label(pgsw2019$age) <- "Age"

pgsw2019 <- mutate(pgsw2019, gender = factor(case_when(read_2019$D02==1 ~ "Male",
                                                       read_2019$D02==2 ~ "Female")))
pgsw2019$gender <- fct_relevel(pgsw2019$gender, "Male", "Female")
var_label(pgsw2019$gender) <- "Gender"

pgsw2019 <- mutate(pgsw2019, edlevel = factor(case_when(read_2019$D03==1 | read_2019$D03==2  ~ "Basic or none",
                                                        read_2019$D03==3 | read_2019$D03==4 ~ "Lower secondary",
                                                        read_2019$D03==5 | read_2019$D03==6 ~ "Upper secondary",
                                                        read_2019$D03==7 | read_2019$D03==8 | read_2019$D03==9  ~ "Higher")))
pgsw2019$edlevel <- fct_relevel(pgsw2019$edlevel, "Basic or none", "Lower secondary", "Upper secondary", "Higher")
var_label(pgsw2019$edlevel) <- "Level of education"

pgsw2019$marital <- dplyr::recode_factor(read_2019$D04, 
                                         `1` = "Married or living together as married", 
                                         `2` = "Widowed", 
                                         `3` = "Divorced or separated", 
                                         `4` = "Single, never married")
var_label(pgsw2019$marital) <- "Marital or civil union status"

pgsw2019$union <- dplyr::recode_factor(read_2019$D05, 
                                       `1` = "A member of a union", 
                                       `2` = "Not a member of a union")
var_label(pgsw2019$union) <- "Belong to a trade union or not"

pgsw2019$employ <- dplyr::recode_factor(read_2019$D06, 
                                        `1` = "Employed: full time", 
                                        `2` = "Employed: part time", 
                                        `3` = "Employed: less than 15 hours", 
                                        `4` = "Helping family member",
                                        `5` = "Unemployed",
                                        `6` = "Student, in school, in vocational training",
                                        `7` = "Retired",
                                        `8` = "Housewife, homemaker, home duties",
                                        `9` = "Permanently disabled",
                                        `10` = "Other, not in labour force")
var_label(pgsw2019$employ) <- "Current employment status"

pgsw2019 <- mutate(pgsw2019, occup = NA) %>%
  mutate(occup=replace(occup, read_2019$D07 %in% c(10:39), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read_2019$D07 %in% c(40:96), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read_2019$D07 %in% c(22, 23, 26, 32, 34), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read_2019$D07==99, "Outside the labour market")) %>%
  as_factor(occup)
var_label(pgsw2019$occup) <- "Occupation"

pgsw2019$socstat <- dplyr::recode_factor(read_2019$D07a, 
                                         `1` = "White collar", 
                                         `2` = "Worker", 
                                         `3` = "Farmer", 
                                         `4` = "Self-employed")
var_label(pgsw2019$socstat) <- "Socio-economic status"

pgsw2019$pubpriv <- dplyr::recode_factor(read_2019$D08, 
                                         `1` = "Public sector", 
                                         `2` = "Private sector", 
                                         `3` = "Mixed", 
                                         `4` = "Third sector / non-profit sector")
var_label(pgsw2019$pubpriv) <- "Employment type: public or private"

pgsw2019<- mutate(pgsw2019, income = case_when(read_2019$D09a==1 | read_2019$D09>=0 & read_2019$D09<=300 ~ 1,
                                               read_2019$D09a==2 | read_2019$D09>=301 & read_2019$D09<=500 ~ 2,
                                               read_2019$D09a==3 | read_2019$D09>=501 & read_2019$D09<=750 ~ 3,
                                               read_2019$D09a==4 | read_2019$D09>=751 & read_2019$D09<=1000 ~ 4,
                                               read_2019$D09a==5 | read_2019$D09>=1001 & read_2019$D09<=1250 ~ 5,
                                               read_2019$D09a==6 | read_2019$D09>=1251 & read_2019$D09<=1500 ~ 6,
                                               read_2019$D09a==7 | read_2019$D09>=1501 & read_2019$D09<=1750 ~ 7,
                                               read_2019$D09a==8 | read_2019$D09>=1751 & read_2019$D09<=2000 ~ 8,
                                               read_2019$D09a==9 | read_2019$D09>=2001 & read_2019$D09<=2250 ~ 9,
                                               read_2019$D09a==10 | read_2019$D09>=2251 & read_2019$D09<=2500 ~ 10,
                                               read_2019$D09a==11 | read_2019$D09>=2501 & read_2019$D09<=2750 ~ 11,
                                               read_2019$D09a==12 | read_2019$D09>=2751 & read_2019$D09<=3000 ~ 12,
                                               read_2019$D09a==13 | read_2019$D09>=3001 & read_2019$D09<=3500 ~ 13,
                                               read_2019$D09a==14 | read_2019$D09>=3501 & read_2019$D09<=4000 ~ 14,
                                               read_2019$D09a==15 | read_2019$D09>=4001 & read_2019$D09<=4500 ~ 15,
                                               read_2019$D09a==16 | read_2019$D09>=4501 & read_2019$D09<=5000 ~ 16,
                                               read_2019$D09a==17 | read_2019$D09>=5001 & read_2019$D09<=6000 ~ 17,
                                               read_2019$D09a==18 | read_2019$D09>=6001 & read_2019$D09<=8000 ~ 18,
                                               read_2019$D09a==19 | read_2019$D09>=8001 & read_2019$D09<=10000 ~ 19,
                                               read_2019$D09a==20 | read_2019$D09>=10000 & read_2019$D09<=40000 ~ 20))
var_label(pgsw2019$income) <- "Income"

pgsw2019$hincq <- as_factor(ntile(pgsw2019$income, 5))
var_label(pgsw2019$hincq) <- "Household income (quintile)"

pgsw2019$regdenom <- dplyr::recode_factor(read_2019$D10, 
                                          `1101` = "Roman Catholic", 
                                          `1102` = "Eastern (Greek Rite) Catholic churches", 
                                          `1600` = "Eastern Orthodox", 
                                          `1200` = "Protestant",
                                          `1504` = "Jehovah's Witness",
                                          `8100` = "Agnostic",
                                          `8200` = "Atheist",
                                          `8300` = "None",
                                          `9600` = "Other")
var_label(pgsw2019$regdenom) <- "Religious denomination"

pgsw2019 <- mutate(pgsw2019, relig = factor(case_when(read_2019$D11==1 ~ "Never",
                                                      read_2019$D11==2 | read_2019$D11==3  ~ "Seldom",
                                                      read_2019$D11==4 | read_2019$D11==5  ~ "Often",
                                                      read_2019$D11==6 ~ "At least weekly")))
pgsw2019$relig <- fct_relevel(pgsw2019$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2019$relig) <- "Religious attendance"

pgsw2019$regdenom <- dplyr::recode_factor(read_2019$D10, 
                                          `1101` = "Roman Catholic", 
                                          `1102` = "Eastern (Greek Rite) Catholic churches", 
                                          `1600` = "Eastern Orthodox", 
                                          `1200` = "Protestant",
                                          `1504` = "Jehovah's Witness",
                                          `8100` = "Agnostic",
                                          `8200` = "Atheist",
                                          `8300` = "None",
                                          `9600` = "Other")
var_label(pgsw2019$regdenom) <- "Religious denomination"

#D13 - ethnicity, not recoded

#D14 - country of birth, not recoded

pgsw2019$bioparent <- dplyr::recode_factor(read_2019$D15, 
                                           `1` = "Yes", 
                                           `2` = "No")
var_label(pgsw2019$bioparent) <- "Was either biological parent born outside the country?"

#D16 - language usually spoken at home, not recoded

pgsw2019$voivod <- read_2019$D17
var_label(pgsw2019$voivod) <- "Region of residence (voivodship)"

pgsw2019$elecdist <- read_2019$D18
var_label(pgsw2019$elecdist) <- "Primary electoral district"

pgsw2019 <- mutate(pgsw2019, region = factor(case_when(read_2019$wlk=="wieś" ~ "Village",
                                                         read_2019$wlk=="miasto do 10tys" ~ "Town < 19999",
                                                         read_2019$wlk=="miasto 10-20tys" ~ "Town < 19999",
                                                         read_2019$wlk=="miasto 20-50tys" ~ "Town 20000 - 49999",
                                                         read_2019$wlk=="miasto 50-100tys" ~ "Town 50000-99999",
                                                         read_2019$wlk=="miasto 100-200tys" ~ "Town 100000 - 499999",
                                                         read_2019$wlk=="miasto 200-500tys" ~ "Town 100000 - 499999",
                                                         read_2019$wlk=="miasto 500tys+" ~ "Town > 500000")))
pgsw2019$region <- fct_relevel(pgsw2019$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                                 "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2019$region) <- "Size of region in which respondent lives"

pgsw2019$numbhous <- read_2019$D20
var_label(pgsw2019$numbhous) <- "Number in household"

# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw2019, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(1,2,3))
# lc<-poLCA(f, data=pgsw2019, nclass=3, nrep=1, maxiter=10000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "2019_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("2019_ses_starting_values.RData")
lc<-poLCA(f, data=pgsw2019, nclass=3, nrep=1, maxiter=10000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw2019$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw2019$ses <- fct_relevel(pgsw2019$ses, "Low", "Medium", "High")
var_label(pgsw2019$ses) <- "Socio-economic status"

#####Voting behaviour#####
pgsw1997$voted <- recode_factor(read_1997$pt97_005,
                                `2` = "No",
                                `1` = "Yes",)
pgsw1997$voted <- fct_drop(pgsw1997$voted)
var_label(pgsw1997$voted) <- "Voted in most recent parliamentary election"

pgsw1997$votefor <- recode_factor(read_1997$vt97_006,
                                  `5` = "AWS",
                                  `4` = "UW",
                                  `6` = "SLD",
                                  `7` = "PSL",
                                  `9` = "ROP",
                                  `99` = "Did not vote")
pgsw1997$votefor <- fct_drop(pgsw1997$votefor)
pgsw1997$votefor <- fct_expand(pgsw1997$votefor, "Other")
pgsw1997$votefor <- replace(pgsw1997$votefor, read_1997$vt97_006 %in% c(1,2,3,8,10:94), "Other")
var_label(pgsw1997$votefor) <- "Party voted for in most recent parliamentary election"

pgsw1997$votefor_t <- recode_factor(read_1997$vt97_006,
                                    `5` = "Right",
                                    `4` = "Liberal",
                                    `6` = "Left",
                                    `9` = "Right",
                                    `7` = "Other",
                                    `99` = "Did not vote")
pgsw1997$votefor_t <- replace(pgsw1997$votefor_t, read_1997$vt97_006 %in% c(1,2,3,8,10:94), "Other")
pgsw1997$votefor_t <- fct_relevel(pgsw1997$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw1997$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw1997$voted_pr <- recode_factor(read_1997$pt93_244,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw1997$voted_pr <- fct_drop(pgsw1997$voted_pr)
var_label(pgsw1997$voted_pr) <- "Voted in previous parliamentary election"

pgsw1997$votefor_pr <- recode_factor(read_1997$vt93_245,
                                     `1` = "PC",
                                     `2` = "Ojcz",
                                     `3` = "PL",
                                     `4` = "KPN",
                                     `5` = "SLD",
                                     `6` = "PSL",
                                     `7` = "KLD",
                                     `8` = "NSZZ",
                                     `9` = "UD",
                                     `10` = "BBWR",
                                     `11` = "UP",
                                     `12` = "UPR")
pgsw1997$votefor_pr <- fct_expand(pgsw1997$votefor_pr, "Other")
pgsw1997$votefor_pr <- replace(pgsw1997$votefor_pr, read_1997$vt93_245 %in% c(15:24), "Other")
pgsw1997$votefor_pr <- fct_expand(pgsw1997$votefor_pr, "Did not vote")
pgsw1997$votefor_pr <- replace(pgsw1997$votefor_pr, pgsw1997$voted_pr=="No", "Did not vote")
var_label(pgsw1997$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw1997$votefor_t_pr <- recode_factor(read_1997$vt93_245,
                                       `1` = "Right",
                                       `2` = "Right",
                                       `3` = "Right",
                                       `4` = "Right",
                                       `5` = "Left",
                                       `6` = "Other",
                                       `7` = "Liberal",
                                       `8` = "Right",
                                       `9` = "Liberal",
                                       `10` = "Right",
                                       `11` = "Left",
                                       `12` = "Right")
pgsw1997$votefor_t_pr <- fct_expand(pgsw1997$votefor_t_pr, "Other")
pgsw1997$votefor_t_pr <- replace(pgsw1997$votefor_t_pr, read_1997$vt93_245 %in% c(15:24), "Other")
pgsw1997$votefor_t_pr <- fct_expand(pgsw1997$votefor_t_pr, "Did not vote")
pgsw1997$votefor_t_pr <- replace(pgsw1997$votefor_t_pr, pgsw1997$voted_pr=="No", "Did not vote")
pgsw1997$votefor_t_pr <- fct_relevel(pgsw1997$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw1997$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw1997 <- mutate(pgsw1997, stability = if_else(pgsw1997$votefor_t=="Right" & pgsw1997$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw1997$votefor_t=="Left" & pgsw1997$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw1997$votefor_t=="Liberal" & pgsw1997$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw1997$votefor_t=="Other" & pgsw1997$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw1997$votefor_t=="Did not vote" & pgsw1997$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw1997$stability <- fct_relevel(pgsw1997$stability, "Unstable", "Stable")
var_label(pgsw1997$stability) <- "Stability of voting behaviour (type)"

pgsw1997 <- mutate(pgsw1997, partyid = factor(case_when(read_1997$var3==2 ~ "No",
                                                        read_1997$var3==1 ~ "Yes",
                                                        read_1997$var13==1 ~ "Yes")))
var_label(pgsw1997$partyid) <- "Party identification"

pgsw1997$vtpres <- recode_factor(read_1997$pt95_240,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw1997$vtpres) <- "Voted in first round of most recent presidential election"

pgsw1997 <- mutate(pgsw1997, vtforpres = factor(case_when(read_1997$vt95_241==1 ~ "Bubel",
                                                          read_1997$vt95_241==2 ~ "Gronkiewicz-Waltz",
                                                          read_1997$vt95_241==3 ~ "Korwin-Mikke",
                                                          read_1997$vt95_241==5 ~ "Kuroń",
                                                          read_1997$vt95_241==6 ~ "Kwaśniewski",
                                                          read_1997$vt95_241==7 ~ "Lepper",
                                                          read_1997$vt95_241==8 ~ "Olszewski",
                                                          read_1997$vt95_241==9 ~ "Pawlak",
                                                          read_1997$vt95_241==10 ~ "Pietrzak",
                                                          read_1997$vt95_241==12 ~ "Wałęsa",
                                                          read_1997$vt95_241==13 ~ "Zieliński")))
pgsw1997$vtforpres <- fct_expand(pgsw1997$vtforpres, "Did not vote")
pgsw1997$vtforpres <- replace(pgsw1997$vtforpres, read_1997$pt95_240==2, "Did not vote")
var_label(pgsw1997$vtforpres) <- "Candidate voted for in first round of most recent presidential election"

pgsw1997$vtpres_2 <- recode_factor(read_1997$pt95_242,
                                   `2` = "No",
                                   `1` = "Yes")
var_label(pgsw1997$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw1997 <- mutate(pgsw1997, vtforpres_2 = factor(case_when(read_1997$vt95_243==1 ~ "Kwaśniewski",
                                                            read_1997$vt95_243==2 ~ "Wałęsa",
                                                            read_1997$pt95_242==2 ~ "Did not vote")))
pgsw1997$vtforpres_2 <- fct_relevel(pgsw1997$vtforpres_2, "Kwaśniewski", "Wałęsa", "Did not vote")
var_label(pgsw1997$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"

pgsw2001$voted <- recode_factor(read_2001$pt01_017,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2001$voted <- fct_drop(pgsw2001$voted)
var_label(pgsw2001$voted) <- "Voted in most recent parliamentary election"

pgsw2001$votefor <- recode_factor(read_2001$vt01_018,
                                  `1` = "SLD-UP",
                                  `2` = "AWSP",
                                  `3` = "UW",
                                  `4` = "SRP",
                                  `5` = "PiS",
                                  `6` = "PSL",
                                  `7` = "PO",
                                  `10` = "LPR")
pgsw2001$votefor <- fct_drop(pgsw2001$votefor)
pgsw2001$votefor <- fct_expand(pgsw2001$votefor, "Other")
pgsw2001$votefor <- replace(pgsw2001$votefor, read_2001$vt01_018 %in% c(8,11,13,14,15), "Other")
pgsw2001$votefor <- fct_expand(pgsw2001$votefor, "Did not vote")
pgsw2001$votefor <- replace(pgsw2001$votefor, pgsw2001$voted=="No", "Did not vote")
var_label(pgsw2001$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2001$votefor_t <- recode_factor(read_2001$vt01_018,
                                    `1` = "Left",
                                    `2` = "Right",
                                    `3` = "Liberal",
                                    `4` = "Left",
                                    `5` = "Right",
                                    `6` = "Other",
                                    `7` = "Liberal",
                                    `10` = "Right")
pgsw2001$votefor_t <- replace(pgsw2001$votefor_t, read_2001$vt01_018 %in% c(8,11,13,14,15), "Other")
pgsw2001$votefor_t <- fct_expand(pgsw2001$votefor_t, "Did not vote")
pgsw2001$votefor_t <- replace(pgsw2001$votefor_t, pgsw2001$voted=="No", "Did not vote")
pgsw2001$votefor_t <- fct_relevel(pgsw2001$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2001$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2001$voted_pr <- recode_factor(read_2001$pt97_030,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2001$voted_pr <- fct_drop(pgsw2001$voted_pr)
var_label(pgsw2001$voted_pr) <- "Voted in previous parliamentary election"

pgsw2001$votefor_pr <- recode_factor(read_2001$vt97_031,
                                     `1` = "UP",
                                     `2` = "N-Ch-D BdP",
                                     `3` = "KPEiR RP",
                                     `4` = "UW",
                                     `5` = "AWS",
                                     `6` = "SLD",
                                     `7` = "PSL",
                                     `8` = "UPR",
                                     `9` = "ROP",
                                     `10` = "KPEiR")
pgsw2001$votefor_pr <- fct_expand(pgsw2001$votefor_pr, "Other")
pgsw2001$votefor_pr <- replace(pgsw2001$votefor_pr, read_2001$vt97_031 %in% c(12,13,21), "Other")
pgsw2001$votefor_pr <- fct_expand(pgsw2001$votefor_pr, "Did not vote")
pgsw2001$votefor_pr <- replace(pgsw2001$votefor_pr, pgsw2001$voted_pr=="No", "Did not vote")
var_label(pgsw2001$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2001$votefor_t_pr <- recode_factor(read_2001$vt97_031,
                                       `1` = "Left",
                                       `2` = "Right",
                                       `3` = "Right",
                                       `4` = "Liberal",
                                       `5` = "Right",
                                       `6` = "Left",
                                       `7` = "Other",
                                       `8` = "Right",
                                       `9` = "Right",
                                       `10` = "Left")
pgsw2001$votefor_t_pr <- replace(pgsw2001$votefor_t_pr, read_2001$vt97_031 %in% c(12,13,21), "Other")
pgsw2001$votefor_t_pr <- fct_expand(pgsw2001$votefor_t_pr, "Did not vote")
pgsw2001$votefor_t_pr <- replace(pgsw2001$votefor_t_pr, pgsw2001$voted_pr=="No", "Did not vote")
pgsw2001$votefor_t_pr <- fct_relevel(pgsw2001$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2001$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw2001 <- mutate(pgsw2001, stability = if_else(pgsw2001$votefor_t=="Right" & pgsw2001$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2001$votefor_t=="Left" & pgsw2001$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2001$votefor_t=="Liberal" & pgsw2001$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2001$votefor_t=="Other" & pgsw2001$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2001$votefor_t=="Did not vote" & pgsw2001$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2001$stability <- fct_relevel(pgsw2001$stability, "Unstable", "Stable")
var_label(pgsw2001$stability) <- "Stability of voting behaviour (type)"

pgsw2001 <- mutate(pgsw2001, partyid = factor(case_when(read_2001$parid041==1 ~ "Yes",
                                                        read_2001$parid041==2 ~ "No",
                                                        read_2001$parid041==7 ~ "Don't know")))
pgsw2001$partyid <- fct_relevel(pgsw2001$partyid, "Yes", "No", "Don't know")
var_label(pgsw2001$partyid) <- "Party identification"

pgsw2005$voted <- recode_factor(read_2005$p29,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2005$voted <- fct_drop(pgsw2005$voted)
var_label(pgsw2005$voted) <- "Voted in most recent parliamentary election"

pgsw2005$votefor <- recode_factor(read_2005$p31,
                                  `1` = "RPat",
                                  `2` = "PPP",
                                  `3` = "LPR",
                                  `4` = "Dem",
                                  `5` = "SdPL",
                                  `6` = "PiS",
                                  `7` = "SLD",
                                  `8` = "PO",
                                  `10` = "PSL",
                                  `12` = "UPR",
                                  `15` = "SRP")
pgsw2005$votefor <- fct_drop(pgsw2005$votefor)
pgsw2005$votefor <- fct_expand(pgsw2005$votefor, "Other")
pgsw2005$votefor <- replace(pgsw2005$votefor, read_2005$p31 %in% c(9, 14, 17, 90), "Other")
pgsw2005$votefor <- fct_expand(pgsw2005$votefor, "Did not vote")
pgsw2005$votefor[pgsw2005$voted=="No"] <- "Did not vote"
var_label(pgsw2005$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2005$votefor_t <- recode_factor(read_2005$p31,
                                    `1` = "Right",
                                    `2` = "Left",
                                    `3` = "Right",
                                    `4` = "Liberal",
                                    `5` = "Left",
                                    `6` = "Right",
                                    `7` = "Left",
                                    `8` = "Liberal",
                                    `10` = "Other",
                                    `12` = "Right",
                                    `15` = "Left")
pgsw2005$votefor_t <- fct_drop(pgsw2005$votefor_t)
pgsw2005$votefor_t <- replace(pgsw2005$votefor_t, read_2005$p31 %in% c(9, 14, 17, 90), "Other")
pgsw2005$votefor_t <- fct_expand(pgsw2005$votefor_t, "Did not vote")
pgsw2005$votefor_t[pgsw2005$voted=="No"] <- "Did not vote"
pgsw2005$votefor_t <- fct_relevel(pgsw2005$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2005$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2005$voted_pr <- recode_factor(read_2005$p38,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2005$voted_pr <- fct_drop(pgsw2005$voted_pr)
var_label(pgsw2005$voted_pr) <- "Voted in previous parliamentary election"

pgsw2005$votefor_pr <- recode_factor(read_2005$p39,
                                     `1` = "SLD-UP",
                                     `2` = "AWSP",
                                     `3` = "UW",
                                     `4` = "SRP",
                                     `5` = "PiS",
                                     `6` = "PSL",
                                     `7` = "PO",
                                     `10` = "LPR")
pgsw2005$votefor_pr <- fct_drop(pgsw2005$votefor_pr)
pgsw2005$votefor_pr <- fct_expand(pgsw2005$votefor_pr, "Other")
pgsw2005$votefor_pr <- replace(pgsw2005$votefor_pr, read_2005$p39 %in% c(8,9,11,12,14,15), "Other")
pgsw2005$votefor_pr <- fct_expand(pgsw2005$votefor_pr, "Did not vote")
pgsw2005$votefor_pr[pgsw2005$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2005$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2005$votefor_t_pr <- recode_factor(read_2005$p39,
                                       `1` = "Left",
                                       `2` = "Right",
                                       `3` = "Liberal",
                                       `4` = "Left",
                                       `5` = "Right",
                                       `6` = "Other",
                                       `7` = "Liberal",
                                       `10` = "Right")
pgsw2005$votefor_t_pr <- fct_drop(pgsw2005$votefor_t_pr)
pgsw2005$votefor_t_pr <- replace(pgsw2005$votefor_t_pr, read_2005$p39 %in% c(8,9,11,12,14,15), "Other")
pgsw2005$votefor_t_pr <- fct_expand(pgsw2005$votefor_t_pr, "Did not vote")
pgsw2005$votefor_t_pr[pgsw2005$voted_pr=="No"] <- "Did not vote"
pgsw2005$votefor_t_pr <- fct_relevel(pgsw2005$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2005$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw2005 <- mutate(pgsw2005, stability = if_else(pgsw2005$votefor_t=="Right" & pgsw2005$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2005$votefor_t=="Left" & pgsw2005$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2005$votefor_t=="Liberal" & pgsw2005$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2005$votefor_t=="Other" & pgsw2005$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2005$votefor_t=="Did not vote" & pgsw2005$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2005$stability <- fct_relevel(pgsw2005$stability, "Unstable", "Stable")
var_label(pgsw2005$stability) <- "Stability of voting behaviour (type)"

pgsw2005 <- mutate(pgsw2005, partyid = factor(case_when(read_2005$r22==1 ~ "Yes",
                                                        read_2005$r22==2 ~ "No",
                                                        read_2005$r22==7 ~ "Don't know")))
pgsw2005$partyid <- fct_relevel(pgsw2005$partyid, "Yes", "No", "Don't know")
var_label(pgsw2005$partyid) <- "Party identification"

pgsw2005$vtpres <- recode_factor(read_2005$r36,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw2005$vtpres) <- "Voted in first round of most recent presidential election"

pgsw2005 <- mutate(pgsw2005, vtforpres = factor(case_when(read_2005$r37==1 ~ "Bochniarz",
                                                          read_2005$r37==2 ~ "Borowski",
                                                          read_2005$r37==4 ~ "Giertych",
                                                          read_2005$r37==5 ~ "Ilasz",
                                                          read_2005$r37==6 ~ "Kaczyński",
                                                          read_2005$r37==7 ~ "Kalinowski",
                                                          read_2005$r37==8 ~ "Korwin-Mikke",
                                                          read_2005$r37==9 ~ "Lepper",
                                                          read_2005$r37==12 ~ "Słomka",
                                                          read_2005$r37==13 ~ "Tusk",
                                                          read_2005$r37==99 ~ "Did not vote")))
var_label(pgsw2005$vtforpres) <- "Candidate voted for in first round of most recent presidential election"

pgsw2005$vtpres_2 <- recode_factor(read_2005$r41,
                                   `2` = "No",
                                   `1` = "Yes")
var_label(pgsw2005$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw2005 <- mutate(pgsw2005, vtforpres_2 = factor(case_when(read_2005$r42==1 ~ "Kaczyński",
                                                            read_2005$r42==2 ~ "Tusk",
                                                            read_2005$r41==2 ~ "Did not vote")))
var_label(pgsw2005$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"

pgsw2007$voted <- recode_factor(read_2007$c26,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2007$voted <- fct_drop(pgsw2007$voted)
var_label(pgsw2007$voted) <- "Voted in most recent parliamentary election"

pgsw2007$votefor <- recode_factor(read_2007$c28,
                                  `1` = "PPP",
                                  `2` = "LPR",
                                  `3` = "PiS",
                                  `4` = "PO",
                                  `5` = "PSL",
                                  `6` = "SRP",
                                  `8` = "LiD")
pgsw2007$votefor <- fct_drop(pgsw2007$votefor)
pgsw2007$votefor <- fct_expand(pgsw2007$votefor, "Other")
pgsw2007$votefor <- replace(pgsw2007$votefor, read_2007$c28 %in% c(7,9), "Other")
pgsw2007$votefor <- fct_expand(pgsw2007$votefor, "Did not vote")
pgsw2007$votefor[pgsw2007$voted=="No"] <- "Did not vote"
var_label(pgsw2007$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2007$votefor_t <- recode_factor(read_2007$c28,
                                    `1` = "Left",
                                    `2` = "Right",
                                    `3` = "Right",
                                    `4` = "Liberal",
                                    `5` = "Other",
                                    `6` = "Left",
                                    `8` = "Left")
pgsw2007$votefor_t <- fct_drop(pgsw2007$votefor_t)
pgsw2007$votefor_t <- replace(pgsw2007$votefor_t, read_2007$c28 %in% c(7,9), "Other")
pgsw2007$votefor_t <- fct_expand(pgsw2007$votefor_t, "Did not vote")
pgsw2007$votefor_t[pgsw2007$voted=="No"] <- "Did not vote"
pgsw2007$votefor_t <- fct_relevel(pgsw2007$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2007$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2007$voted_pr <- recode_factor(read_2007$c32,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2007$voted_pr <- fct_drop(pgsw2007$voted_pr)
var_label(pgsw2007$voted_pr) <- "Voted in previous parliamentary election"

pgsw2007$votefor_pr <- recode_factor(read_2007$c33,
                                     `1` = "RPat",
                                     `3` = "LPR",
                                     `4` = "Dem",
                                     `5` = "SdRP",
                                     `6` = "PiS",
                                     `7` = "SLD",
                                     `8` = "PO",
                                     `9` = "PN",
                                     `10` = "PSL",
                                     `12` = "UPR",
                                     `15` = "SRP")
pgsw2007$votefor_pr <- fct_drop(pgsw2007$votefor_pr)
pgsw2007$votefor_pr <- fct_expand(pgsw2007$votefor_pr, "Did not vote")
pgsw2007$votefor_pr[pgsw2007$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2007$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2007$votefor_t_pr <- recode_factor(read_2007$c33,
                                       `1` = "Right",
                                       `3` = "Right",
                                       `4` = "Liberal",
                                       `5` = "Left",
                                       `6` = "Right",
                                       `7` = "Left",
                                       `8` = "Liberal",
                                       `9` = "Right",
                                       `10` = "Other",
                                       `12` = "Right",
                                       `15` = "Left")
pgsw2007$votefor_t_pr <- fct_drop(pgsw2007$votefor_t_pr)
pgsw2007$votefor_t_pr <- fct_expand(pgsw2007$votefor_t_pr, "Did not vote")
pgsw2007$votefor_t_pr[pgsw2007$voted_pr=="No"] <- "Did not vote"
pgsw2007$votefor_t_pr <- fct_relevel(pgsw2007$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2007$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw2007 <- mutate(pgsw2007, stability = if_else(pgsw2007$votefor_t=="Right" & pgsw2007$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2007$votefor_t=="Left" & pgsw2007$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2007$votefor_t=="Liberal" & pgsw2007$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2007$votefor_t=="Other" & pgsw2007$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2007$votefor_t=="Did not vote" & pgsw2007$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2007$stability <- fct_relevel(pgsw2007$stability, "Unstable", "Stable")
var_label(pgsw2007$stability) <- "Stability of voting behaviour (type)"

pgsw2007 <- mutate(pgsw2007, partyid = factor(case_when(read_2007$c22==1 ~ "Yes",
                                                        read_2007$c22==2 ~ "No",
                                                        read_2007$c22==7 ~ "Don't know")))
pgsw2007$partyid <- fct_relevel(pgsw2007$partyid, "Yes", "No", "Don't know")
var_label(pgsw2007$partyid) <- "Party identification"

pgsw2011$voted <- recode_factor(read_2011$C8a,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2011$voted <- fct_drop(pgsw2011$voted)
var_label(pgsw2011$voted) <- "Voted in most recent parliamentary election"

pgsw2011$votefor <- recode_factor(read_2011$C8b,
                                  `1` = "PiS",
                                  `2` = "PJN",
                                  `3` = "SLD",
                                  `4` = "RP",
                                  `5` = "PSL",
                                  `6` = "PPP",
                                  `7` = "PO",
                                  `8` = "SRP",
                                  `9` = "UPR")
pgsw2011$votefor <- fct_drop(pgsw2011$votefor)
pgsw2011$votefor <- fct_expand(pgsw2011$votefor, "Other")
pgsw2011$votefor <- replace(pgsw2011$votefor, read_2011$C8b %in% c(10,11), "Other")
pgsw2011$votefor <- fct_expand(pgsw2011$votefor, "Did not vote")
pgsw2011$votefor[pgsw2011$voted=="No"] <- "Did not vote"
var_label(pgsw2011$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2011$votefor_t <- recode_factor(read_2011$C8b,
                                    `1` = "Right",
                                    `2` = "Right",
                                    `3` = "Left",
                                    `4` = "Left",
                                    `5` = "Other",
                                    `6` = "Left",
                                    `7` = "Liberal",
                                    `8` = "Left",
                                    `9` = "Right")
pgsw2011$votefor_t <- fct_drop(pgsw2011$votefor_t)
pgsw2011$votefor_t <- replace(pgsw2011$votefor_t, read_2011$C8b %in% c(10,11), "Other")
pgsw2011$votefor_t <- fct_expand(pgsw2011$votefor_t, "Did not vote")
pgsw2011$votefor_t[pgsw2011$voted=="No"] <- "Did not vote"
pgsw2011$votefor_t <- fct_relevel(pgsw2011$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2011$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2011$voted_pr <- recode_factor(read_2011$C9,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2011$voted_pr <- fct_drop(pgsw2011$voted_pr)
var_label(pgsw2011$voted_pr) <- "Voted in previous parliamentary election"

pgsw2011$votefor_pr <- recode_factor(read_2011$C9b,
                                     `1` = "PPP",
                                     `2` = "LPR",
                                     `3` = "PiS",
                                     `4` = "PO",
                                     `5` = "PSL",
                                     `6` = "SRP",
                                     `8` = "LiD")
pgsw2011$votefor_pr <- fct_drop(pgsw2011$votefor_pr)
pgsw2011$votefor_pr <- fct_expand(pgsw2011$votefor_pr, "Other")
pgsw2011$votefor_pr <- replace(pgsw2011$votefor_pr, read_2011$C9b %in% c(7,10), "Other")
pgsw2011$votefor_pr <- fct_expand(pgsw2011$votefor_pr, "Did not vote")
pgsw2011$votefor_pr[pgsw2011$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2011$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2011$votefor_t_pr <- recode_factor(read_2011$C9b,
                                       `1` = "Left",
                                       `2` = "Right",
                                       `3` = "Right",
                                       `4` = "Liberal",
                                       `5` = "Other",
                                       `6` = "Left",
                                       `8` = "Left")
pgsw2011$votefor_t_pr <- fct_drop(pgsw2011$votefor_t_pr)
pgsw2011$votefor_t_pr <- replace(pgsw2011$votefor_t_pr, read_2011$C9b %in% c(7,10), "Other")
pgsw2011$votefor_t_pr <- fct_expand(pgsw2011$votefor_t_pr, "Did not vote")
pgsw2011$votefor_t_pr[pgsw2011$voted_pr=="No"] <- "Did not vote"
pgsw2011$votefor_t_pr <- fct_relevel(pgsw2011$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2011$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw2011 <- mutate(pgsw2011, stability = if_else(pgsw2011$votefor_t=="Right" & pgsw2011$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2011$votefor_t=="Left" & pgsw2011$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2011$votefor_t=="Liberal" & pgsw2011$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2011$votefor_t=="Other" & pgsw2011$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2011$votefor_t=="Did not vote" & pgsw2011$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2011$stability <- fct_relevel(pgsw2011$stability, "Unstable", "Stable")
var_label(pgsw2011$stability) <- "Stability of voting behaviour (type)"

pgsw2011 <- mutate(pgsw2011, partyid = factor(case_when(read_2011$C19==1 ~ "Yes",
                                                        read_2011$C19==5 ~ "No",
                                                        read_2011$C19==7 ~ "Don't know")))
pgsw2011$partyid <- fct_relevel(pgsw2011$partyid, "Yes", "No", "Don't know")
var_label(pgsw2011$partyid) <- "Party identification"

pgsw2015$voted <- recode_factor(read_2015$q31,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2015$voted <- fct_drop(pgsw2015$voted)
var_label(pgsw2015$voted) <- "Voted in most recent parliamentary election"

pgsw2015$votefor <- recode_factor(read_2015$q32,
                                  `1` = "PiS",
                                  `2` = "PO",
                                  `3` = "Razem",
                                  `4` = "KORWiN",
                                  `5` = "PSL",
                                  `6` = "ZL",
                                  `7` = "Kukiz'15",
                                  `8` = "Nowoczesna")
pgsw2015$votefor <- fct_drop(pgsw2015$votefor)
pgsw2015$votefor <- fct_expand(pgsw2015$votefor, "Did not vote")
pgsw2015$votefor[pgsw2015$voted=="No"] <- "Did not vote"
var_label(pgsw2015$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2015$votefor_t <- recode_factor(read_2015$q32,
                                    `1` = "Right",
                                    `2` = "Liberal",
                                    `3` = "Left",
                                    `4` = "Right",
                                    `5` = "Other",
                                    `6` = "Left",
                                    `7` = "Right",
                                    `8` = "Liberal")
pgsw2015$votefor_t <- fct_drop(pgsw2015$votefor_t)
pgsw2015$votefor_t <- fct_expand(pgsw2015$votefor_t, "Did not vote")
pgsw2015$votefor_t[pgsw2015$voted=="No"] <- "Did not vote"
pgsw2015$votefor_t <- fct_relevel(pgsw2015$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2015$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2015$voted_pr <- recode_factor(read_2015$q38,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2015$voted_pr <- fct_drop(pgsw2015$voted_pr)
var_label(pgsw2015$voted_pr) <- "Voted in previous parliamentary election"

pgsw2015$votefor_pr <- recode_factor(read_2015$q39,
                                     `1` = "PiS",
                                     `2` = "PJN",
                                     `3` = "SLD",
                                     `4` = "RP",
                                     `5` = "PSL",
                                     `6` = "PO",
                                     `7` = "Other")
pgsw2015$votefor_pr <- fct_drop(pgsw2015$votefor_pr)
pgsw2015$votefor_pr <- fct_expand(pgsw2015$votefor_pr, "Did not vote")
pgsw2015$votefor_pr[pgsw2015$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2015$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2015$votefor_t_pr <- recode_factor(read_2015$q39,
                                       `1` = "Right",
                                       `2` = "Right",
                                       `3` = "Left",
                                       `4` = "Liberal",
                                       `5` = "Other",
                                       `6` = "Liberal",
                                       `7` = "Other")
pgsw2015$votefor_t_pr <- fct_drop(pgsw2015$votefor_t_pr)
pgsw2015$votefor_t_pr <- fct_expand(pgsw2015$votefor_t_pr, "Did not vote")
pgsw2015$votefor_t_pr[pgsw2015$voted_pr=="No"] <- "Did not vote"
pgsw2015$votefor_t_pr <- fct_relevel(pgsw2015$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2015$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw2015 <- mutate(pgsw2015, stability = if_else(pgsw2015$votefor_t=="Right" & pgsw2015$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2015$votefor_t=="Left" & pgsw2015$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2015$votefor_t=="Liberal" & pgsw2015$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2015$votefor_t=="Other" & pgsw2015$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2015$votefor_t=="Did not vote" & pgsw2015$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2015$stability <- fct_relevel(pgsw2015$stability, "Unstable", "Stable")
var_label(pgsw2015$stability) <- "Stability of voting behaviour (type)"

pgsw2015 <- mutate(pgsw2015, partyid = factor(case_when(read_2015$q105==1 ~ "Yes",
                                                        read_2015$q105==2 ~ "No",
                                                        read_2015$q105==7 ~ "Don't know")))
pgsw2015$partyid <- fct_relevel(pgsw2015$partyid, "Yes", "No", "Don't know")
var_label(pgsw2015$partyid) <- "Party identification"

pgsw2015 <- mutate(pgsw2015, vtpres = factor(if_else(read_2015$q115==95, "No", "Yes"))) %>%
  mutate(vtpres=replace(vtpres, read_2015$q115==9, NA)) %>%
  mutate(vtpres=replace(vtpres, read_2015$q115==10, NA)) %>%
  mutate(vtpres=replace(vtpres, read_2015$q115==11, NA))
var_label(pgsw2015$vtpres) <- "Voted in first round of most recent presidential election"

pgsw2015 <- mutate(pgsw2015, vtforpres = factor(case_when(read_2015$q115==1 ~ "Komorowski",
                                                          read_2015$q115==2 ~ "Duda",
                                                          read_2015$q115==3 ~ "Kukiz",
                                                          read_2015$q115==4 ~ "Korwin-Mikke",
                                                          read_2015$q115==5 ~ "Ogórek",
                                                          read_2015$q115==6 ~ "Other",
                                                          read_2015$q115==7 ~ "Did not vote")))
var_label(pgsw2015$vtforpres) <- "Candidate voted for in first round of most recent presidential election"

pgsw2015 <- mutate(pgsw2015, vtpres_2 = factor(if_else(read_2015$q118==3, "No", "Yes"))) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2015$q118==4, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2015$q118==6, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2015$q118==7, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2015$q118==8, NA))
var_label(pgsw2015$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw2015 <- mutate(pgsw2015, vtforpres_2 = factor(case_when(read_2015$q118==1 ~ "Komorowski",
                                                            read_2015$q118==2 ~ "Duda",
                                                            read_2015$q118==3 ~ "Did not vote")))
var_label(pgsw2015$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"

pgsw2019$voted <- recode_factor(read_2019$Q12LHa,
                                `5` = "No",
                                `6` = "No",
                                `1` = "Yes")
var_label(pgsw2019$voted) <- "Voted in most recent election to the Sejm"

pgsw2019$votefor <- recode_factor(read_2019$Q12LHb,
                                  `2` = "PiS",
                                  `5` = "KO",
                                  `3` = "Lewica",
                                  `1` = "PSL-Kukiz",
                                  `4` = "Konfederacja")
pgsw2019$votefor <- fct_expand(pgsw2019$votefor, "Did not vote")
pgsw2019$votefor[pgsw2019$voted=="No"] <- "Did not vote"
var_label(pgsw2019$votefor) <- "Party voted for in most recent election to the Sejm"

pgsw2019$votefor_t <- recode_factor(read_2019$Q12LHb,
                                    `2` = "Right",
                                    `5` = "Liberal",
                                    `3` = "Left",
                                    `1` = "Other",
                                    `4` = "Right")
pgsw2019$votefor_t <- fct_drop(pgsw2019$votefor_t)
pgsw2019$votefor_t <- fct_expand(pgsw2019$votefor_t, "Did not vote")
pgsw2019$votefor_t[pgsw2019$voted=="No"] <- "Did not vote"
pgsw2019$votefor_t <- fct_relevel(pgsw2019$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2019$votefor_t) <- "Type of party voted for in most recent election to the Sejm"

pgsw2019$votedsen <- recode_factor(read_2019$Q12UHa,
                                   `5` = "No",
                                   `6` = "No",
                                   `1` = "Yes")
var_label(pgsw2019$votedsen) <- "Voted in most recent election to the Senate"

pgsw2019$votefor_s <- recode_factor(read_2019$Q12UHb,
                                    `2` = "PiS",
                                    `5` = "KO",
                                    `3` = "Lewica",
                                    `1` = "PSL-Kukiz",
                                    `4` = "Konfederacja")
pgsw2019$votefor_s <- fct_expand(pgsw2019$votefor_s, "Did not vote")
pgsw2019$votefor_s[pgsw2019$votedsen=="No"] <- "Did not vote"
var_label(pgsw2019$votefor_s) <- "Party voted for in most recent parliamentary election"

pgsw2019$voted_pr <- recode_factor(read_2019$Q13a,
                                   `5` = "No",
                                   `1` = "Yes",)
pgsw2019$voted_pr <- fct_drop(pgsw2019$voted_pr)
var_label(pgsw2019$voted_pr) <- "Voted in previous election to the Sejm"

pgsw2019$votefor_pr <- recode_factor(read_2019$Q13b,
                                     `1` = "PiS",
                                     `2` = "PO",
                                     `3` = "Razem",
                                     `4` = "KORWiN",
                                     `5` = "PSL",
                                     `6` = "ZL",
                                     `7` = "Kukiz'15",
                                     `8` = "Nowoczesna")
pgsw2019$votefor_pr <- fct_drop(pgsw2019$votefor_pr)
pgsw2019$votefor_pr <- fct_expand(pgsw2019$votefor_pr, "Did not vote")
pgsw2019$votefor_pr[pgsw2019$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2019$votefor_pr) <- "Party voted for in previous election to the Sejm"

pgsw2019$votefor_t_pr <- recode_factor(read_2019$Q13b,
                                       `1` = "Right",
                                       `2` = "Liberal",
                                       `3` = "Left",
                                       `4` = "Right",
                                       `5` = "Other",
                                       `6` = "Left",
                                       `7` = "Right",
                                       `8` = "Liberal")
pgsw2019$votefor_t_pr <- fct_drop(pgsw2019$votefor_t_pr)
pgsw2019$votefor_t_pr <- fct_expand(pgsw2019$votefor_t_pr, "Did not vote")
pgsw2019$votefor_t_pr[pgsw2019$voted_pr=="No"] <- "Did not vote"
pgsw2019$votefor_t_pr <- fct_relevel(pgsw2019$votefor_t_pr, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2019$votefor_t_pr) <- "Type of party voted for in previous election to the Sejm"

pgsw2019 <- mutate(pgsw2019, stability = if_else(pgsw2019$votefor_t=="Right" & pgsw2019$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2019$votefor_t=="Left" & pgsw2019$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2019$votefor_t=="Liberal" & pgsw2019$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2019$votefor_t=="Other" & pgsw2019$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2019$votefor_t=="Did not vote" & pgsw2019$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2019$stability <- fct_relevel(pgsw2019$stability, "Unstable", "Stable")
var_label(pgsw2019$stability) <- "Stability of voting behaviour (type)"

pgsw2019 <- mutate(pgsw2019, partyid = factor(case_when(read_2019$Q10a==1 ~ "Yes",
                                                        read_2019$Q10a==2 ~ "No",
                                                        read_2019$Q10a==7 ~ "Don't know")))
pgsw2019$partyid <- fct_relevel(pgsw2019$partyid, "Yes", "No", "Don't know")
var_label(pgsw2019$partyid) <- "Party identification"

pgsw2019 <- mutate(pgsw2019, vtpres_2 = factor(if_else(read_2019$P18==3, "No", "Yes"))) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2019$P18==4, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2019$P18==5, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2019$P18==97, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read_2019$P18==98, NA))
var_label(pgsw2019$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw2019 <- mutate(pgsw2019, vtforpres_2 = factor(case_when(read_2019$P18==1 ~ "Komorowski",
                                                            read_2019$P18==2 ~ "Duda",
                                                            read_2019$P18==3 ~ "Did not vote")))
var_label(pgsw2019$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"


#####Likes/dislikes#####
pgsw1997$likeSLD <- dplyr::recode(read_1997$var30, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeSLD  <- add_labels(pgsw1997$likeSLD , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw1997$likeSLD) <- "Feeling toward SLD"  

pgsw1997$likePSL <- dplyr::recode(read_1997$var32, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likePSL  <- add_labels(pgsw1997$likePSL , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw1997$likePSL) <- "Feeling toward PSL"  

pgsw1997$likeUW <- dplyr::recode(read_1997$var34, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeUW  <- add_labels(pgsw1997$likeUW , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw1997$likeUW) <- "Feeling toward UW"  

pgsw1997$likeUP <- dplyr::recode(read_1997$var36, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeUP  <- add_labels(pgsw1997$likeUP , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw1997$likeUP) <- "Feeling toward UP"  

pgsw1997$likeAWS <- dplyr::recode(read_1997$var38, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeAWS  <- add_labels(pgsw1997$likeAWS , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw1997$likeAWS) <- "Feeling toward AWS"  

pgsw1997$likeROP <- dplyr::recode(read_1997$var40, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeROP  <- add_labels(pgsw1997$likeROP , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw1997$likeROP) <- "Feeling toward ROP"

pgsw2001$likeSLD <- dplyr::recode(read_2001$liksl052, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeSLD  <- add_labels(pgsw2001$likeSLD , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeSLD) <- "Feeling toward SLD"  

pgsw2001$likePSL <- dplyr::recode(read_2001$likps057, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likePSL  <- add_labels(pgsw2001$likePSL , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likePSL) <- "Feeling toward PSL"  

pgsw2001$likeUW <- dplyr::recode(read_2001$likuw054, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeUW  <- add_labels(pgsw2001$likeUW , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeUW) <- "Feeling toward UW"  

pgsw2001$likeSRP <- dplyr::recode(read_2001$liksa055, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeSRP  <- add_labels(pgsw2001$likeSRP , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeSRP) <- "Feeling toward SRP"  

pgsw2001$likeAWS <- dplyr::recode(read_2001$likaw053, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeAWS  <- add_labels(pgsw2001$likeAWS , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeAWS) <- "Feeling toward AWS"  

pgsw2001$likeLPR <- dplyr::recode(read_2001$liklp059, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeLPR  <- add_labels(pgsw2001$likeLPR , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeLPR) <- "Feeling toward LPR"

pgsw2001$likePO <- dplyr::recode(read_2001$likpo058, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likePO  <- add_labels(pgsw2001$likePO , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likePO) <- "Feeling toward PO"

pgsw2005$likePiS <- dplyr::recode(read_2005$p17d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePiS <- add_labels(pgsw2005$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePiS) <- "Feeling toward PiS"  

pgsw2005$likePO <- dplyr::recode(read_2005$p17f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePO <- add_labels(pgsw2005$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePO) <- "Feeling toward PO" 

pgsw2005$likePSL <- dplyr::recode(read_2005$p17g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePSL <- add_labels(pgsw2005$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePSL) <- "Feeling toward PSL" 

pgsw2005$likeSLD <- dplyr::recode(read_2005$p17e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSLD <- add_labels(pgsw2005$likeSLD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeSLD) <- "Feeling toward SLD" 

pgsw2005$likeSdPL <- dplyr::recode(read_2005$p17c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSdPL <- add_labels(pgsw2005$likeSdPL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeSdPL) <- "Feeling toward SdPL" 

pgsw2005$likePD<- dplyr::recode(read_2005$p17b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePD <- add_labels(pgsw2005$likePD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePD) <- "Feeling toward PD" 

pgsw2005$likeSRP<- dplyr::recode(read_2005$p17h, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSRP <- add_labels(pgsw2005$likeSRP, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeSRP) <- "Feeling toward SRP" 

pgsw2005$likeLPR<- dplyr::recode(read_2005$p17a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeLPR <- add_labels(pgsw2005$likeLPR, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeLPR) <- "Feeling toward LPR" 

pgsw2007$likePiS <- dplyr::recode(read_2007$c14a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePiS <- add_labels(pgsw2007$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likePiS) <- "Feeling toward PiS"  

pgsw2007$likePO <- dplyr::recode(read_2007$c14b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePO <- add_labels(pgsw2007$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likePO) <- "Feeling toward PO" 

pgsw2007$likePSL <- dplyr::recode(read_2007$c14c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePSL <- add_labels(pgsw2007$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likePSL) <- "Feeling toward PSL" 

pgsw2007$likeLiD <- dplyr::recode(read_2007$c14d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likeLiD <- add_labels(pgsw2007$likeLiD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likeLiD) <- "Feeling toward LiD" 

pgsw2011$likePiS <- dplyr::recode(read_2011$C12A, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePiS <- add_labels(pgsw2011$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likePiS) <- "Feeling toward PiS"  

pgsw2011$likePO <- dplyr::recode(read_2011$C12B, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePO <- add_labels(pgsw2011$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likePO) <- "Feeling toward PO" 

pgsw2011$likePSL <- dplyr::recode(read_2011$C12C, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePSL <- add_labels(pgsw2011$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likePSL) <- "Feeling toward PSL" 

pgsw2011$likeSLD <- dplyr::recode(read_2011$C12D, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likeSLD <- add_labels(pgsw2011$likeSLD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likeSLD) <- "Feeling toward SLD" 

pgsw2011$likeRP <- dplyr::recode(read_2011$C12E, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likeRP <- add_labels(pgsw2011$likeRP, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likeRP) <- "Feeling toward RP" 

pgsw2015$likePiS <- dplyr::recode(read_2015$q42, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePiS <- add_labels(pgsw2015$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likePiS) <- "Feeling toward PiS"  

pgsw2015$likePO <- dplyr::recode(read_2015$q45, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePO <- add_labels(pgsw2015$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likePO) <- "Feeling toward PO" 

pgsw2015$likePSL <- dplyr::recode(read_2015$q48, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePSL <- add_labels(pgsw2015$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likePSL) <- "Feeling toward PSL" 

pgsw2015$likeZL <- dplyr::recode(read_2015$q51, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeZL <- add_labels(pgsw2015$likeZL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeZL) <- "Feeling toward ZL" 

pgsw2015$likeKukiz <- dplyr::recode(read_2015$q54, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeKukiz <- add_labels(pgsw2015$likeKukiz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeKukiz) <- "Feeling toward Kukiz'15" 

pgsw2015$likeNowo<- dplyr::recode(read_2015$q57, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeNowo <- add_labels(pgsw2015$likeNowo, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeNowo) <- "Feeling toward Nowoczesna" 

pgsw2015$likeKorwin<- dplyr::recode(read_2015$q60, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeKorwin <- add_labels(pgsw2015$likeKorwin, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                  '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeKorwin) <- "Feeling toward KORWiN" 

pgsw2015$likeRazem<- dplyr::recode(read_2015$q63, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeRazem <- add_labels(pgsw2015$likeRazem, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeRazem) <- "Feeling toward Razem" 

pgsw2019$likePiS <- dplyr::recode(read_2019$Q15a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePiS <- add_labels(pgsw2019$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePiS) <- "Feeling toward PiS"  

pgsw2019$likePO <- dplyr::recode(read_2019$Q15b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePO <- add_labels(pgsw2019$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePO) <- "Feeling toward PO" 

pgsw2019$likePSL <- dplyr::recode(read_2019$Q15c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePSL <- add_labels(pgsw2019$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePSL) <- "Feeling toward PSL" 

pgsw2019$likeSLD <- dplyr::recode(read_2019$Q15d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeSLD <- add_labels(pgsw2019$likeSLD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeSLD) <- "Feeling toward SLD" 

pgsw2019$likeKukiz <- dplyr::recode(read_2019$Q15e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKukiz <- add_labels(pgsw2019$likeKukiz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKukiz) <- "Feeling toward Kukiz'15" 

pgsw2019$likeWiosna <- dplyr::recode(read_2019$Q15f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                     `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeWiosna <- add_labels(pgsw2019$likeWiosna, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                  '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeWiosna) <- "Feeling toward Wiosna" 

pgsw2019$likeKonf<- dplyr::recode(read_2019$Q15g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKonf <- add_labels(pgsw2019$likeKonf, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKonf) <- "Feeling toward Konfederacja" 

pgsw2019$likeKacz <- dplyr::recode(read_2019$Q16a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKacz <- add_labels(pgsw2019$likeKacz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKacz) <- "Feeling toward Jarosław Kaczyński"  

pgsw2019$likeSche <- dplyr::recode(read_2019$Q16b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeSche <- add_labels(pgsw2019$likeSche, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeSche) <- "Feeling toward Grzegorz Schetyna" 

pgsw2019$likeKosz <- dplyr::recode(read_2019$Q16c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKosz <- add_labels(pgsw2019$likeKosz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKosz) <- "Feeling toward Władysław Kosiniak-Kamysz" 

pgsw2019$likeCzar <- dplyr::recode(read_2019$Q16d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeCzar <- add_labels(pgsw2019$likeCzar, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeCzar) <- "Feeling toward Włodzimierz Czarzasty" 

pgsw2019$likePKukiz <- dplyr::recode(read_2019$Q16e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                     `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePKukiz <- add_labels(pgsw2019$likePKukiz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                  '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePKukiz) <- "Feeling toward Paweł Kukiz" 

pgsw2019$likeBied <- dplyr::recode(read_2019$Q16f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeBied <- add_labels(pgsw2019$likeBied, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeBied) <- "Feeling toward Robert Biedroń" 

pgsw2019$likeTusk <- dplyr::recode(read_2019$Q16g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeTusk <- add_labels(pgsw2019$likeTusk, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeTusk) <- "Feeling toward Donald Tusk" 

pgsw2019$likeMora <- dplyr::recode(read_2019$Q16h, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeMora <- add_labels(pgsw2019$likeMora, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeMora) <- "Feeling toward Mateusz Morawiecki" 

pgsw2019$likeKida <- dplyr::recode(read_2019$Q16i, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKida <- add_labels(pgsw2019$likeKida, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKida) <- "Feeling toward Małgorzata Kidawa-Błońska" 

pgsw2019$likeKorw <- dplyr::recode(read_2019$Q16j, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKorw <- add_labels(pgsw2019$likeKorw, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKorw) <- "Feeling toward Janusz Korwin-Mikke" 


#####Party placement on ideological issues#####
pgsw2015$lrPiS <- dplyr::recode(read_2015$q72, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPiS <- add_labels(pgsw2015$lrPiS, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrPiS) <- "Left-right placement of PiS"  

pgsw2015$lrPO <- dplyr::recode(read_2015$q75, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPO <- add_labels(pgsw2015$lrPO, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                      '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrPO) <- "Left-right placement of PO"  

pgsw2015$lrRazem <- dplyr::recode(read_2015$q78, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrRazem <- add_labels(pgsw2015$lrRazem, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrRazem) <- "Left-right placement of Razem"  

pgsw2015$lrZL <- dplyr::recode(read_2015$q87, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrZL <- add_labels(pgsw2015$lrZL, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                      '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrZL) <- "Left-right placement of United Left" 

pgsw2015$lrKukiz <- dplyr::recode(read_2015$q90, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrKukiz <- add_labels(pgsw2015$lrKukiz, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrKukiz) <- "Left-right placement of Kukiz'15" 

pgsw2015$lrNowo <- dplyr::recode(read_2015$q93, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrNowo <- add_labels(pgsw2015$lrNowo, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrNowo) <- "Left-right placement of Nowoczesna" 

pgsw2015$lrKorwin <- dplyr::recode(read_2015$q81, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrKorwin <- add_labels(pgsw2015$lrKorwin, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrKorwin) <- "Left-right placement of KORWiN" 

pgsw2015$lrPSL <- dplyr::recode(read_2015$q84, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPSL <- add_labels(pgsw2015$lrPSL, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrPSL) <- "Left-right placement of PSL" 


pgsw2019$lrPiS <- dplyr::recode(read_2019$Q17a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrPiS <- add_labels(pgsw2019$lrPiS, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrPiS) <- "Left-right placement of PiS"  

pgsw2019$lrPO <- dplyr::recode(read_2019$Q17b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrPO <- add_labels(pgsw2019$lrPO, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                      '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrPO) <- "Left-right placement of PO"  

pgsw2019$lrPSL <- dplyr::recode(read_2019$Q17c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrPSL <- add_labels(pgsw2019$lrPSL, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrPSL) <- "Left-right placement of PSL" 

pgsw2019$lrSLD <- dplyr::recode(read_2019$Q17d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrSLD <- add_labels(pgsw2019$lrSLD, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrSLD) <- "Left-right placement of SLD"  

pgsw2019$lrKukiz <- dplyr::recode(read_2019$Q17e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrKukiz <- add_labels(pgsw2019$lrKukiz, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrKukiz) <- "Left-right placement of Kukiz'15" 

pgsw2019$lrWiosna <- dplyr::recode(read_2019$Q17f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrWiosna <- add_labels(pgsw2019$lrWiosna, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrWiosna) <- "Left-right placement of Nowoczesna" 

pgsw2019$lrKonf <- dplyr::recode(read_2019$Q17g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrKonf <- add_labels(pgsw2019$lrKonf, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrKonf) <- "Left-right placement of KORWiN" 

pgsw2019$climate_PiS <- dplyr::recode(read_2019$I2_1, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$climate_PiS <- add_labels(pgsw2019$climate_PiS, labels = c("Climate change and pollution are not important problems; there are many other much more important ones" = 1, 
                                                                    '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                    "Climate change and pollution are the most important problems in contemporary Poland" = 7))
var_label(pgsw2019$climate_PiS) <- "Climate change (PiS)"  

pgsw2019$forpol_PiS <- dplyr::recode(read_2019$I2_2, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$forpol_PiS <- add_labels(pgsw2019$forpol_PiS, labels = c("Our foreign policy should be based above all on defence of our political and economic independence, even at the cost of leaving the European Union" = 1,
                                                                  '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                  "Our foreign policy should be based on the closest possible cooperation with the European Union, and observance of its binding rules" = 7))
var_label(pgsw2019$forpol_PiS) <- "Foreign policy (PiS)"  

pgsw2019$lgbt_PiS <- dplyr::recode(read_2019$I2_3, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$lgbt_PiS <- add_labels(pgsw2019$lgbt_PiS, labels = c("Same-sex couples should not have the same rights as heterosexual couples to publicly manifest their lifestyle" = 1,
                                                              '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                              "Same-sex couples should have the same rights as heterosexual couples to publicly manifest their lifestyle" = 7))
var_label(pgsw2019$lgbt_PiS) <- "LGBT rights (PiS)"  

pgsw2019$climate_PO <- dplyr::recode(read_2019$I3_1, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$climate_PO <- add_labels(pgsw2019$climate_PO, labels = c("Climate change and pollution are not important problems; there are many other much more important ones" = 1, 
                                                                  '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                  "Climate change and pollution are the most important problems in contemporary Poland" = 7))
var_label(pgsw2019$climate_PO) <- "Climate change (PO)"  

pgsw2019$forpol_PO <- dplyr::recode(read_2019$I3_2, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$forpol_PO <- add_labels(pgsw2019$forpol_PO, labels = c("Our foreign policy should be based above all on defence of our political and economic independence, even at the cost of leaving the European Union" = 1,
                                                                '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                "Our foreign policy should be based on the closest possible cooperation with the European Union, and observance of its binding rules" = 7))
var_label(pgsw2019$forpol_PO) <- "Foreign policy (PO)"  

pgsw2019$lgbt_PO <- dplyr::recode(read_2019$I3_3, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$lgbt_PO <- add_labels(pgsw2019$lgbt_PO, labels = c("Same-sex couples should not have the same rights as heterosexual couples to publicly manifest their lifestyle" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                            "Same-sex couples should have the same rights as heterosexual couples to publicly manifest their lifestyle" = 7))
var_label(pgsw2019$lgbt_PO) <- "LGBT rights (PO)"  

pgsw2019$climate_PSL <- dplyr::recode(read_2019$I4_1, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$climate_PSL <- add_labels(pgsw2019$climate_PSL, labels = c("Climate change and pollution are not important problems; there are many other much more important ones" = 1, 
                                                                    '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                    "Climate change and pollution are the most important problems in contemporary Poland" = 7))
var_label(pgsw2019$climate_PSL) <- "Climate change (PSL)"  

pgsw2019$forpol_PSL <- dplyr::recode(read_2019$I4_2, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$forpol_PSL <- add_labels(pgsw2019$forpol_PSL, labels = c("Our foreign policy should be based above all on defence of our political and economic independence, even at the cost of leaving the European Union" = 1,
                                                                  '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                  "Our foreign policy should be based on the closest possible cooperation with the European Union, and observance of its binding rules" = 7))
var_label(pgsw2019$forpol_PSL) <- "Foreign policy (PSL)"  

pgsw2019$lgbt_PSL <- dplyr::recode(read_2019$I4_3, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$lgbt_PSL <- add_labels(pgsw2019$lgbt_PSL, labels = c("Same-sex couples should not have the same rights as heterosexual couples to publicly manifest their lifestyle" = 1,
                                                              '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                              "Same-sex couples should have the same rights as heterosexual couples to publicly manifest their lifestyle" = 7))
var_label(pgsw2019$lgbt_PSL) <- "LGBT rights (PSL)"  

pgsw2019$climate_Lewica <- dplyr::recode(read_2019$I5_1, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$climate_Lewica <- add_labels(pgsw2019$climate_Lewica, labels = c("Climate change and pollution are not important problems; there are many other much more important ones" = 1, 
                                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                          "Climate change and pollution are the most important problems in contemporary Poland" = 7))
var_label(pgsw2019$climate_Lewica) <- "Climate change (Lewica)"  

pgsw2019$forpol_Lewica <- dplyr::recode(read_2019$I5_2, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$forpol_Lewica <- add_labels(pgsw2019$forpol_Lewica, labels = c("Our foreign policy should be based above all on defence of our political and economic independence, even at the cost of leaving the European Union" = 1,
                                                                        '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                        "Our foreign policy should be based on the closest possible cooperation with the European Union, and observance of its binding rules" = 7))
var_label(pgsw2019$forpol_Lewica) <- "Foreign policy (Lewica)"  

pgsw2019$lgbt_Lewica <- dplyr::recode(read_2019$I5_3, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$lgbt_Lewica <- add_labels(pgsw2019$lgbt_Lewica, labels = c("Same-sex couples should not have the same rights as heterosexual couples to publicly manifest their lifestyle" = 1,
                                                                    '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                                    "Same-sex couples should have the same rights as heterosexual couples to publicly manifest their lifestyle" = 7))
var_label(pgsw2019$lgbt_Lewica) <- "LGBT rights (Lewica)"  


#####Self-placement on ideological issues#####
pgsw1997$leftrt <- dplyr::recode(read_1997$var56, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)

pgsw1997$leftrt <- add_labels(pgsw1997$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw1997$leftrt) <- "Left-right self-placement"

pgsw1997$euinteg <- dplyr::recode(read_1997$p_eu0115, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$euinteg <- add_labels(pgsw1997$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw1997$euinteg) <- "European integration"

pgsw1997$chstdiv <- dplyr::recode(read_1997$p_rel111, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$chstdiv <- add_labels(pgsw1997$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "The Church should be completely separate from the state" = 10))
var_label(pgsw1997$chstdiv) <- "Church/state divide" 

pgsw1997$taxreg <- dplyr::recode(read_1997$p_tax114, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$taxreg <- add_labels(pgsw1997$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "Flat tax regime" = 10))
var_label(pgsw1997$taxreg) <- "Tax regime" 

pgsw1997$socpol <- dplyr::recode(read_1997$p_soc117, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$socpol <- add_labels(pgsw1997$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "People should take care of their own welfare" = 10))
var_label(pgsw1997$socpol) <- "Social policy" 

pgsw1997$unemp <- dplyr::recode(read_1997$p_une113, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$unemp <- add_labels(pgsw1997$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw1997$unemp) <- "Unemployment" 

pgsw1997$private <- dplyr::recode(read_1997$p_prv110, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$private <- add_labels(pgsw1997$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw1997$private) <- "Privatisation" 

pgsw1997$abort <- dplyr::recode(read_1997$p_abo119, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$abort <- add_labels(pgsw1997$abort, labels = c("There should be no right to abortion" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 10))
var_label(pgsw1997$abort) <- "Abortion" 

pgsw1997$crime <- dplyr::recode(read_1997$p_crm109, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$crime <- add_labels(pgsw1997$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Fight crime, but with attention to citizens' rights" = 10))
var_label(pgsw1997$crime) <- "Crime" 

HS.model <- ' economic  =~ private + socpol + taxreg + unemp
              cultural =~ abort + euinteg '

fit <- cfa(HS.model, data=pgsw1997)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw1997[idx, fs] <- fscores[ , fs]
}

pgsw1997$economic <- scales::rescale(pgsw1997$economic, c(0,1))
var_label(pgsw1997$economic) <- "Index of economic attitudes"

pgsw1997$cultural <- scales::rescale(pgsw1997$cultural, c(0,1))
var_label(pgsw1997$cultural) <- "Index of cultural attitudes" 

pgsw2001$leftrt <- dplyr::recode(read_2001$lrslf073, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$leftrt <- add_labels(pgsw2001$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2001$leftrt) <- "Left-right self-placement"

pgsw2001$euinteg <- dplyr::recode(read_2001$p_eu0101, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$euinteg <- add_labels(pgsw2001$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2001$euinteg) <- "European integration"

pgsw2001$chstdiv <- dplyr::recode(read_2001$p_rel097, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$chstdiv <- add_labels(pgsw2001$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "The Church should be completely separate from the state" = 10))
var_label(pgsw2001$chstdiv) <- "Church/state divide" 

pgsw2001$taxreg <- dplyr::recode(read_2001$p_tax100, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$taxreg <- add_labels(pgsw2001$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Flat tax regime" = 10))
var_label(pgsw2001$taxreg) <- "Tax regime" 

pgsw2001$socpol <- dplyr::recode(read_2001$p_soc103, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$socpol <- add_labels(pgsw2001$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "People should take care of their own welfare" = 10))
var_label(pgsw2001$socpol) <- "Social policy" 

pgsw2001$unemp <- dplyr::recode(read_2001$p_une099, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$unemp <- add_labels(pgsw2001$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw2001$unemp) <- "Unemployment" 

pgsw2001$private <- dplyr::recode(read_2001$p_prv096, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$private <- add_labels(pgsw2001$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw2001$private) <- "Privatisation" 

pgsw2001$crime <- dplyr::recode(read_2001$p_crm095, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$crime <- add_labels(pgsw2001$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Fight crime, but with attention to citizens' rights" = 10))
var_label(pgsw2001$crime) <- "Crime" 

HS.model <- ' economic  =~ private + taxreg + socpol
              cultural =~ crime + euinteg'

fit <- cfa(HS.model, data=pgsw2001)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2001[idx, fs] <- fscores[ , fs]
}

pgsw2001$economic <- scales::rescale(pgsw2001$economic, c(0,1))
var_label(pgsw2001$economic) <- "Index of economic attitudes"

pgsw2001$cultural <- scales::rescale(pgsw2001$cultural, c(0,1))
var_label(pgsw2001$cultural) <- "Index of cultural attitudes" 

pgsw2005$leftrt <- dplyr::recode(read_2005$p20, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$leftrt <- add_labels(pgsw2005$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2005$leftrt) <- "Left-right self-placement"

pgsw2005$euinteg <- dplyr::recode(read_2005$p65g, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$euinteg <- add_labels(pgsw2005$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2005$euinteg) <- "European integration"

pgsw2005$chstdiv <- dplyr::recode(read_2005$p65c, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$chstdiv <- add_labels(pgsw2005$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "The Church should be completely separate from the state" = 10))
var_label(pgsw2005$chstdiv) <- "Church/state divide" 

pgsw2005$taxreg <- dplyr::recode(read_2005$p65f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$taxreg <- add_labels(pgsw2005$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "Flat tax regime" = 10))
var_label(pgsw2005$taxreg) <- "Tax regime" 

pgsw2005$socpol <- dplyr::recode(read_2005$p65i, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$socpol <- add_labels(pgsw2005$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "People should take care of their own welfare" = 10))
var_label(pgsw2005$socpol) <- "Social policy" 

pgsw2005$unemp <- dplyr::recode(read_2005$p65e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$unemp <- add_labels(pgsw2005$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw2005$unemp) <- "Unemployment" 

pgsw2005$private <- dplyr::recode(read_2005$p65b, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$private <- add_labels(pgsw2005$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw2005$private) <- "Privatisation" 

pgsw2005$abort <- dplyr::recode(read_2005$p65l, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$abort <- add_labels(pgsw2005$abort, labels = c("There should be no right to abortion" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 10))
var_label(pgsw2005$abort) <- "Abortion" 

pgsw2005$crime <- dplyr::recode(read_2005$p65a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$crime <- add_labels(pgsw2005$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Fight crime, but with attention to citizens' rights" = 10))
var_label(pgsw2005$crime) <- "Crime" 

pgsw2005$immigr <- dplyr::recode(read_2005$p65m, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                 `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$immigr <- add_labels(pgsw2005$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "The state should encourage people from other countries to immigrate to Poland" = 10))
var_label(pgsw2005$immigr) <- "Immigration" 

HS.model <- ' economic  =~ private + socpol + taxreg + unemp
              cultural =~ abort + immigr + euinteg '

fit <- cfa(HS.model, data=pgsw2005)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2005[idx, fs] <- fscores[ , fs]
}

pgsw2005$economic <- scales::rescale(pgsw2005$economic, c(0,1))
var_label(pgsw2005$economic) <- "Index of economic attitudes"

pgsw2005$cultural <- scales::rescale(pgsw2005$cultural, c(0,1))
var_label(pgsw2005$cultural) <- "Index of cultural attitudes" 

pgsw2007$leftrt <- dplyr::recode(read_2007$c17, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$leftrt <- add_labels(pgsw2007$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2007$leftrt) <- "Left-right self-placement"

pgsw2007$euinteg <- dplyr::recode(read_2007$p49f, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$euinteg <- add_labels(pgsw2007$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2007$euinteg) <- "European integration"

pgsw2007$taxreg <- dplyr::recode(read_2007$p49e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$taxreg <- add_labels(pgsw2007$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "Flat tax regime" = 10))
var_label(pgsw2007$taxreg) <- "Tax regime" 

pgsw2007$unemp <- dplyr::recode(read_2007$p49d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$unemp <- add_labels(pgsw2007$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw2007$unemp) <- "Unemployment" 

pgsw2007$private <- dplyr::recode(read_2007$p49b, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$private <- add_labels(pgsw2007$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw2007$private) <- "Privatisation" 

pgsw2007$abort <- dplyr::recode(read_2007$p49i, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$abort <- add_labels(pgsw2007$abort, labels = c("There should be no right to abortion" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 10))
var_label(pgsw2007$abort) <- "Abortion" 

pgsw2007$crime <- dplyr::recode(read_2007$p49a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$crime <- add_labels(pgsw2007$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Fight crime, but with attention to citizens' rights" = 10))
var_label(pgsw2007$crime) <- "Crime"

HS.model <- ' economic  =~ taxreg + private
              cultural =~ euinteg + abort  '

fit <- cfa(HS.model, data=pgsw2007)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2007[idx, fs] <- fscores[ , fs]
}

pgsw2007$economic <- scales::rescale(pgsw2007$economic, c(0,1))
var_label(pgsw2007$economic) <- "Index of economic attitudes"

pgsw2007$cultural <- scales::rescale(pgsw2007$cultural, c(0,1))
var_label(pgsw2007$cultural) <- "Index of cultural attitudes"

pgsw2011$leftrt <- dplyr::recode(read_2011$C15, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$leftrt <- add_labels(pgsw2011$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2011$leftrt) <- "Left-right self-placement"  

pgsw2011$euinteg <- dplyr::recode(read_2011$PI36E, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L)
pgsw2011$euinteg <- add_labels(pgsw2011$euinteg, labels = c("Anti-integration" = 1, '2' = 2, '3' = 3, '4' = 4, "Pro-integration" = 5))
var_label(pgsw2011$euinteg) <- "European integration"   

pgsw2011$chstdiv <- dplyr::recode(read_2011$PI36B, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$chstdiv <- add_labels(pgsw2011$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "The Church should be completely separate from the state" = 7))
var_label(pgsw2011$chstdiv) <- "Church/state divide"                                                           

pgsw2011$taxreg <- dplyr::recode(read_2011$PI36D, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$taxreg <- add_labels(pgsw2011$taxreg, labels = c("Progressive tax regime" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "Flat tax regime" = 7))
var_label(pgsw2011$taxreg) <- "Tax regime"                                                           

pgsw2011$unemp <- dplyr::recode(read_2011$PI36C, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$unemp <- add_labels(pgsw2011$unemp, labels = c("Employment should be an absolute policy priority" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, "Employment is less important than other policy domains" = 7))
var_label(pgsw2011$unemp) <- "Unemployment"  

pgsw2011$socpol <- dplyr::recode(read_2011$PI36G, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$socpol <- add_labels(pgsw2011$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "People should take care of their own welfare" = 7))
var_label(pgsw2011$socpol) <- "Social policy"  

pgsw2011$private <- dplyr::recode(read_2011$PI36A, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$private <- add_labels(pgsw2011$private, labels = c("A significant number of enterprises should remain in state hands" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "All state-owned enterprises should be privatised" = 7))
var_label(pgsw2011$private) <- "Privatisation"  

pgsw2011$abort <- dplyr::recode(read_2011$PI36H, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$abort <- add_labels(pgsw2011$abort, labels = c("There should be no right to abortion" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, "A woman should have a right to an abortion whatever the circumstances" = 7))
var_label(pgsw2011$abort) <- "Abortion"  

HS.model <- ' economic  =~ private + socpol + taxreg + unemp
              cultural =~ euinteg + abort '

fit <- cfa(HS.model, data=pgsw2011)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2011[idx, fs] <- fscores[ , fs]
}

pgsw2011$economic <- scales::rescale(pgsw2011$economic, c(0,1))
var_label(pgsw2011$economic) <- "Index of economic attitudes"

pgsw2011$cultural <- scales::rescale(pgsw2011$cultural, c(0,1))
var_label(pgsw2011$cultural) <- "Index of cultural attitudes" 

pgsw2015$leftrt <- dplyr::recode(read_2015$q96, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$leftrt <- add_labels(pgsw2015$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$leftrt) <- "Left-right self-placement"  

pgsw2015$sollib <- dplyr::recode(read_2015$q102, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$sollib <- add_labels(pgsw2015$sollib, labels = c("Solidaristic" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Liberal" = 10))
var_label(pgsw2015$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2015$euinteg <- dplyr::recode(read_2015$q99, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$euinteg <- add_labels(pgsw2015$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2015$euinteg) <- "European integration"   

pgsw2015$chstdiv <- dplyr::recode(read_2015$L1q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$chstdiv <- add_labels(pgsw2015$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "The Church should be completely separate from the state" = 7))
var_label(pgsw2015$chstdiv) <- "Church/state divide"                                                           

pgsw2015$taxreg <- dplyr::recode(read_2015$L2q144, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2015$taxreg <- add_labels(pgsw2015$taxreg, labels = c("Progressive tax regime" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "Flat tax regime" = 7))
var_label(pgsw2015$taxreg) <- "Tax regime"                                                           

pgsw2015$forpol <- dplyr::recode(read_2015$L3q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$forpol <- add_labels(pgsw2015$forpol, labels = c("Foreign policy should be based on political and economic independence" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "Foreign policy should be based on close cooperation with the EU" = 7))
var_label(pgsw2015$forpol) <- "Foreign policy"  

pgsw2015$immigr <- dplyr::recode(read_2015$L4q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$immigr <- add_labels(pgsw2015$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "The state should encourage people from other countries to immigrate to Poland" = 7))
var_label(pgsw2015$immigr) <- "Immigration"  

pgsw2015$socpol <- dplyr::recode(read_2015$L5q144, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2015$socpol <- add_labels(pgsw2015$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "People should take care of their own welfare" = 7))
var_label(pgsw2015$socpol) <- "Social policy"  

pgsw2015$private <- dplyr::recode(read_2015$L6q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$private <- add_labels(pgsw2015$private, labels = c("A significant number of enterprises should remain in state hands" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "All state-owned enterprises should be privatised" = 7))
var_label(pgsw2015$private) <- "Privatisation"  

pgsw2015$abort <- dplyr::recode(read_2015$L7q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$abort <- add_labels(pgsw2015$abort, labels = c("There should be no right to abortion" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, "A woman should have a right to an abortion whatever the circumstances" = 7))
var_label(pgsw2015$abort) <- "Abortion"  

HS.model <- ' economic  =~ private + socpol + taxreg
              cultural =~ euinteg + abort  '

fit <- cfa(HS.model, data=pgsw2015)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2015[idx, fs] <- fscores[ , fs]
}

pgsw2015$economic <- scales::rescale(pgsw2015$economic, c(0,1))
var_label(pgsw2015$economic) <- "Index of economic attitudes"

pgsw2015$cultural <- scales::rescale(pgsw2015$cultural, c(0,1))
var_label(pgsw2015$cultural) <- "Index of cultural attitudes" 

pgsw2019$leftrt <- dplyr::recode(read_2019$Q18, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$leftrt <- add_labels(pgsw2019$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$leftrt) <- "Left-right self-placement"

pgsw2019$euinteg <- dplyr::recode(read_2019$P10, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$euinteg <- add_labels(pgsw2019$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2019$euinteg) <- "European integration"  

pgsw2019$sollib <- dplyr::recode(read_2019$P11, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$sollib <- add_labels(pgsw2019$sollib, labels = c("Solidaristic" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Liberal" = 10))
var_label(pgsw2019$sollib) <- "Solidarism-liberalism self-placement"  

pgsw2019$climate <- dplyr::recode(read_2019$I1a, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$climate <- add_labels(pgsw2019$climate, labels = c("Climate change and environmental degradation is not an important problem" = 1, 
                                                            '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                            "Climate change and environmental degradation is the most important problem facing Poland" = 7))
var_label(pgsw2019$climate) <- "Climate change"  

pgsw2019$taxreg <- dplyr::recode(read_2019$I1b, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2019$taxreg <- add_labels(pgsw2019$taxreg, labels = c("Progressive tax regime" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "Flat tax regime" = 7))
var_label(pgsw2019$taxreg) <- "Tax regime"     

pgsw2019$forpol <- dplyr::recode(read_2019$I1c, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$forpol <- add_labels(pgsw2019$forpol, labels = c("Foreign policy should be based on political and economic independence" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "Foreign policy should be based on close cooperation with the EU" = 7))
var_label(pgsw2019$forpol) <- "Foreign policy"  

pgsw2019$immigr <- dplyr::recode(read_2019$I1d, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$immigr <- add_labels(pgsw2019$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "The state should encourage people from other countries to immigrate to Poland" = 7))
var_label(pgsw2019$immigr) <- "Immigration"  

pgsw2019$socpol <- dplyr::recode(read_2019$I1e, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2019$socpol <- add_labels(pgsw2019$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                          "People should take care of their own welfare" = 7))
var_label(pgsw2019$socpol) <- "Social policy"  

pgsw2019$lgbt <- dplyr::recode(read_2019$I1f, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$lgbt <- add_labels(pgsw2019$lgbt, labels = c("Same-sex couples should not have the same rights as heterosexuals to publicly display their lifestyle" = 1,
                                                      '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                      "Same-sex couples should have the same rights as heterosexuals to publicly display their lifestyle" = 7))
var_label(pgsw2019$lgbt) <- "LGBT rights"  

pgsw2019$abort <- dplyr::recode(read_2019$I1g, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$abort <- add_labels(pgsw2019$abort, labels = c("There should be no right to abortion" = 1,
                                                        '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 7))
var_label(pgsw2019$abort) <- "Abortion"  

#####Rescale inconsistent variables#####
pgsw1997$leftrt <- rescale(pgsw1997$leftrt, c(0,1))
pgsw1997$leftrt <- set_labels(pgsw1997$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw1997$leftrt) <- "Left-right self-placement"

pgsw1997$euinteg <- rescale(pgsw1997$euinteg, c(0,1))
pgsw1997$euinteg <- set_labels(pgsw1997$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw1997$euinteg) <- "European integration"

pgsw1997$chstdiv <- rescale(pgsw1997$chstdiv, c(0,1))
pgsw1997$chstdiv <- set_labels(pgsw1997$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw1997$chstdiv) <- "Church/state divide" 

pgsw1997$taxreg <- rescale(pgsw1997$taxreg, c(0,1))
pgsw1997$taxreg <- set_labels(pgsw1997$taxreg, labels = c("Progressive tax regime" = 0, "Flat tax regime" = 1))
var_label(pgsw1997$taxreg) <- "Tax regime" 

pgsw1997$socpol <- rescale(pgsw1997$socpol, c(0,1))
pgsw1997$socpol <- set_labels(pgsw1997$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw1997$socpol) <- "Social policy" 

pgsw1997$unemp <- rescale(pgsw1997$unemp, c(0,1))
pgsw1997$unemp <- set_labels(pgsw1997$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw1997$unemp) <- "Unemployment" 

pgsw1997$private <- rescale(pgsw1997$private, c(0,1))
pgsw1997$private <- set_labels(pgsw1997$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw1997$private) <- "Privatisation" 

pgsw1997$abort <- rescale(pgsw1997$abort, c(0,1))
pgsw1997$abort <- set_labels(pgsw1997$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw1997$abort) <- "Abortion" 

pgsw1997$crime <- rescale(pgsw1997$crime, c(0,1))
pgsw1997$crime <- set_labels(pgsw1997$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw1997$crime) <- "Crime" 

pgsw2001$leftrt <- rescale(pgsw2001$leftrt, c(0,1))
pgsw2001$leftrt <- set_labels(pgsw2001$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2001$leftrt) <- "Left-right self-placement"

pgsw2001$euinteg <- rescale(pgsw2001$euinteg, c(0,1))
pgsw2001$euinteg <- set_labels(pgsw2001$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2001$euinteg) <- "European integration"

pgsw2001$chstdiv <- rescale(pgsw2001$chstdiv, c(0,1))
pgsw2001$chstdiv <- set_labels(pgsw2001$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2001$chstdiv) <- "Church/state divide" 

pgsw2001$taxreg <- rescale(pgsw2001$taxreg, c(0,1))
pgsw2001$taxreg <- set_labels(pgsw2001$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2001$taxreg) <- "Tax regime" 

pgsw2001$socpol <- rescale(pgsw2001$socpol, c(0,1))
pgsw2001$socpol <- set_labels(pgsw2001$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2001$socpol) <- "Social policy" 

pgsw2001$unemp <- rescale(pgsw2001$unemp, c(0,1))
pgsw2001$unemp <- set_labels(pgsw2001$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2001$unemp) <- "Unemployment" 

pgsw2001$private <- rescale(pgsw2001$private, c(0,1))
pgsw2001$private <- set_labels(pgsw2001$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2001$private) <- "Privatisation" 

pgsw2001$crime <- rescale(pgsw2001$crime, c(0,1))
pgsw2001$crime <- set_labels(pgsw2001$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2001$crime) <- "Crime" 

pgsw2005$leftrt <- rescale(pgsw2005$leftrt, c(0,1))
pgsw2005$leftrt <- set_labels(pgsw2005$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2005$leftrt) <- "Left-right self-placement"

pgsw2005$euinteg <- rescale(pgsw2005$euinteg, c(0,1))
pgsw2005$euinteg <- set_labels(pgsw2005$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2005$euinteg) <- "European integration"

pgsw2005$chstdiv <- rescale(pgsw2005$chstdiv, c(0,1))
pgsw2005$chstdiv <- set_labels(pgsw2005$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2005$chstdiv) <- "Church/state divide" 

pgsw2005$taxreg <- rescale(pgsw2005$taxreg, c(0,1))
pgsw2005$taxreg <- set_labels(pgsw2005$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2005$taxreg) <- "Tax regime" 

pgsw2005$socpol <- rescale(pgsw2005$socpol, c(0,1))
pgsw2005$socpol <- set_labels(pgsw2005$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2005$socpol) <- "Social policy" 

pgsw2005$unemp <- rescale(pgsw2005$unemp, c(0,1))
pgsw2005$unemp <- set_labels(pgsw2005$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2005$unemp) <- "Unemployment" 

pgsw2005$private <- rescale(pgsw2005$private, c(0,1))
pgsw2005$private <- set_labels(pgsw2005$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2005$private) <- "Privatisation" 

pgsw2005$abort <- rescale(pgsw2005$abort, c(0,1))
pgsw2005$abort <- set_labels(pgsw2005$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2005$abort) <- "Abortion" 

pgsw2005$crime <- rescale(pgsw2005$crime, c(0,1))
pgsw2005$crime <- set_labels(pgsw2005$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2005$crime) <- "Crime" 

pgsw2005$immigr <- rescale(pgsw2005$immigr, c(0,1))
pgsw2005$immigr <- set_labels(pgsw2005$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0,
                                                          "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2005$immigr) <- "Immigration" 

pgsw2007$leftrt <- rescale(pgsw2007$leftrt, c(0,1))
pgsw2007$leftrt <- set_labels(pgsw2007$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2007$leftrt) <- "Left-right self-placement"

pgsw2007$euinteg <- rescale(pgsw2007$euinteg, c(0,1))
pgsw2007$euinteg <- set_labels(pgsw2007$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2007$euinteg) <- "European integration"

pgsw2007$taxreg <- rescale(pgsw2007$taxreg, c(0,1))
pgsw2007$taxreg <- set_labels(pgsw2007$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2007$taxreg) <- "Tax regime" 

pgsw2007$unemp <- rescale(pgsw2007$unemp, c(0,1))
pgsw2007$unemp <- set_labels(pgsw2007$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2007$unemp) <- "Unemployment" 

pgsw2007$private <- rescale(pgsw2007$private, c(0,1))
pgsw2007$private <- set_labels(pgsw2007$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2007$private) <- "Privatisation" 

pgsw2007$abort <- rescale(pgsw2007$abort, c(0,1))
pgsw2007$abort <- set_labels(pgsw2007$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2007$abort) <- "Abortion" 

pgsw2007$crime <- rescale(pgsw2007$crime, c(0,1))
pgsw2007$crime <- set_labels(pgsw2007$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2007$crime) <- "Crime"

pgsw2011$leftrt <- rescale(pgsw2011$leftrt, c(0,1))
pgsw2011$leftrt <- set_labels(pgsw2011$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2011$leftrt) <- "Left-right self-placement"  

pgsw2011$euinteg <- rescale(pgsw2011$euinteg, c(0,1))
pgsw2011$euinteg <- set_labels(pgsw2011$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2011$euinteg) <- "European integration"   

pgsw2011$chstdiv <- rescale(pgsw2011$chstdiv, c(0,1))
pgsw2011$chstdiv <- set_labels(pgsw2011$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2011$chstdiv) <- "Church/state divide"                                                           

pgsw2011$taxreg <- rescale(pgsw2011$taxreg, c(0,1))
pgsw2011$taxreg <- set_labels(pgsw2011$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2011$taxreg) <- "Tax regime"                                                           

pgsw2011$unemp <- rescale(pgsw2011$unemp, c(0,1))
pgsw2011$unemp <- set_labels(pgsw2011$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2011$unemp) <- "Unemployment"  

pgsw2011$socpol <- rescale(pgsw2011$socpol, c(0,1))
pgsw2011$socpol <- set_labels(pgsw2011$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2011$socpol) <- "Social policy"  

pgsw2011$private <- rescale(pgsw2011$private, c(0,1))
pgsw2011$private <- set_labels(pgsw2011$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2011$private) <- "Privatisation"  

pgsw2011$abort <- rescale(pgsw2011$abort, c(0,1))
pgsw2011$abort <- set_labels(pgsw2011$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2011$abort) <- "Abortion"  

pgsw2015$leftrt <- rescale(pgsw2015$leftrt, c(0,1))
pgsw2015$leftrt <- set_labels(pgsw2015$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2015$leftrt) <- "Left-right self-placement"  

pgsw2015$sollib <- rescale(pgsw2015$sollib, c(0,1))
pgsw2015$sollib <- set_labels(pgsw2015$sollib, labels = c("Solidaristic" = 0, "Liberal" = 1))
var_label(pgsw2015$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2015$euinteg <- rescale(pgsw2015$euinteg, c(0,1))
pgsw2015$euinteg <- set_labels(pgsw2015$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2015$euinteg) <- "European integration"   

pgsw2015$chstdiv <- rescale(pgsw2015$chstdiv, c(0,1))
pgsw2015$chstdiv <- set_labels(pgsw2015$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2015$chstdiv) <- "Church/state divide"                                                           

pgsw2015$taxreg <- rescale(pgsw2015$taxreg, c(0,1))
pgsw2015$taxreg <- set_labels(pgsw2015$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2015$taxreg) <- "Tax regime"                                                           

pgsw2015$forpol <- rescale(pgsw2015$forpol, c(0,1))
pgsw2015$forpol <- set_labels(pgsw2015$forpol, labels = c("Foreign policy should be based on political and economic independence" = 0,
                                                          "Foreign policy should be based on close cooperation with the EU" = 1))
var_label(pgsw2015$forpol) <- "Foreign policy"  

pgsw2015$immigr <- rescale(pgsw2015$immigr, c(0,1))
pgsw2015$immigr <- set_labels(pgsw2015$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0,
                                                          "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2015$immigr) <- "Immigration"  

pgsw2015$socpol <- rescale(pgsw2015$socpol, c(0,1))
pgsw2015$socpol <- set_labels(pgsw2015$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2015$socpol) <- "Social policy"  

pgsw2015$private <- rescale(pgsw2015$private, c(0,1))
pgsw2015$private <- set_labels(pgsw2015$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2015$private) <- "Privatisation"  

pgsw2015$abort <- rescale(pgsw2015$abort, c(0,1))
pgsw2015$abort <- set_labels(pgsw2015$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2015$abort) <- "Abortion"

pgsw2019$leftrt <- rescale(pgsw2019$leftrt, c(0,1))
pgsw2019$leftrt <- set_labels(pgsw2019$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2019$leftrt) <- "Left-right self-placement"  

pgsw2019$sollib <- rescale(pgsw2019$sollib, c(0,1))
pgsw2019$sollib <- set_labels(pgsw2019$sollib, labels = c("Solidaristic" = 0, "Liberal" = 1))
var_label(pgsw2019$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2019$euinteg <- rescale(pgsw2019$euinteg, c(0,1))
pgsw2019$euinteg <- set_labels(pgsw2019$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2019$euinteg) <- "European integration"  

pgsw2019$climate <- rescale(pgsw2019$climate, c(0,1))
pgsw2019$climate <- set_labels(pgsw2019$climate, labels = c("Climate change and environmental degradation is not an important problem" = 0,
                                                            "Climate change and environmental degradation is the most important problem facing Poland" = 1))
var_label(pgsw2019$climate) <- "Climate change"  

pgsw2019$taxreg <- rescale(pgsw2019$taxreg, c(0,1))
pgsw2019$taxreg <- set_labels(pgsw2019$taxreg, labels = c("Progressive tax regime" = 0, "Flat tax regime" = 1))
var_label(pgsw2019$taxreg) <- "Tax regime"     

pgsw2019$forpol <- rescale(pgsw2019$forpol, c(0,1))
pgsw2019$forpol <- set_labels(pgsw2019$forpol, labels = c("Foreign policy should be based on political and economic independence" = 0, "Foreign policy should be based on close cooperation with the EU" = 1))
var_label(pgsw2019$forpol) <- "Foreign policy"  

pgsw2019$immigr <- rescale(pgsw2019$immigr, c(0,1))
pgsw2019$immigr <- set_labels(pgsw2019$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0, "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2019$immigr) <- "Immigration"  

pgsw2019$socpol <- rescale(pgsw2019$socpol, c(0,1))
pgsw2019$socpol <- set_labels(pgsw2019$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, "People should take care of their own welfare" = 1))
var_label(pgsw2019$socpol) <- "Social policy"  

pgsw2019$lgbt <- rescale(pgsw2019$lgbt, c(0,1))
pgsw2019$lgbt <- set_labels(pgsw2019$lgbt, labels = c("Same-sex couples should not have the same rights as heterosexuals to publicly display their lifestyle" = 0, "Same-sex couples should have the same rights as heterosexuals to publicly display their lifestyle" = 1))
var_label(pgsw2019$lgbt) <- "LGBT rights"  

pgsw2019$abort <- rescale(pgsw2019$abort, c(0,1))
pgsw2019$abort <- set_labels(pgsw2019$abort, labels = c("There should be no right to abortion" = 0, "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2019$abort) <- "Abortion"  

#####Merge and save files#####
pgsw_all <- add_rows(pgsw1997, pgsw2001, pgsw2005, pgsw2007, pgsw2011, pgsw2015, pgsw2019)
sjPlot::view_df(pgsw_all, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)
export(pgsw_all, "PGSW master file.Rdata")
export(pgsw_all, "PGSW master file.dta")