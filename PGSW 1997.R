#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(depmixS4); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id('https://drive.google.com/file/d/1xOF84UYRhEuasZCM1QoRnWR0u_-V97JR/view?usp=sharing'), overwrite=TRUE)
read <- read_spss('PGSW 1997.sav')
pgsw1997 <- tibble(1:2003)

#####1997: General variables#####
colnames(pgsw1997) <- "n"
var_label(pgsw1997$n) <- "ID number"

pgsw1997$year <- 1997
pgsw1997$year <- as_factor(pgsw1997$year)
var_label(pgsw1997$year) <- "Year of election"

pgsw1997$weight <- read$waga
var_label(pgsw1997$weight) <- "Weight"

#####1997: Voting behaviour#####
pgsw1997$voted <- recode_factor(read$pt97_005,
                                `2` = "No",
                                `1` = "Yes",)
pgsw1997$voted <- fct_drop(pgsw1997$voted)
var_label(pgsw1997$voted) <- "Voted in most recent parliamentary election"

pgsw1997$votefor <- recode_factor(read$vt97_006,
                                  `5` = "AWS",
                                  `4` = "UW",
                                  `6` = "SLD",
                                  `7` = "PSL",
                                  `9` = "ROP",
                                  `99` = "Did not vote")
pgsw1997$votefor <- fct_drop(pgsw1997$votefor)
pgsw1997$votefor <- fct_expand(pgsw1997$votefor, "Other")
pgsw1997$votefor <- replace(pgsw1997$votefor, read$vt97_006 %in% c(1,2,3,8,10:94), "Other")
var_label(pgsw1997$votefor) <- "Party voted for in most recent parliamentary election"

pgsw1997$votefor_t <- recode_factor(read$vt97_006,
                                    `5` = "Right",
                                    `4` = "Liberal",
                                    `6` = "Left",
                                    `9` = "Right",
                                    `7` = "Other",
                                    `99` = "Did not vote")
pgsw1997$votefor_t <- replace(pgsw1997$votefor_t, read$vt97_006 %in% c(1,2,3,8,10:94), "Other")
pgsw1997$votefor_t <- fct_relevel(pgsw1997$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw1997$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw1997$voted_pr <- recode_factor(read$pt93_244,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw1997$voted_pr <- fct_drop(pgsw1997$voted_pr)
var_label(pgsw1997$voted_pr) <- "Voted in previous parliamentary election"

pgsw1997$votefor_pr <- recode_factor(read$vt93_245,
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
pgsw1997$votefor_pr <- replace(pgsw1997$votefor_pr, read$vt93_245 %in% c(15:24), "Other")
pgsw1997$votefor_pr <- fct_expand(pgsw1997$votefor_pr, "Did not vote")
pgsw1997$votefor_pr <- replace(pgsw1997$votefor_pr, pgsw1997$voted_pr=="No", "Did not vote")
var_label(pgsw1997$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw1997$votefor_t_pr <- recode_factor(read$vt93_245,
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
pgsw1997$votefor_t_pr <- replace(pgsw1997$votefor_t_pr, read$vt93_245 %in% c(15:24), "Other")
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

pgsw1997 <- mutate(pgsw1997, partyid = factor(case_when(read$var3==2 ~ "No",
                                                        read$var3==1 ~ "Yes",
                                                        read$var13==1 ~ "Yes")))
var_label(pgsw1997$partyid) <- "Party identification"

pgsw1997$vtpres <- recode_factor(read$pt95_240,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw1997$vtpres) <- "Voted in first round of most recent presidential election"

pgsw1997 <- mutate(pgsw1997, vtforpres = factor(case_when(read$vt95_241==1 ~ "Bubel",
                                                          read$vt95_241==2 ~ "Gronkiewicz-Waltz",
                                                          read$vt95_241==3 ~ "Korwin-Mikke",
                                                          read$vt95_241==5 ~ "Kuroń",
                                                          read$vt95_241==6 ~ "Kwaśniewski",
                                                          read$vt95_241==7 ~ "Lepper",
                                                          read$vt95_241==8 ~ "Olszewski",
                                                          read$vt95_241==9 ~ "Pawlak",
                                                          read$vt95_241==10 ~ "Pietrzak",
                                                          read$vt95_241==12 ~ "Wałęsa",
                                                          read$vt95_241==13 ~ "Zieliński")))
pgsw1997$vtforpres <- fct_expand(pgsw1997$vtforpres, "Did not vote")
pgsw1997$vtforpres <- replace(pgsw1997$vtforpres, read$pt95_240==2, "Did not vote")
var_label(pgsw1997$vtforpres) <- "Candidate voted for in first round of most recent presidential election"

pgsw1997$vtpres_2 <- recode_factor(read$pt95_242,
                                   `2` = "No",
                                   `1` = "Yes")
var_label(pgsw1997$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw1997 <- mutate(pgsw1997, vtforpres_2 = factor(case_when(read$vt95_243==1 ~ "Kwaśniewski",
                                                            read$vt95_243==2 ~ "Wałęsa",
                                                            read$pt95_242==2 ~ "Did not vote")))
pgsw1997$vtforpres_2 <- fct_relevel(pgsw1997$vtforpres_2, "Kwaśniewski", "Wałęsa", "Did not vote")
var_label(pgsw1997$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"

pgsw1997$likeSLD <- dplyr::recode(read$var30, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeSLD  <- add_labels(pgsw1997$likeSLD , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw1997$likeSLD) <- "Feeling toward SLD"  

pgsw1997$likePSL <- dplyr::recode(read$var32, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likePSL  <- add_labels(pgsw1997$likePSL , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw1997$likePSL) <- "Feeling toward PSL"  

pgsw1997$likeUW <- dplyr::recode(read$var34, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeUW  <- add_labels(pgsw1997$likeUW , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw1997$likeUW) <- "Feeling toward UW"  

pgsw1997$likeUP <- dplyr::recode(read$var36, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeUP  <- add_labels(pgsw1997$likeUP , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw1997$likeUP) <- "Feeling toward UP"  

pgsw1997$likeAWS <- dplyr::recode(read$var38, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeAWS  <- add_labels(pgsw1997$likeAWS , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw1997$likeAWS) <- "Feeling toward AWS"  

pgsw1997$likeROP <- dplyr::recode(read$var40, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$likeROP  <- add_labels(pgsw1997$likeROP , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw1997$likeROP) <- "Feeling toward ROP"

#####1997: Values and attitudes#####
pgsw1997$leftrt <- dplyr::recode(read$var56, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)

pgsw1997$leftrt <- add_labels(pgsw1997$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw1997$leftrt) <- "Left-right self-placement"

pgsw1997$euinteg <- dplyr::recode(read$p_eu0115, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$euinteg <- rescale(pgsw1997$euinteg, c(0,1))
pgsw1997$euinteg <- add_labels(pgsw1997$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw1997$euinteg) <- "European integration"

pgsw1997$chstdiv <- dplyr::recode(read$p_rel111, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$chstdiv <- rescale(pgsw1997$chstdiv, c(0,1))
pgsw1997$chstdiv <- add_labels(pgsw1997$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw1997$chstdiv) <- "Church/state divide" 

pgsw1997$taxreg <- dplyr::recode(read$p_tax114, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$taxreg <- rescale(pgsw1997$taxreg, c(0,1))
pgsw1997$taxreg <- add_labels(pgsw1997$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw1997$taxreg) <- "Tax regime" 

pgsw1997$socpol <- dplyr::recode(read$p_soc117, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$socpol <- rescale(pgsw1997$socpol, c(0,1))
pgsw1997$socpol <- add_labels(pgsw1997$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw1997$socpol) <- "Social policy" 

pgsw1997$unemp <- dplyr::recode(read$p_une113, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$unemp <- rescale(pgsw1997$unemp, c(0,1))
pgsw1997$unemp <- add_labels(pgsw1997$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw1997$unemp) <- "Unemployment" 

pgsw1997$private <- dplyr::recode(read$p_prv110, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$private <- rescale(pgsw1997$private, c(0,1))
pgsw1997$private <- add_labels(pgsw1997$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw1997$private) <- "Privatisation" 

pgsw1997$abort <- dplyr::recode(read$p_abo119, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw1997$abort <- rescale(pgsw1997$abort, c(0,1))
pgsw1997$abort <- add_labels(pgsw1997$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw1997$abort) <- "Abortion" 

pgsw1997$crime <- dplyr::recode(read$p_crm109, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw1997$crime <- rescale(pgsw1997$crime, c(0,1))
pgsw1997$crime <- add_labels(pgsw1997$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
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

#####1997: Socio-demographic variables#####
pgsw1997 <- mutate(pgsw1997, gender = factor(case_when(read$sex==1 ~ "Male",
                                                       read$sex==2 ~ "Female")))
pgsw1997$gender <- fct_relevel(pgsw1997$gender, "Male", "Female")
var_label(pgsw1997$gender) <- "Gender"

pgsw1997 <- mutate(pgsw1997, region = factor(case_when(read$resid312==1 ~ "Village",
                                                       read$resid312==2 ~ "Town < 19999",
                                                       read$resid312==3 ~ "Town 20000 - 49999",
                                                       read$resid312==4 ~ "Town 50000-99999",
                                                       read$resid312==5 ~ "Town 100000 - 499999",
                                                       read$resid312==6 ~ "Town > 500000")))
pgsw1997$region <- fct_relevel(pgsw1997$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw1997$region) <- "Size of region in which respondent lives"

pgsw1997 <- mutate(pgsw1997, age = read$age)
var_label(pgsw1997$age) <- "Age"

pgsw1997 <- mutate(pgsw1997, edlevel = factor(case_when(read$educ==1 | read$educ==2  ~ "Basic or none",
                                                        read$educ==3 ~ "Basic vocational",
                                                        read$educ==4 | read$educ==5 | read$educ==6 ~ "Secondary",
                                                        read$educ==7 | read$educ==8 | read$educ==9  ~ "Higher")))
pgsw1997$edlevel <- fct_relevel(pgsw1997$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw1997$edlevel) <- "Level of education"

pgsw1997 <- mutate(pgsw1997, relig = factor(case_when(read$churchat==1 | read$churchat==2  ~ "Never",
                                                      read$churchat==3 | read$churchat==4  ~ "Seldom",
                                                      read$churchat==5 | read$churchat==6  ~ "Often",
                                                      read$churchat==7 | read$churchat==8  ~ "At least weekly")))
pgsw1997$relig <- fct_relevel(pgsw1997$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw1997$relig) <- "Religious attendance"

pgsw1997 <- mutate(pgsw1997, hincq = read$hhincome) %>%
  mutate(hincq = replace(hincq, read$hhincome %in% c(9700:9999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw1997$hincq) <- "Household income (quintile)"

pgsw1997 <- mutate(pgsw1997, occup = NA) %>%
  mutate(occup=replace(occup, read$occup %in% c(41:93), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$occup %in% c(22, 23, 24, 32, 33, 34), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$occup %in% c(11, 12, 13, 21, 31), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$occup %in% c(99), "Outside the labour market")) %>%
  as_factor(occup)
pgsw1997$occup <- fct_relevel(pgsw1997$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw1997$occup) <- "Occupation"
# 
# mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw1997, nstates=3,
#            family=list(multinomial("logit"), multinomial("logit"), multinomial("logit")),
#            respstart=runif(9))
# set.seed(780045)
# ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
# posterior.states <- depmixS4::posterior(ses)
# posterior.states$state <- as.factor(posterior.states$state)
# pgsw1997$ses <- posterior.states$state
# pgsw1997$ses <- fct_recode(pgsw1997$ses, "Low"="2", "Medium"="1", "High"="3")
# pgsw1997$ses <- fct_relevel(pgsw1997$ses, "Low", "Medium", "High")
# var_label(pgsw1997$ses) <- "Socio-economic status"


# f <- cbind(edlevel, occup, hincq)~1
# lc<-poLCA(f, data=pgsw1997, nclass=3, nrep=1, maxiter=5000, graphs=TRUE, na.rm=FALSE)
# probs.start<-lc$probs.start
# new.probs.start <- poLCA.reorder(probs.start, c(3,2,1))
# lc<-poLCA(f, data=pgsw1997, nclass=3, nrep=1, maxiter=5000, probs.start=new.probs.start, na.rm=FALSE)
# saveRDS(lc$probs.start, "1997_ses_starting_values.RData")

f <- cbind(edlevel, occup, hincq)~1
probs.start <- readRDS("ses_starting_values.RData")
lc<-poLCA(f, data=pgsw1997, nclass=3, nrep=1, maxiter=5000, probs.start=probs.start, na.rm=FALSE)
post <- data.frame(lc$posterior)
colnames(post) <- c("Low", "Medium", "High")
pgsw1997$ses <- colnames(post)[max.col(post,ties.method="first")]
pgsw1997$ses <- fct_relevel(pgsw1997$ses, "Low", "Medium", "High")
var_label(pgsw1997$ses) <- "Socio-economic status"

pgsw1997 <- mutate(pgsw1997, union = NA) %>%
  mutate(union=replace(union, read$tumemb97==4, "No")) %>%
  mutate(union=replace(union, read$tumemb97 %in% c(2,3), "Yes")) %>%
  as_factor(union)
pgsw1997$union <- fct_relevel(pgsw1997$union, "No", "Yes")
var_label(pgsw1997$union) <- "Union membership"


#Create codebook
sjPlot::view_df(pgsw1997, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW1997.RData")