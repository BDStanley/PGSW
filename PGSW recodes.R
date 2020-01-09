###########PREPARE WORKSPACE##########
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(depmixS4); library(rstan); library(brms); library(sjPlot);
library(hrbrthemes)

##########1997##########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw1997/PGSW 1997.sav')

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

mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw1997, nstates=3,
           family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")),
           respstart=runif(39))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw1997$ses <- posterior.states$state
pgsw1997$ses <- fct_recode(pgsw1997$ses, "Low"="2", "Medium"="1", "High"="3")
pgsw1997$ses <- fct_relevel(pgsw1997$ses, "Low", "Medium", "High")
var_label(pgsw1997$ses) <- "Socio-economic status"

pgsw1997 <- mutate(pgsw1997, union = NA) %>%
  mutate(union=replace(union, read$tumemb97==4, "No")) %>%
  mutate(union=replace(union, read$tumemb97 %in% c(2,3), "Yes")) %>%
  as_factor(union)
pgsw1997$union <- fct_relevel(pgsw1997$union, "No", "Yes")
var_label(pgsw1997$union) <- "Union membership"

##########2001###########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2001/PGSW 2001.sav')

pgsw2001 <- tibble(1:1794)

#####2001: General variables#####
colnames(pgsw2001) <- "n"
var_label(pgsw2001$n) <- "ID number"

pgsw2001$year <- 2001
pgsw2001$year <- as_factor(pgsw2001$year)
var_label(pgsw2001$year) <- "Year of election"

pgsw2001$weight <- read$wghtsamp
var_label(pgsw2001$weight) <- "Weight"

#####2001: Voting behaviour#####
pgsw2001$voted <- recode_factor(read$pt01_017,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2001$voted <- fct_drop(pgsw2001$voted)
var_label(pgsw2001$voted) <- "Voted in most recent parliamentary election"

pgsw2001$votefor <- recode_factor(read$vt01_018,
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
pgsw2001$votefor <- replace(pgsw2001$votefor, read$vt01_018 %in% c(8,11,13,14,15), "Other")
pgsw2001$votefor <- fct_expand(pgsw2001$votefor, "Did not vote")
pgsw2001$votefor <- replace(pgsw2001$votefor, pgsw2001$voted=="No", "Did not vote")
var_label(pgsw2001$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2001$votefor_t <- recode_factor(read$vt01_018,
                                    `1` = "Left",
                                    `2` = "Right",
                                    `3` = "Liberal",
                                    `4` = "Left",
                                    `5` = "Right",
                                    `6` = "Other",
                                    `7` = "Liberal",
                                    `10` = "Right")
pgsw2001$votefor_t <- replace(pgsw2001$votefor_t, read$vt01_018 %in% c(8,11,13,14,15), "Other")
pgsw2001$votefor_t <- fct_expand(pgsw2001$votefor_t, "Did not vote")
pgsw2001$votefor_t <- replace(pgsw2001$votefor_t, pgsw2001$voted=="No", "Did not vote")
pgsw2001$votefor_t <- fct_relevel(pgsw2001$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2001$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2001$voted_pr <- recode_factor(read$pt97_030,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2001$voted_pr <- fct_drop(pgsw2001$voted_pr)
var_label(pgsw2001$voted_pr) <- "Voted in previous parliamentary election"

pgsw2001$votefor_pr <- recode_factor(read$vt97_031,
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
pgsw2001$votefor_pr <- replace(pgsw2001$votefor_pr, read$vt97_031 %in% c(12,13,21), "Other")
pgsw2001$votefor_pr <- fct_expand(pgsw2001$votefor_pr, "Did not vote")
pgsw2001$votefor_pr <- replace(pgsw2001$votefor_pr, pgsw2001$voted_pr=="No", "Did not vote")
var_label(pgsw2001$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2001$votefor_t_pr <- recode_factor(read$vt97_031,
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
pgsw2001$votefor_t_pr <- replace(pgsw2001$votefor_t_pr, read$vt97_031 %in% c(12,13,21), "Other")
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

pgsw2001 <- mutate(pgsw2001, partyid = factor(case_when(read$parid041==1 ~ "Yes",
                                                        read$parid041==2 ~ "No",
                                                        read$parid041==7 ~ "Don't know")))
pgsw2001$partyid <- fct_relevel(pgsw2001$partyid, "Yes", "No", "Don't know")
var_label(pgsw2001$partyid) <- "Party identification"

pgsw2001$likeSLD <- dplyr::recode(read$liksl052, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeSLD  <- add_labels(pgsw2001$likeSLD , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likeSLD) <- "Feeling toward SLD"  

pgsw2001$likePSL <- dplyr::recode(read$likps057, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likePSL  <- add_labels(pgsw2001$likePSL , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likePSL) <- "Feeling toward PSL"  

pgsw2001$likeUW <- dplyr::recode(read$likuw054, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeUW  <- add_labels(pgsw2001$likeUW , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likeUW) <- "Feeling toward UW"  

pgsw2001$likeSRP <- dplyr::recode(read$liksa055, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeSRP  <- add_labels(pgsw2001$likeSRP , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likeSRP) <- "Feeling toward SRP"  

pgsw2001$likeAWS <- dplyr::recode(read$likaw053, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeAWS  <- add_labels(pgsw2001$likeAWS , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likeAWS) <- "Feeling toward AWS"  

pgsw2001$likeLPR <- dplyr::recode(read$liklp059, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeLPR  <- add_labels(pgsw2001$likeLPR , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likeLPR) <- "Feeling toward LPR"

pgsw2001$likePO <- dplyr::recode(read$likpo058, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likePO  <- add_labels(pgsw2001$likePO , labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2001$likePO) <- "Feeling toward PO"

#####2001: Values and attitudes#####
pgsw2001$leftrt <- dplyr::recode(read$lrslf073, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$leftrt <- add_labels(pgsw2001$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2001$leftrt) <- "Left-right self-placement"

pgsw2001$euinteg <- dplyr::recode(read$p_eu0101, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$euinteg <- rescale(pgsw2001$euinteg, c(0,1))
pgsw2001$euinteg <- add_labels(pgsw2001$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2001$euinteg) <- "European integration"

pgsw2001$chstdiv <- dplyr::recode(read$p_rel097, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$chstdiv <- rescale(pgsw2001$chstdiv, c(0,1))
pgsw2001$chstdiv <- add_labels(pgsw2001$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2001$chstdiv) <- "Church/state divide" 

pgsw2001$taxreg <- dplyr::recode(read$p_tax100, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$taxreg <- rescale(pgsw2001$taxreg, c(0,1))
pgsw2001$taxreg <- add_labels(pgsw2001$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2001$taxreg) <- "Tax regime" 

pgsw2001$socpol <- dplyr::recode(read$p_soc103, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$socpol <- rescale(pgsw2001$socpol, c(0,1))
pgsw2001$socpol <- add_labels(pgsw2001$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2001$socpol) <- "Social policy" 

pgsw2001$unemp <- dplyr::recode(read$p_une099, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$unemp <- rescale(pgsw2001$unemp, c(0,1))
pgsw2001$unemp <- add_labels(pgsw2001$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2001$unemp) <- "Unemployment" 

pgsw2001$private <- dplyr::recode(read$p_prv096, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$private <- rescale(pgsw2001$private, c(0,1))
pgsw2001$private <- add_labels(pgsw2001$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2001$private) <- "Privatisation" 

pgsw2001$crime <- dplyr::recode(read$p_crm095, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$crime <- rescale(pgsw2001$crime, c(0,1))
pgsw2001$crime <- add_labels(pgsw2001$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
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

#####2001: Socio-demographic variables#####
pgsw2001 <- mutate(pgsw2001, gender = factor(case_when(read$sex__310==1 ~ "Male",
                                                       read$sex__310==2 ~ "Female")))
pgsw2001$gender <- fct_relevel(pgsw2001$gender, "Male", "Female")
var_label(pgsw2001$gender) <- "Gender"

pgsw2001 <- mutate(pgsw2001, region = factor(case_when(read$resid350==1 ~ "Village",
                                                       read$resid350==2 ~ "Town < 19999",
                                                       read$resid350==3 ~ "Town 20000 - 49999",
                                                       read$resid350==4 ~ "Town 50000-99999",
                                                       read$resid350==5 ~ "Town 100000 - 499999",
                                                       read$resid350==6 ~ "Town > 500000")))
pgsw2001$region <- fct_relevel(pgsw2001$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2001$region) <- "Size of region in which respondent lives"

pgsw2001 <- mutate(pgsw2001, age = 2001-read$yrbir309)
var_label(pgsw2001$age) <- "Age"

pgsw2001 <- mutate(pgsw2001, edlevel = factor(case_when(read$educ_311==1 | read$educ_311==2 | read$educ_311==3  ~ "Basic or none",
                                                        read$educ_311==4 ~ "Basic vocational",
                                                        read$educ_311 %in% c(5:9) ~ "Secondary",
                                                        read$educ_311==10 | read$educ_311==11  ~ "Higher")))
pgsw2001$edlevel <- fct_relevel(pgsw2001$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2001$edlevel) <- "Level of education"

pgsw2001 <- mutate(pgsw2001, relig = factor(case_when(read$chrat365==1 | read$chrat365==2  ~ "Never",
                                                      read$chrat365==3 | read$chrat365==4  ~ "Seldom",
                                                      read$chrat365==5 | read$chrat365==6  ~ "Often",
                                                      read$chrat365==7 | read$chrat365==8  ~ "At least weekly")))
pgsw2001$relig <- fct_relevel(pgsw2001$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2001$relig) <- "Religious attendance"

pgsw2001 <- mutate(pgsw2001, hincq = read$hhinc343) %>%
  mutate(hincq = replace(hincq, read$hhinc343 %in% c(99997:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2001$hincq) <- "Household income (quintile)"

pgsw2001 <- mutate(pgsw2001, occup = NA) %>%
  mutate(occup=replace(occup, read$occup329 %in% c(41:93), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$occup329 %in% c(22, 23, 24, 32, 33, 34), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$occup329 %in% c(11, 12, 13, 21, 31), "Managers and professionals")) %>%
  mutate(occup=replace(occup, is.na(read$occup329), "Outside the labour market")) %>%
  as_factor(occup)
pgsw2001$occup <- fct_relevel(pgsw2001$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2001$occup) <- "Occupation"

mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw2001, nstates=3,
           family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")),
           respstart=runif(39))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw2001$ses <- posterior.states$state
pgsw2001$ses <- fct_recode(pgsw2001$ses, "Low"="1", "Medium"="2", "High"="3")
var_label(pgsw2001$ses) <- "Socio-economic status"

pgsw2001 <- mutate(pgsw2001, union = NA) %>%
  mutate(union=replace(union, read$tum01357==4, "No")) %>%
  mutate(union=replace(union, read$tum01357 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2001$union <- fct_relevel(pgsw2001$union, "No", "Yes")
var_label(pgsw2001$union) <- "Union membership"

##########2005#########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2005/PGSW 2005.sav')
read <- read[read$wave==2,]
pgsw2005 <- tibble(1:1201)

#####2005: General variables#####
colnames(pgsw2005) <- "n"
var_label(pgsw2005$n) <- "ID number"

pgsw2005$year <- 2005
pgsw2005$year <- as_factor(pgsw2005$year)
var_label(pgsw2005$year) <- "Year of election"

pgsw2005$weight <- read$wght1
var_label(pgsw2005$weight) <- "Weight"

#####2005: Voting behaviour#####
pgsw2005$voted <- recode_factor(read$p29,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2005$voted <- fct_drop(pgsw2005$voted)
var_label(pgsw2005$voted) <- "Voted in most recent parliamentary election"

pgsw2005$votefor <- recode_factor(read$p31,
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
pgsw2005$votefor <- replace(pgsw2005$votefor, read$p31 %in% c(9, 14, 17, 90), "Other")
pgsw2005$votefor <- fct_expand(pgsw2005$votefor, "Did not vote")
pgsw2005$votefor[pgsw2005$voted=="No"] <- "Did not vote"
var_label(pgsw2005$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2005$votefor_t <- recode_factor(read$p31,
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
pgsw2005$votefor_t <- replace(pgsw2005$votefor_t, read$p31 %in% c(9, 14, 17, 90), "Other")
pgsw2005$votefor_t <- fct_expand(pgsw2005$votefor_t, "Did not vote")
pgsw2005$votefor_t[pgsw2005$voted=="No"] <- "Did not vote"
pgsw2005$votefor_t <- fct_relevel(pgsw2005$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2005$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2005$voted_pr <- recode_factor(read$p38,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2005$voted_pr <- fct_drop(pgsw2005$voted_pr)
var_label(pgsw2005$voted_pr) <- "Voted in previous parliamentary election"

pgsw2005$votefor_pr <- recode_factor(read$p39,
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
pgsw2005$votefor_pr <- replace(pgsw2005$votefor_pr, read$p39 %in% c(8,9,11,12,14,15), "Other")
pgsw2005$votefor_pr <- fct_expand(pgsw2005$votefor_pr, "Did not vote")
pgsw2005$votefor_pr[pgsw2005$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2005$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2005$votefor_t_pr <- recode_factor(read$p39,
                                       `1` = "Left",
                                       `2` = "Right",
                                       `3` = "Liberal",
                                       `4` = "Left",
                                       `5` = "Right",
                                       `6` = "Other",
                                       `7` = "Liberal",
                                       `10` = "Right")
pgsw2005$votefor_t_pr <- fct_drop(pgsw2005$votefor_t_pr)
pgsw2005$votefor_t_pr <- replace(pgsw2005$votefor_t_pr, read$p39 %in% c(8,9,11,12,14,15), "Other")
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

pgsw2005 <- mutate(pgsw2005, partyid = factor(case_when(read$r22==1 ~ "Yes",
                                                        read$r22==2 ~ "No",
                                                        read$r22==7 ~ "Don't know")))
pgsw2005$partyid <- fct_relevel(pgsw2005$partyid, "Yes", "No", "Don't know")
var_label(pgsw2005$partyid) <- "Party identification"

pgsw2005$vtpres <- recode_factor(read$r36,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw2005$vtpres) <- "Voted in first round of most recent presidential election"

pgsw2005 <- mutate(pgsw2005, vtforpres = factor(case_when(read$r37==1 ~ "Bochniarz",
                                                          read$r37==2 ~ "Borowski",
                                                          read$r37==4 ~ "Giertych",
                                                          read$r37==5 ~ "Ilasz",
                                                          read$r37==6 ~ "Kaczyński",
                                                          read$r37==7 ~ "Kalinowski",
                                                          read$r37==8 ~ "Korwin-Mikke",
                                                          read$r37==9 ~ "Lepper",
                                                          read$r37==12 ~ "Słomka",
                                                          read$r37==13 ~ "Tusk",
                                                          read$r37==99 ~ "Did not vote")))
var_label(pgsw2005$vtforpres) <- "Candidate voted for in first round of most recent presidential election"

pgsw2005$vtpres_2 <- recode_factor(read$r41,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw2005$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw2005 <- mutate(pgsw2005, vtforpres_2 = factor(case_when(read$r42==1 ~ "Kaczyński",
                                                            read$r42==2 ~ "Tusk",
                                                            read$r41==2 ~ "Did not vote")))
var_label(pgsw2005$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"

pgsw2005$likePiS <- dplyr::recode(read$p17d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePiS <- add_labels(pgsw2005$likePiS, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likePiS) <- "Feeling toward PiS"  

pgsw2005$likePO <- dplyr::recode(read$p17f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePO <- add_labels(pgsw2005$likePO, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likePO) <- "Feeling toward PO" 

pgsw2005$likePSL <- dplyr::recode(read$p17g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePSL <- add_labels(pgsw2005$likePSL, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likePSL) <- "Feeling toward PSL" 

pgsw2005$likeSLD <- dplyr::recode(read$p17e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSLD <- add_labels(pgsw2005$likeSLD, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likeSLD) <- "Feeling toward SLD" 

pgsw2005$likeSdPL <- dplyr::recode(read$p17c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSdPL <- add_labels(pgsw2005$likeSdPL, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likeSdPL) <- "Feeling toward SdPL" 

pgsw2005$likePD<- dplyr::recode(read$p17b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePD <- add_labels(pgsw2005$likePD, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likePD) <- "Feeling toward PD" 

pgsw2005$likeSRP<- dplyr::recode(read$p17h, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSRP <- add_labels(pgsw2005$likeSRP, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likeSRP) <- "Feeling toward SRP" 

pgsw2005$likeLPR<- dplyr::recode(read$p17a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeLPR <- add_labels(pgsw2005$likeLPR, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2005$likeLPR) <- "Feeling toward LPR" 

#####2005: Values and attitudes#####
pgsw2005$leftrt <- dplyr::recode(read$p20, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)

pgsw2005$leftrt <- add_labels(pgsw2005$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2005$leftrt) <- "Left-right self-placement"

pgsw2005$euinteg <- dplyr::recode(read$p65g, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$euinteg <- rescale(pgsw2005$euinteg, c(0,1))
pgsw2005$euinteg <- add_labels(pgsw2005$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2005$euinteg) <- "European integration"

pgsw2005$chstdiv <- dplyr::recode(read$p65c, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$chstdiv <- rescale(pgsw2005$chstdiv, c(0,1))
pgsw2005$chstdiv <- add_labels(pgsw2005$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2005$chstdiv) <- "Church/state divide" 

pgsw2005$taxreg <- dplyr::recode(read$p65f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$taxreg <- rescale(pgsw2005$taxreg, c(0,1))
pgsw2005$taxreg <- add_labels(pgsw2005$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2005$taxreg) <- "Tax regime" 

pgsw2005$socpol <- dplyr::recode(read$p65i, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$socpol <- rescale(pgsw2005$socpol, c(0,1))
pgsw2005$socpol <- add_labels(pgsw2005$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2005$socpol) <- "Social policy" 

pgsw2005$unemp <- dplyr::recode(read$p65e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$unemp <- rescale(pgsw2005$unemp, c(0,1))
pgsw2005$unemp <- add_labels(pgsw2005$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2005$unemp) <- "Unemployment" 

pgsw2005$private <- dplyr::recode(read$p65b, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$private <- rescale(pgsw2005$private, c(0,1))
pgsw2005$private <- add_labels(pgsw2005$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2005$private) <- "Privatisation" 

pgsw2005$abort <- dplyr::recode(read$p65l, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$abort <- rescale(pgsw2005$abort, c(0,1))
pgsw2005$abort <- add_labels(pgsw2005$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2005$abort) <- "Abortion" 

pgsw2005$crime <- dplyr::recode(read$p65a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$crime <- rescale(pgsw2005$crime, c(0,1))
pgsw2005$crime <- add_labels(pgsw2005$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2005$crime) <- "Crime" 

pgsw2005$immigr <- dplyr::recode(read$p65m, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$immigr <- rescale(pgsw2005$immigr, c(0,1))
pgsw2005$immigr <- add_labels(pgsw2005$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0,
                                                        "The state should encourage people from other countries to immigrate to Poland" = 1))
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

#####2005: Socio-demographic variables#####
pgsw2005 <- mutate(pgsw2005, gender = factor(case_when(read$m2==1 ~ "Male",
                                                       read$m2==2 ~ "Female")))
pgsw2005$gender <- fct_relevel(pgsw2005$gender, "Male", "Female")
var_label(pgsw2005$gender) <- "Gender"

pgsw2005 <- mutate(pgsw2005, region = factor(case_when(read$m27==1 ~ "Village",
                                                       read$m27==2 ~ "Town < 19999",
                                                       read$m27==3 ~ "Town 20000 - 49999",
                                                       read$m27==4 ~ "Town 50000-99999",
                                                       read$m27==5 ~ "Town 100000 - 499999",
                                                       read$m27==6 ~ "Town > 500000")))
pgsw2005$region <- fct_relevel(pgsw2005$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2005$region) <- "Size of region in which respondent lives"

pgsw2005 <- mutate(pgsw2005, age = 2005-read$m1)
var_label(pgsw2005$age) <- "Age"

pgsw2005 <- mutate(pgsw2005, edlevel = factor(case_when(read$m3==1 | read$m3==2 | read$m3==3  ~ "Basic or none",
                                                        read$m3==4 ~ "Basic vocational",
                                                        read$m3 %in% c(5:9) ~ "Secondary",
                                                        read$m3==10 | read$m3==11  ~ "Higher")))
pgsw2005$edlevel <- fct_relevel(pgsw2005$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2005$edlevel) <- "Level of education"

pgsw2005 <- mutate(pgsw2005, relig = factor(case_when(read$m36==1 | read$m36==2  ~ "Never",
                                                      read$m36==3 | read$m36==4  ~ "Seldom",
                                                      read$m36==5 | read$m36==6  ~ "Often",
                                                      read$m36==7 | read$m36==8  ~ "At least weekly")))
pgsw2005$relig <- fct_relevel(pgsw2005$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2005$relig) <- "Religious attendance"

pgsw2005 <- mutate(pgsw2005, hincq = read$m21) %>%
  mutate(hincq = replace(hincq, read$m21 %in% c(99992:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2005$hincq) <- "Household income (quintile)"

pgsw2005 <- mutate(pgsw2005, occup = NA) %>%
  mutate(occup=replace(occup, read$isco %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$isco %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$isco %in% c(2220, 2221, 2222, 2223, 2224, 2229, 2230, 2300, 
                                               2310, 2320, 2350, 2351, 2359, 2420, 2421, 2422, 
                                               2429, 2430, 2431, 2432, 2440, 2442, 2443, 2445, 
                                               2451, 2460, 2330, 2331, 2332, 2340, 2352, 2444, 
                                               2446, 2450, 2452, 2453, 2454, 2455, 3220, 3222, 
                                               3223, 3224, 3226, 3229, 3230, 3231, 3232, 3300, 
                                               3310, 3320, 3330, 3340, 3400, 3450, 3460, 3470, 
                                               3472, 3473, 3474, 3475, 3480), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$isco %in% c(3221, 3225, 3227, 3228), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$isco==9999, "Outside the labour market")) %>%
  as_factor(occup)
pgsw2005$occup <- fct_relevel(pgsw2005$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2005$occup) <- "Occupation"

mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw2005, nstates=3,
           family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")),
           respstart=runif(39))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw2005$ses <- posterior.states$state
pgsw2005$ses <- fct_recode(pgsw2005$ses, "Low"="1", "Medium"="2", "High"="3")
var_label(pgsw2005$ses) <- "Socio-economic status"

pgsw2005 <- mutate(pgsw2005, union = NA) %>%
  mutate(union=replace(union, read$m32==4, "No")) %>%
  mutate(union=replace(union, read$m32 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2005$union <- fct_relevel(pgsw2005$union, "No", "Yes")
var_label(pgsw2005$union) <- "Union membership"

##########2007##########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2007/PGSW 2007.sav')

pgsw2007 <- tibble(1:1817)

#####2007: General variables#####
colnames(pgsw2007) <- "n"
var_label(pgsw2007$n) <- "ID number"

pgsw2007$year <- 2007
pgsw2007$year <- as_factor(pgsw2007$year)
var_label(pgsw2007$year) <- "Year of election"

pgsw2007$weight <- read$waga
var_label(pgsw2007$weight) <- "Weight"

#####2007: Voting behaviour#####
pgsw2007$voted <- recode_factor(read$c26,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2007$voted <- fct_drop(pgsw2007$voted)
var_label(pgsw2007$voted) <- "Voted in most recent parliamentary election"

pgsw2007$votefor <- recode_factor(read$c28,
                                  `1` = "PPP",
                                  `2` = "LPR",
                                  `3` = "PiS",
                                  `4` = "PO",
                                  `5` = "PSL",
                                  `6` = "SRP",
                                  `8` = "LiD")
pgsw2007$votefor <- fct_drop(pgsw2007$votefor)
pgsw2007$votefor <- fct_expand(pgsw2007$votefor, "Other")
pgsw2007$votefor <- replace(pgsw2007$votefor, read$c28 %in% c(7,9), "Other")
pgsw2007$votefor <- fct_expand(pgsw2007$votefor, "Did not vote")
pgsw2007$votefor[pgsw2007$voted=="No"] <- "Did not vote"
var_label(pgsw2007$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2007$votefor_t <- recode_factor(read$c28,
                                    `1` = "Left",
                                    `2` = "Right",
                                    `3` = "Right",
                                    `4` = "Liberal",
                                    `5` = "Other",
                                    `6` = "Left",
                                    `8` = "Left")
pgsw2007$votefor_t <- fct_drop(pgsw2007$votefor_t)
pgsw2007$votefor_t <- replace(pgsw2007$votefor_t, read$c28 %in% c(7,9), "Other")
pgsw2007$votefor_t <- fct_expand(pgsw2007$votefor_t, "Did not vote")
pgsw2007$votefor_t[pgsw2007$voted=="No"] <- "Did not vote"
pgsw2007$votefor_t <- fct_relevel(pgsw2007$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2007$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2007$voted_pr <- recode_factor(read$c32,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2007$voted_pr <- fct_drop(pgsw2007$voted_pr)
var_label(pgsw2007$voted_pr) <- "Voted in previous parliamentary election"

pgsw2007$votefor_pr <- recode_factor(read$c33,
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

pgsw2007$votefor_t_pr <- recode_factor(read$c33,
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

pgsw2007 <- mutate(pgsw2007, partyid = factor(case_when(read$c22==1 ~ "Yes",
                                                        read$c22==2 ~ "No",
                                                        read$c22==7 ~ "Don't know")))
pgsw2007$partyid <- fct_relevel(pgsw2007$partyid, "Yes", "No", "Don't know")
var_label(pgsw2007$partyid) <- "Party identification"

pgsw2007$likePiS <- dplyr::recode(read$c14a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePiS <- add_labels(pgsw2007$likePiS, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2007$likePiS) <- "Feeling toward PiS"  

pgsw2007$likePO <- dplyr::recode(read$c14b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePO <- add_labels(pgsw2007$likePO, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2007$likePO) <- "Feeling toward PO" 

pgsw2007$likePSL <- dplyr::recode(read$c14c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePSL <- add_labels(pgsw2007$likePSL, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2007$likePSL) <- "Feeling toward PSL" 

pgsw2007$likeLiD <- dplyr::recode(read$c14d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likeLiD <- add_labels(pgsw2007$likeLiD, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2007$likeLiD) <- "Feeling toward LiD" 

#####2007: Values and attitudes#####
pgsw2007$leftrt <- dplyr::recode(read$c17, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$leftrt <- add_labels(pgsw2007$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2007$leftrt) <- "Left-right self-placement"

pgsw2007$euinteg <- dplyr::recode(read$p49f, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$euinteg <- rescale(pgsw2007$euinteg, c(0,1))
pgsw2007$euinteg <- add_labels(pgsw2007$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2007$euinteg) <- "European integration"

pgsw2007$taxreg <- dplyr::recode(read$p49e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$taxreg <- rescale(pgsw2007$taxreg, c(0,1))
pgsw2007$taxreg <- add_labels(pgsw2007$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2007$taxreg) <- "Tax regime" 

pgsw2007$unemp <- dplyr::recode(read$p49d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$unemp <- rescale(pgsw2007$unemp, c(0,1))
pgsw2007$unemp <- add_labels(pgsw2007$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2007$unemp) <- "Unemployment" 

pgsw2007$private <- dplyr::recode(read$p49b, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$private <- rescale(pgsw2007$private, c(0,1))
pgsw2007$private <- add_labels(pgsw2007$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2007$private) <- "Privatisation" 

pgsw2007$abort <- dplyr::recode(read$p49i, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$abort <- rescale(pgsw2007$abort, c(0,1))
pgsw2007$abort <- add_labels(pgsw2007$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2007$abort) <- "Abortion" 

pgsw2007$crime <- dplyr::recode(read$p49a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$crime <- rescale(pgsw2007$crime, c(0,1))
pgsw2007$crime <- add_labels(pgsw2007$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
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

#####2007: Socio-demographic variables#####
pgsw2007 <- mutate(pgsw2007, gender = factor(case_when(read$m2==1 ~ "Male",
                                                       read$m2==2 ~ "Female")))
pgsw2007$gender <- fct_relevel(pgsw2007$gender, "Male", "Female")
var_label(pgsw2007$gender) <- "Gender"

pgsw2007 <- mutate(pgsw2007, region = factor(case_when(read$m33==1 ~ "Village",
                                                       read$m33==2 ~ "Town < 19999",
                                                       read$m33==3 ~ "Town 20000 - 49999",
                                                       read$m33==4 ~ "Town 50000-99999",
                                                       read$m33==5 ~ "Town 100000 - 499999",
                                                       read$m33==6 ~ "Town > 500000")))
pgsw2007$region <- fct_relevel(pgsw2007$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2007$region) <- "Size of region in which respondent lives"

pgsw2007 <- mutate(pgsw2007, age = 2007-read$m1)
var_label(pgsw2007$age) <- "Age"

pgsw2007 <- mutate(pgsw2007, edlevel = factor(case_when(read$m3==1 | read$m3==2 | read$m3==3  ~ "Basic or none",
                                                        read$m3==4 ~ "Basic vocational",
                                                        read$m3 %in% c(5:9) ~ "Secondary",
                                                        read$m3==10 | read$m3==11  ~ "Higher")))
pgsw2007$edlevel <- fct_relevel(pgsw2007$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2007$edlevel) <- "Level of education"

pgsw2007 <- mutate(pgsw2007, relig = factor(case_when(read$m42==1 | read$m42==2  ~ "Never",
                                                      read$m42==3 | read$m42==4  ~ "Seldom",
                                                      read$m42==5 | read$m42==6  ~ "Often",
                                                      read$m42==7 | read$m42==8  ~ "At least weekly")))
pgsw2007$relig <- fct_relevel(pgsw2007$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2007$relig) <- "Religious attendance"

pgsw2007 <- mutate(pgsw2007, hincq = read$m21) %>%
  mutate(hincq = replace(hincq, read$m21 %in% c(99992:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2007$hincq) <- "Household income (quintile)"

pgsw2007 <- mutate(pgsw2007, occup = NA) %>%
  mutate(occup=replace(occup, read$isco88 %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$isco88 %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$isco88 %in% c(2220, 2221, 2222, 2223, 2224, 2229, 2230, 2300, 
                                               2310, 2320, 2350, 2351, 2359, 2420, 2421, 2422, 
                                               2429, 2430, 2431, 2432, 2440, 2442, 2443, 2445, 
                                               2451, 2460, 2330, 2331, 2332, 2340, 2352, 2444, 
                                               2446, 2450, 2452, 2453, 2454, 2455, 3220, 3222, 
                                               3223, 3224, 3226, 3229, 3230, 3231, 3232, 3300, 
                                               3310, 3320, 3330, 3340, 3400, 3450, 3460, 3470, 
                                               3472, 3473, 3474, 3475, 3480), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$isco88 %in% c(3221, 3225, 3227, 3228), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$isco88==9997, "Outside the labour market")) %>%
  as_factor(occup)
pgsw2007$occup <- fct_relevel(pgsw2007$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2007$occup) <- "Occupation"

mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw2007, nstates=3,
           family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")),
           respstart=runif(39))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw2007$ses <- posterior.states$state
pgsw2007$ses <- fct_recode(pgsw2007$ses, "Low"="3", "Medium"="1", "High"="2")
var_label(pgsw2007$ses) <- "Socio-economic status"

pgsw2007 <- mutate(pgsw2007, union = NA) %>%
  mutate(union=replace(union, read$m38==4, "No")) %>%
  mutate(union=replace(union, read$m38 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2007$union <- fct_relevel(pgsw2007$union, "No", "Yes")
var_label(pgsw2007$union) <- "Union membership"

###########2011##########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2011/PGSW 2011.sav')

pgsw2011 <- tibble(1:1919)

#####2011: General variables#####
colnames(pgsw2011) <- "n"
var_label(pgsw2011$n) <- "ID number"

pgsw2011$year <- 2011
pgsw2011$year <- as_factor(pgsw2011$year)
var_label(pgsw2011$year) <- "Year of election"

pgsw2011$weight <- read$waga
var_label(pgsw2011$weight) <- "Weight"

#####2011: Voting behaviour#####
pgsw2011$voted <- recode_factor(read$C8a,
                                `2` = "No",
                                `1` = "Yes",)
pgsw2011$voted <- fct_drop(pgsw2011$voted)
var_label(pgsw2011$voted) <- "Voted in most recent parliamentary election"

pgsw2011$votefor <- recode_factor(read$C8b,
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
pgsw2011$votefor <- replace(pgsw2011$votefor, read$C8b %in% c(10,11), "Other")
pgsw2011$votefor <- fct_expand(pgsw2011$votefor, "Did not vote")
pgsw2011$votefor[pgsw2011$voted=="No"] <- "Did not vote"
var_label(pgsw2011$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2011$votefor_t <- recode_factor(read$C8b,
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
pgsw2011$votefor_t <- replace(pgsw2011$votefor_t, read$C8b %in% c(10,11), "Other")
pgsw2011$votefor_t <- fct_expand(pgsw2011$votefor_t, "Did not vote")
pgsw2011$votefor_t[pgsw2011$voted=="No"] <- "Did not vote"
pgsw2011$votefor_t <- fct_relevel(pgsw2011$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2011$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2011$voted_pr <- recode_factor(read$C9,
                                   `2` = "No",
                                   `1` = "Yes",)
pgsw2011$voted_pr <- fct_drop(pgsw2011$voted_pr)
var_label(pgsw2011$voted_pr) <- "Voted in previous parliamentary election"

pgsw2011$votefor_pr <- recode_factor(read$C9b,
                                     `1` = "PPP",
                                     `2` = "LPR",
                                     `3` = "PiS",
                                     `4` = "PO",
                                     `5` = "PSL",
                                     `6` = "SRP",
                                     `8` = "LiD")
pgsw2011$votefor_pr <- fct_drop(pgsw2011$votefor_pr)
pgsw2011$votefor_pr <- fct_expand(pgsw2011$votefor_pr, "Other")
pgsw2011$votefor_pr <- replace(pgsw2011$votefor_pr, read$C9b %in% c(7,10), "Other")
pgsw2011$votefor_pr <- fct_expand(pgsw2011$votefor_pr, "Did not vote")
pgsw2011$votefor_pr[pgsw2011$voted_pr=="No"] <- "Did not vote"
var_label(pgsw2011$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2011$votefor_t_pr <- recode_factor(read$C9b,
                                       `1` = "Left",
                                       `2` = "Right",
                                       `3` = "Right",
                                       `4` = "Liberal",
                                       `5` = "Other",
                                       `6` = "Left",
                                       `8` = "Left")
pgsw2011$votefor_t_pr <- fct_drop(pgsw2011$votefor_t_pr)
pgsw2011$votefor_t_pr <- replace(pgsw2011$votefor_t_pr, read$C9b %in% c(7,10), "Other")
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

pgsw2011 <- mutate(pgsw2011, partyid = factor(case_when(read$C19==1 ~ "Yes",
                                                        read$C19==5 ~ "No",
                                                        read$C19==7 ~ "Don't know")))
pgsw2011$partyid <- fct_relevel(pgsw2011$partyid, "Yes", "No", "Don't know")
var_label(pgsw2011$partyid) <- "Party identification"

pgsw2011$likePiS <- dplyr::recode(read$C12A, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePiS <- add_labels(pgsw2011$likePiS, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2011$likePiS) <- "Feeling toward PiS"  

pgsw2011$likePO <- dplyr::recode(read$C12B, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePO <- add_labels(pgsw2011$likePO, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2011$likePO) <- "Feeling toward PO" 

pgsw2011$likePSL <- dplyr::recode(read$C12C, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePSL <- add_labels(pgsw2011$likePSL, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2011$likePSL) <- "Feeling toward PSL" 

pgsw2011$likeSLD <- dplyr::recode(read$C12D, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likeSLD <- add_labels(pgsw2011$likeSLD, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2011$likeSLD) <- "Feeling toward SLD" 

pgsw2011$likeRP <- dplyr::recode(read$C12E, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likeRP <- add_labels(pgsw2011$likeRP, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2011$likeRP) <- "Feeling toward RP" 

#####2011: Values and attitudes#####
pgsw2011$leftrt <- dplyr::recode(read$C15, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$leftrt <- add_labels(pgsw2011$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2011$leftrt) <- "Left-right self-placement"  

pgsw2011$euinteg <- dplyr::recode(read$PI36E, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L)
pgsw2011$euinteg <- rescale(pgsw2011$euinteg, c(0,1))
pgsw2011$euinteg <- add_labels(pgsw2011$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2011$euinteg) <- "European integration"   

pgsw2011$chstdiv <- dplyr::recode(read$PI36B, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$chstdiv <- rescale(pgsw2011$chstdiv, c(0,1))
pgsw2011$chstdiv <- add_labels(pgsw2011$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2011$chstdiv) <- "Church/state divide"                                                           

pgsw2011$taxreg <- dplyr::recode(read$PI36D, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$taxreg <- rescale(pgsw2011$taxreg, c(0,1))
pgsw2011$taxreg <- add_labels(pgsw2011$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2011$taxreg) <- "Tax regime"                                                           

pgsw2011$unemp <- dplyr::recode(read$PI36C, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$unemp <- rescale(pgsw2011$unemp, c(0,1))
pgsw2011$unemp <- add_labels(pgsw2011$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2011$unemp) <- "Unemployment"  

pgsw2011$socpol <- dplyr::recode(read$PI36G, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$socpol <- rescale(pgsw2011$socpol, c(0,1))
pgsw2011$socpol <- add_labels(pgsw2011$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2011$socpol) <- "Social policy"  

pgsw2011$private <- dplyr::recode(read$PI36A, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$private <- rescale(pgsw2011$private, c(0,1))
pgsw2011$private <- add_labels(pgsw2011$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2011$private) <- "Privatisation"  

pgsw2011$abort <- dplyr::recode(read$PI36H, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$abort <- rescale(pgsw2011$abort, c(0,1))
pgsw2011$abort <- add_labels(pgsw2011$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
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

#####2011: Socio-demographic variables#####
pgsw2011 <- mutate(pgsw2011, gender = factor(case_when(read$PLEC==1 ~ "Male",
                                                       read$PLEC==2 ~ "Female")))
pgsw2011$gender <- fct_relevel(pgsw2011$gender, "Male", "Female")
var_label(pgsw2011$gender) <- "Gender"

pgsw2011 <- mutate(pgsw2011, region = factor(case_when(read$KLM6==1 ~ "Village",
                                                       read$KLM6==2 ~ "Town < 19999",
                                                       read$KLM6==3 ~ "Town 20000 - 49999",
                                                       read$KLM6==4 ~ "Town 50000-99999",
                                                       read$KLM6==5 ~ "Town 100000 - 499999",
                                                       read$KLM6==6 ~ "Town > 500000")))
pgsw2011$region <- fct_relevel(pgsw2011$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2011$region) <- "Size of region in which respondent lives"

pgsw2011 <- mutate(pgsw2011, age = read$age)
var_label(pgsw2011$age) <- "Age"

pgsw2011 <- mutate(pgsw2011, edlevel = factor(case_when(read$W1==1 | read$W1==2 | read$W1==3  ~ "Basic or none",
                                                        read$W1==4 ~ "Basic vocational",
                                                        read$W1 %in% c(5:9) ~ "Secondary",
                                                        read$W1==10 | read$W1==11  ~ "Higher")))
pgsw2011$edlevel <- fct_relevel(pgsw2011$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2011$edlevel) <- "Level of education"

pgsw2011 <- mutate(pgsw2011, relig = factor(case_when(read$M42==1 | read$M42==2  ~ "Never",
                                                      read$M42==3 | read$M42==4  ~ "Seldom",
                                                      read$M42==5 | read$M42==6  ~ "Often",
                                                      read$M42==7 | read$M42==8  ~ "At least weekly")))
pgsw2011$relig <- fct_relevel(pgsw2011$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2011$relig) <- "Religious attendance"

pgsw2011 <- mutate(pgsw2011, hincq = read$M21) %>%
  mutate(hincq = replace(hincq, read$M21 %in% c(99992:99999), NA)) %>%
  mutate(hincq, hincq = xtile(hincq, 5)) %>%
  as_factor(hincq)
var_label(pgsw2011$hincq) <- "Household income (quintile)"

pgsw2011 <- mutate(pgsw2011, occup = NA) %>%
  mutate(occup=replace(occup, read$ISCO %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$ISCO %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$ISCO %in% c(2211, 2212, 2261, 2250, 2262, 2263, 2269, 1342, 2221, 
                                               2222, 3221, 3222, 2310, 2320, 2330, 2341, 2342, 2352,
                                               2351, 2353, 2354, 2355, 2356, 2359, 2611, 2612, 2619, 
                                               2621, 2622, 2632, 2633, 2643, 2634, 2635, 2431, 2432, 
                                               2641, 2642, 2651, 2652, 2653, 2654, 2655, 2636, 2263, 
                                               3257, 2265, 2267, 3254, 2264, 2269, 3255, 3259, 2230, 
                                               2266, 2267, 2269, 3259, 3221, 3222, 2230, 3230, 3413, 
                                               2341, 2342, 2352, 2353, 2355, 2356, 2359, 3153, 3423, 
                                               3435, 5165, 3355, 3411, 3412, 2642, 2656, 2652, 2653, 
                                               2659, 3421, 3422, 3423, 3413), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$ISCO %in% c(2240, 3253, 3256, 3251, 3240, 3213), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$ISCO %in% c(9996:9999), "Outside the labour market")) %>%
  as_factor(occup)
pgsw2011$occup <- fct_relevel(pgsw2011$occup, "Blue collar and clerical", "Sociocultural professionals", "Managers and professionals", "Outside the labour market")
var_label(pgsw2011$occup) <- "Occupation"

mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw2011, nstates=3,
           family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")),
           respstart=runif(39))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw2011$ses <- posterior.states$state
pgsw2011$ses <- fct_recode(pgsw2011$ses, "Low"="2", "Medium"="1", "High"="3")
var_label(pgsw2011$ses) <- "Socio-economic status"

pgsw2011 <- mutate(pgsw2011, union = NA) %>%
  mutate(union=replace(union, read$M38==5, "No")) %>%
  mutate(union=replace(union, read$M38 %in% c(1:4), "Yes")) %>%
  as_factor(union)
pgsw2011$union <- fct_relevel(pgsw2011$union, "No", "Yes")
var_label(pgsw2011$union) <- "Union membership"

###########2015##########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW2015/PGSW 2015.sav')

pgsw2015 <- tibble(1:1733)

#####2015: General variables#####
colnames(pgsw2015) <- "n"
var_label(pgsw2015$n) <- "ID number"

pgsw2015$year <- 2015
pgsw2015$year <- as_factor(pgsw2015$year)
var_label(pgsw2015$year) <- "Year of election"

pgsw2015$weight <- read$WAGA
var_label(pgsw2015$weight) <- "Weight"


#####2015: Voting behaviour#####
pgsw2015$voted <- recode_factor(read$q31,
                         `2` = "No",
                         `1` = "Yes",)
pgsw2015$voted <- fct_drop(pgsw2015$voted)
var_label(pgsw2015$voted) <- "Voted in most recent parliamentary election"

pgsw2015$votefor <- recode_factor(read$q32,
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

pgsw2015$votefor_t <- recode_factor(read$q32,
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

pgsw2015$voted_pr <- recode_factor(read$q38,
                       `2` = "No",
                       `1` = "Yes",)
pgsw2015$voted_pr <- fct_drop(pgsw2015$voted_pr)
var_label(pgsw2015$voted_pr) <- "Voted in previous parliamentary election"

pgsw2015$votefor_pr <- recode_factor(read$q39,
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

pgsw2015$votefor_t_pr <- recode_factor(read$q39,
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

pgsw2015 <- mutate(pgsw2015, partyid = factor(case_when(read$q105==1 ~ "Yes",
                                                        read$q105==2 ~ "No",
                                                        read$q105==7 ~ "Don't know")))
pgsw2015$partyid <- fct_relevel(pgsw2015$partyid, "Yes", "No", "Don't know")
var_label(pgsw2015$partyid) <- "Party identification"

pgsw2015 <- mutate(pgsw2015, vtpres = factor(if_else(read$q115==95, "No", "Yes"))) %>%
  mutate(vtpres=replace(vtpres, read$q115==9, NA)) %>%
  mutate(vtpres=replace(vtpres, read$q115==10, NA)) %>%
  mutate(vtpres=replace(vtpres, read$q115==11, NA))
var_label(pgsw2015$vtpres) <- "Voted in first round of most recent presidential election"

pgsw2015 <- mutate(pgsw2015, vtforpres = factor(case_when(read$q115==1 ~ "Komorowski",
                                                       read$q115==2 ~ "Duda",
                                                       read$q115==3 ~ "Kukiz",
                                                       read$q115==4 ~ "Korwin-Mikke",
                                                       read$q115==5 ~ "Ogórek",
                                                       read$q115==6 ~ "Other",
                                                       read$q115==7 ~ "Did not vote")))
var_label(pgsw2015$vtforpres) <- "Candidate voted for in first round of most recent presidential election"

pgsw2015 <- mutate(pgsw2015, vtpres_2 = factor(if_else(read$q118==3, "No", "Yes"))) %>%
  mutate(vtpres_2=replace(vtpres_2, read$q118==4, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read$q118==6, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read$q118==7, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read$q118==8, NA))
var_label(pgsw2015$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw2015 <- mutate(pgsw2015, vtforpres_2 = factor(case_when(read$q118==1 ~ "Komorowski",
                                                          read$q118==2 ~ "Duda",
                                                          read$q118==3 ~ "Did not vote")))
var_label(pgsw2015$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"



pgsw2015$likePiS <- dplyr::recode(read$q42, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePiS <- add_labels(pgsw2015$likePiS, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likePiS) <- "Feeling toward PiS"  

pgsw2015$likePO <- dplyr::recode(read$q45, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePO <- add_labels(pgsw2015$likePO, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likePO) <- "Feeling toward PO" 

pgsw2015$likePSL <- dplyr::recode(read$q48, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePSL <- add_labels(pgsw2015$likePSL, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likePSL) <- "Feeling toward PSL" 

pgsw2015$likeZL <- dplyr::recode(read$q51, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeZL <- add_labels(pgsw2015$likeZL, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likeZL) <- "Feeling toward ZL" 

pgsw2015$likeKukiz <- dplyr::recode(read$q54, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeKukiz <- add_labels(pgsw2015$likeKukiz, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likeKukiz) <- "Feeling toward Kukiz'15" 

pgsw2015$likeNowo<- dplyr::recode(read$q57, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeNowo <- add_labels(pgsw2015$likeNowo, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likeNowo) <- "Feeling toward Nowoczesna" 

pgsw2015$likeKorwin<- dplyr::recode(read$q60, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeKorwin <- add_labels(pgsw2015$likeKorwin, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likeKorwin) <- "Feeling toward KORWiN" 

pgsw2015$likeRazem<- dplyr::recode(read$q63, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeRazem <- add_labels(pgsw2015$likeRazem, labels = c("Dislike" = 0, "Like" = 10))
var_label(pgsw2015$likeRazem) <- "Feeling toward Razem" 

pgsw2015$lrPiS <- dplyr::recode(read$q72, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPiS <- add_labels(pgsw2015$lrPiS, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrPiS) <- "Left-right placement of PiS"  

pgsw2015$lrPO <- dplyr::recode(read$q75, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPO <- add_labels(pgsw2015$lrPO, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrPO) <- "Left-right placement of PO"  

pgsw2015$lrRazem <- dplyr::recode(read$q78, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrRazem <- add_labels(pgsw2015$lrRazem, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrRazem) <- "Left-right placement of Razem"  

pgsw2015$lrZL <- dplyr::recode(read$q87, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrZL <- add_labels(pgsw2015$lrZL, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrZL) <- "Left-right placement of United Left" 

pgsw2015$lrKukiz <- dplyr::recode(read$q90, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrKukiz <- add_labels(pgsw2015$lrKukiz, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrKukiz) <- "Left-right placement of Kukiz'15" 

pgsw2015$lrNowo <- dplyr::recode(read$q93, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrNowo <- add_labels(pgsw2015$lrNowo, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrNowo) <- "Left-right placement of Nowoczesna" 

pgsw2015$lrKorwin <- dplyr::recode(read$q81, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrKorwin <- add_labels(pgsw2015$lrKorwin, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrKorwin) <- "Left-right placement of KORWiN" 

pgsw2015$lrPSL <- dplyr::recode(read$q84, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPSL <- add_labels(pgsw2015$lrPSL, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$lrPSL) <- "Left-right placement of PSL" 


#####2015: Values and attitudes#####
pgsw2015$leftrt <- dplyr::recode(read$q96, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$leftrt <- add_labels(pgsw2015$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2015$leftrt) <- "Left-right self-placement"  

pgsw2015$sollib <- dplyr::recode(read$q102, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$sollib <- add_labels(pgsw2015$sollib, labels = c("Solidaristic" = 0, "Liberal" = 10))
var_label(pgsw2015$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2015$euinteg <- dplyr::recode(read$q99, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$euinteg <- rescale(pgsw2015$euinteg, c(0,1))
pgsw2015$euinteg <- add_labels(pgsw2015$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2015$euinteg) <- "European integration"   

pgsw2015$chstdiv <- dplyr::recode(read$L1q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$chstdiv <- rescale(pgsw2015$chstdiv, c(0,1))
pgsw2015$chstdiv <- add_labels(pgsw2015$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2015$chstdiv) <- "Church/state divide"                                                           
                               
pgsw2015$taxreg <- dplyr::recode(read$L2q144, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2015$taxreg <- rescale(pgsw2015$taxreg, c(0,1))
pgsw2015$taxreg <- add_labels(pgsw2015$taxreg, labels = c("Progressive tax regime" = 0,
                                                            "Flat tax regime" = 1))
var_label(pgsw2015$taxreg) <- "Tax regime"                                                           

pgsw2015$forpol <- dplyr::recode(read$L3q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$forpol <- rescale(pgsw2015$forpol, c(0,1))
pgsw2015$forpol <- add_labels(pgsw2015$forpol, labels = c("Foreign policy should be based on political and economic independence" = 0,
                                                            "Foreign policy should be based on close cooperation with the EU" = 1))
var_label(pgsw2015$forpol) <- "Foreign policy"  

pgsw2015$immigr <- dplyr::recode(read$L4q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$immigr <- rescale(pgsw2015$immigr, c(0,1))
pgsw2015$immigr <- add_labels(pgsw2015$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0,
                                                          "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2015$immigr) <- "Immigration"  

pgsw2015$socpol <- dplyr::recode(read$L5q144, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2015$socpol <- rescale(pgsw2015$socpol, c(0,1))
pgsw2015$socpol <- add_labels(pgsw2015$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2015$socpol) <- "Social policy"  

pgsw2015$private <- dplyr::recode(read$L6q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$private <- rescale(pgsw2015$private, c(0,1))
pgsw2015$private <- add_labels(pgsw2015$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                          "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2015$private) <- "Privatisation"  

pgsw2015$abort <- dplyr::recode(read$L7q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$abort <- rescale(pgsw2015$abort, c(0,1))
pgsw2015$abort <- add_labels(pgsw2015$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
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

pgsw2015$strgman <- as.factor(dplyr::recode(read$L1q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$strgman <- add_labels(pgsw2015$strgman, labels = c("Definitely no" = 1,
                                                        "Definitely yes" = 5))
var_label(pgsw2015$strgman) <- "Necessity of a strong leader"  

pgsw2015$elitpow <- as.factor(dplyr::recode(read$L2q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$elitpow <- add_labels(pgsw2015$elitpow, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$elitpow) <- "Political elites arrogate power to themselves"  

pgsw2015$crisis <- as.factor(dplyr::recode(read$L3q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$crisis <- add_labels(pgsw2015$crisis, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$crisis) <- "Last chance to rescue Poland from crisis"  

pgsw2015$easyref <- as.factor(dplyr::recode(read$L4q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$easyref <- add_labels(pgsw2015$easyref, labels = c("Definitely no" = 1,
                                                          "Definitely yes" = 5))
var_label(pgsw2015$easyref) <- "Solving the problems before us is very easy"  

pgsw2015$goodevil <- as.factor(dplyr::recode(read$L5q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$goodevil <- add_labels(pgsw2015$goodevil, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$goodevil) <- "In politics everything is either good or evil"  

pgsw2015$unkelit <- as.factor(dplyr::recode(read$L1q290, `1` = 5L, `2` = 4L, `5` = 3L, `3` = 2L, `4` = 1L))
pgsw2015$unkelit <- add_labels(pgsw2015$unkelit, labels = c("Definitely no" = 1,
                                                              "Definitely yes" = 5))
var_label(pgsw2015$unkelit) <- "It is not the government which rules, but unknown elites"  

pgsw2015$conspir <- as.factor(dplyr::recode(read$L2q290, `1` = 5L, `2` = 4L, `5` = 3L, `3` = 2L, `4` = 1L))
pgsw2015$conspir <- add_labels(pgsw2015$conspir, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$conspir) <- "Those who see conspiracies against us are in many senses right"  

HS.model <- ' populism  =~ strgman + elitpow + crisis + easyref + goodevil + unkelit + conspir '

fit <- cfa(HS.model, data=pgsw2015, ordered=c("strgman","elitpow", "crisis", "easyref", "goodevil", "unkelit", "conspir"))

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2015[idx, fs] <- fscores[ , fs]
}

pgsw2015$populism <- scales::rescale(pgsw2015$populism, c(0,1))
var_label(pgsw2015$populism) <- "Index of populism"  

pgsw2015$polpast <- dplyr::recode_factor(read$L1q21, 
                                            `5` = "Got much worse", 
                                            `4` = "Got somewhat worse", 
                                            `3` = "Did not change", 
                                            `2` = "Got somewhat better", 
                                            `1` = "Got much better",
                                         .ordered=TRUE)
var_label(pgsw2015$polpast) <- "Attitudes to recent political situation in Poland" 

pgsw2015$ecpast <- dplyr::recode_factor(read$L2q21, 
                                         `5` = "Got much worse", 
                                         `4` = "Got somewhat worse", 
                                         `3` = "Did not change", 
                                         `2` = "Got somewhat better", 
                                         `1` = "Got much better",
                                         .ordered=TRUE)
var_label(pgsw2015$ecpast) <- "Attitudes to recent economic situation in Poland" 

pgsw2015$hecpast <- dplyr::recode_factor(read$L3q21, 
                                        `5` = "Got much worse", 
                                        `4` = "Got somewhat worse", 
                                        `3` = "Did not change", 
                                        `2` = "Got somewhat better", 
                                        `1` = "Got much better",
                                        .ordered=TRUE)
var_label(pgsw2015$hecpast) <- "Attitudes to recent household economic situation" 

pgsw2015$polcurr <- dplyr::recode_factor(read$L1q23, 
                                         `5` = "Very bad", 
                                         `4` = "Bad", 
                                         `3` = "Neither good nor bad", 
                                         `2` = "Good", 
                                         `1` = "Very good",
                                         .ordered=TRUE)
var_label(pgsw2015$polcurr) <- "Attitudes to current political situation in Poland" 

pgsw2015$eccurr <- dplyr::recode_factor(read$L2q23, 
                                        `5` = "Very bad", 
                                        `4` = "Bad", 
                                        `3` = "Neither good nor bad", 
                                        `2` = "Good", 
                                        `1` = "Very good",
                                         .ordered=TRUE)
var_label(pgsw2015$eccurr) <- "Attitudes to current economic situation in Poland" 

pgsw2015$heccurr <- dplyr::recode_factor(read$L3q23, 
                                         `5` = "Very bad", 
                                         `4` = "Bad", 
                                         `3` = "Neither good nor bad", 
                                         `2` = "Good", 
                                         `1` = "Very good",
                                        .ordered=TRUE)
var_label(pgsw2015$heccurr) <- "Attitudes to current household economic situation" 

pgsw2015$polfut <- dplyr::recode_factor(read$L1q25, 
                                         `5` = "Will get much worse", 
                                         `4` = "Will get somewhat worse", 
                                         `3` = "Will not change", 
                                         `2` = "Will get somewhat better", 
                                         `1` = "Will get much better",
                                         .ordered=TRUE)
var_label(pgsw2015$polfut) <- "Attitudes to future political situation in Poland" 

pgsw2015$ecfut <- dplyr::recode_factor(read$L2q25, 
                                       `5` = "Will get much worse", 
                                       `4` = "Will get somewhat worse", 
                                       `3` = "Will not change", 
                                       `2` = "Will get somewhat better", 
                                       `1` = "Will get much better",
                                        .ordered=TRUE)
var_label(pgsw2015$ecfut) <- "Attitudes to future economic situation in Poland" 

pgsw2015$hecfut <- dplyr::recode_factor(read$L3q25, 
                                        `5` = "Will get much worse", 
                                        `4` = "Will get somewhat worse", 
                                        `3` = "Will not change", 
                                        `2` = "Will get somewhat better", 
                                        `1` = "Will get much better",
                                       .ordered=TRUE)
var_label(pgsw2015$hecfut) <- "Attitudes to future household economic situation" 

HS.model <- ' pocketbook  =~ ecpast + eccurr + hecpast + heccurr '

fit <- cfa(HS.model, data=pgsw2015, ordered=c("ecpast","eccurr", "hecpast", "heccurr"))

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2015[idx, fs] <- fscores[ , fs]
}

#####2015: Socio-demographic variables#####
pgsw2015 <- mutate(pgsw2015, gender = factor(case_when(read$q12==1 ~ "Male",
                                                          read$q12==2 ~ "Female")))
pgsw2015$gender <- fct_relevel(pgsw2015$gender, "Male", "Female")
var_label(pgsw2015$gender) <- "Gender"

pgsw2015 <- mutate(pgsw2015, region = factor(case_when(read$q13==1 ~ "Village",
                                                       read$q13==2 ~ "Town < 19999",
                                                       read$q13==3 ~ "Town 20000 - 49999",
                                                       read$q13==4 ~ "Town 50000-99999",
                                                       read$q13==5 ~ "Town 100000 - 499999",
                                                       read$q13==6 ~ "Town > 500000")))
pgsw2015$region <- fct_relevel(pgsw2015$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2015$region) <- "Size of region in which respondent lives"

pgsw2015 <- mutate(pgsw2015, age = 2015-read$q14)
var_label(pgsw2015$age) <- "Age"

pgsw2015 <- mutate(pgsw2015, edlevel = factor(case_when(read$q296==1 | read$q296==2  ~ "Basic or none",
                                                        read$q296==3 | read$q296==4 | read$q296==5 | read$q296==7 ~ "Basic vocational",
                                                        read$q296==6 | read$q296==8 | read$q296==9 ~ "Secondary",
                                                        read$q296==10 | read$q296==11 | read$q296==12 ~ "Higher")))
pgsw2015$edlevel <- fct_relevel(pgsw2015$edlevel, "Basic or none", "Basic vocational", "Secondary", "Higher")
var_label(pgsw2015$edlevel) <- "Level of education"

pgsw2015 <- mutate(pgsw2015, relig = factor(case_when(read$q337==1 | read$q337==2  ~ "Never",
                                                        read$q337==3 | read$q337==4  ~ "Seldom",
                                                        read$q337==5 | read$q337==6  ~ "Often",
                                                        read$q337==7 | read$q337==8  ~ "At least weekly")))
pgsw2015$relig <- fct_relevel(pgsw2015$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2015$relig) <- "Religious attendance"

pgsw2015 <- mutate(pgsw2015, income = case_when(read$q326==1 | read$q323>=0 & read$q323<=300 ~ 1,
                                                read$q326==2 | read$q323>=301 & read$q323<=500 ~ 2,
                                                read$q326==3 | read$q323>=501 & read$q323<=750 ~ 3,
                                                read$q326==4 | read$q323>=751 & read$q323<=1000 ~ 4,
                                                read$q326==5 | read$q323>=1001 & read$q323<=1250 ~ 5,
                                                read$q326==6 | read$q323>=1251 & read$q323<=1500 ~ 6,
                                                read$q326==7 | read$q323>=1501 & read$q323<=1750 ~ 7,
                                                read$q326==8 | read$q323>=1751 & read$q323<=2000 ~ 8,
                                                read$q326==9 | read$q323>=2001 & read$q323<=2250 ~ 9,
                                                read$q326==10 | read$q323>=2251 & read$q323<=2500 ~ 10,
                                                read$q326==11 | read$q323>=2501 & read$q323<=2750 ~ 11,
                                                read$q326==12 | read$q323>=2751 & read$q323<=3000 ~ 12,
                                                read$q326==13 | read$q323>=3001 & read$q323<=3500 ~ 13,
                                                read$q326==14 | read$q323>=3501 & read$q323<=4000 ~ 14,
                                                read$q326==15 | read$q323>=4001 & read$q323<=4500 ~ 15,
                                                read$q326==16 | read$q323>=4501 & read$q323<=5000 ~ 16,
                                                read$q326==17 | read$q323>=5001 & read$q323<=6000 ~ 17,
                                                read$q326==18 | read$q323>=6001 & read$q323<=8000 ~ 18,
                                                read$q326==19 | read$q323>=8001 & read$q323<=10000 ~ 19,
                                                read$q326==20 | read$q323>=10000 & read$q323<=35000 ~ 20))
var_label(pgsw2015$income) <- "Income"

pgsw2015 <- mutate(pgsw2015, hincq = read$q323) %>%
  mutate(hincq, hincq = ntile(hincq, 5)) %>%
  mutate(hincq=replace(hincq, read$q326 %in% c(1:6), 1)) %>%
  mutate(hincq=replace(hincq, read$q326 %in% c(7:8), 2)) %>%
  mutate(hincq=replace(hincq, read$q326 %in% c(9:12), 3)) %>%
  mutate(hincq=replace(hincq, read$q326 %in% c(13:16), 4)) %>%
  mutate(hincq=replace(hincq, read$q326 %in% c(17:20), 5)) %>%
  as_factor(hincq)
var_label(pgsw2015$hincq) <- "Household income (quintile)"

pgsw2015 <- mutate(pgsw2015, occup = NA) %>%
  mutate(occup=replace(occup, read$ISCO08_1 %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$ISCO08_1 %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$ISCO08_1 %in% c(2211, 2212, 261, 2250, 2262, 2263, 2269, 1342, 2221, 2222, 
                                                   3221, 3222, 2310, 2320, 2330, 2341, 2342, 2352, 2351, 2353, 
                                                   2354, 2355, 2356, 2359, 2611, 2612, 2619, 2621, 2622, 2632, 
                                                   2633, 2643, 2634, 2635, 2431, 2432, 2641, 2642, 2651, 2652, 
                                                   2653, 2654, 2655, 2636, 2263, 3257, 2265, 2267, 3254, 2264, 
                                                   2269, 3255, 3259, 2230, 2266, 2267, 2269, 3259, 3221, 3222, 
                                                   2230, 3230, 3413, 2341, 2342, 2352, 2353, 2355, 2356, 2359, 
                                                   3153, 3423, 3435, 5165, 3355, 3411, 3412, 2642, 2656, 2652, 
                                                   2653, 2659, 3421, 3422, 3423, 3413), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$ISCO08_1 %in% c(2240, 3253, 3256, 3251, 3240, 3213), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$ISCO08_2 %in% c(1000:3999), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$ISCO08_2 %in% c(4000:9629), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$ISCO08_2 %in% c(2211, 2212, 261, 2250, 2262, 2263, 2269, 1342, 2221, 2222, 
                                                   3221, 3222, 2310, 2320, 2330, 2341, 2342, 2352, 2351, 2353, 
                                                   2354, 2355, 2356, 2359, 2611, 2612, 2619, 2621, 2622, 2632, 
                                                   2633, 2643, 2634, 2635, 2431, 2432, 2641, 2642, 2651, 2652, 
                                                   2653, 2654, 2655, 2636, 2263, 3257, 2265, 2267, 3254, 2264, 
                                                   2269, 3255, 3259, 2230, 2266, 2267, 2269, 3259, 3221, 3222, 
                                                   2230, 3230, 3413, 2341, 2342, 2352, 2353, 2355, 2356, 2359, 
                                                   3153, 3423, 3435, 5165, 3355, 3411, 3412, 2642, 2656, 2652, 
                                                   2653, 2659, 3421, 3422, 3423, 3413), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$ISCO08_2 %in% c(2240, 3253, 3256, 3251, 3240, 3213), "Managers and professionals")) %>%
  mutate(occup=replace(occup, is.na(read$ISCO08_1 | read$ISCO08_2), "Outside the labour market")) %>%
  as_factor(occup)
var_label(pgsw2015$occup) <- "Occupation"

mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw2015, nstates=3,
           family=list(multinomial("identity"), multinomial("identity"), multinomial("identity")),
           respstart=runif(39))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw2015$ses <- posterior.states$state
pgsw2015$ses <- fct_recode(pgsw2015$ses, "Low"="3", "Medium"="2", "High"="1")
pgsw2015$ses <- fct_relevel(pgsw2015$ses, "Low", "Medium", "High")
var_label(pgsw2015$ses) <- "Socio-economic status"

pgsw2015 <- mutate(pgsw2015, union = NA) %>%
  mutate(union=replace(union, read$q333==5, "No")) %>%
  mutate(union=replace(union, read$q333 %in% c(1:4), "Yes")) %>%
  as_factor(union)
var_label(pgsw2015$union) <- "Union membership"


##########2019##########
read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2019/PGSW2019_CAWI_wave_1.sav')
read <- read_spss('~/Desktop/Parallels Shared Folders/Google Drive/Resources/Datasets/Poland/pgsw2019/PGSW2019_CAWI_wave_1.sav')
read$populism <- as.ordered(read$P26)
read$vote <- as.factor(read$P2_sejm)
read$leftrt <- as.ordered(read$P14a)
read$relig <- as.ordered(read$P47)
read$weight <- as.numeric(read$waga)
read$wiek <- as.numeric(read$wiek)
sjPlot::view_df(read, show.frq=TRUE, show.prc=TRUE, weight.by="waga", show.wtd.frq=TRUE, show.wtd.prc=TRUE, max.len=50, use.viewer=FALSE)

fit <- brm(ordered(populism) | weights(waga) ~ vote, 
    data=read, family=cumulative(), save_all_pars=TRUE)

fit_mo <- brm(as.factor(vote) | weights(waga) ~ mo(relig), 
           data=read, family=categorical(), save_all_pars=TRUE)

HS.model <- ' simplism  =~ S1a + S1b + S1c + S1d + S1e + S1f '

read$listprob <- read$Q23a
read$amongp <- 8-read$Q23b
read$willp <- read$Q23c
read$intgrp <- read$Q23d
read$admins <- 8-read$Q23e
read$cheats <- read$Q23f
read$goodbad <- read$Q23g
read$disevil <- 8-read$Q23h
read$disinfo <- 8-read$Q23i

fit <- cfa(HS.model, data=read, ordered=c("S1a", "S1b", "S1c", "S1d", "S1e",
                                          "S1f"))

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  read[idx, fs] <- fscores[ , fs]
}



read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2019/PGSW2019_CAPI.sav')
read <- read_spss('~/Desktop/Parallels Shared Folders/Google Drive/Resources/Datasets/Poland/pgsw2019/PGSW2019_CAPI.sav')
read <- read_spss("/mnt/hgfs/Google Drive/Resources/Datasets/Poland/PGSW2019/PGSW2019_CAPI.sav")

pgsw2019 <- tibble(1:2003)
pgsw2019$weight <- read$waga

pgsw2019$voted <- recode_factor(read$Q12LHa,
                                `5` = "No",
                                `6` = "No",
                                `1` = "Yes")
var_label(pgsw2019$voted) <- "Voted in most recent parliamentary election"

pgsw2019$votefor <- recode_factor(read$Q12LHb,
                                    `2` = "PiS",
                                    `5` = "KO",
                                    `3` = "Lewica",
                                    `1` = "PSL-Kukiz",
                                    `4` = "Konfederacja")
pgsw2019$votefor <- fct_expand(pgsw2019$votefor, "Did not vote")
pgsw2019$votefor[pgsw2019$voted=="No"] <- "Did not vote"
var_label(pgsw2019$votefor) <- "Party voted for in most recent parliamentary election"

pgsw2019$strlead <- dplyr::recode_factor(read$Q04_5, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$strlead) <- "Having a strong leader is good, even if that leader bends the rules to get things done"


pgsw2019$imgcult <- dplyr::recode_factor(read$Q05_4, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgcult) <- "The presence of immigrants has a negative impact on Polish culture"

pgsw2019$lgbtfree <- dplyr::recode_factor(read$I1f,
                                          `7` = "No, they shouldn't", 
                                          `6` = "2", 
                                         `5` = "3", 
                                         `4` = "4", 
                                         `3` = "5", 
                                         `2` = "6", 
                                         `1` = "Yes, they should",
                                         .ordered=TRUE)
var_label(pgsw2019$lgbtfree) <- "Should LGBT people have the same right as heterosexuals to publicly display their lifestyles?"

pgsw2019$socwelf <- dplyr::recode_factor(read$I1e,
                                          `1` = "The state should provide for individuals", 
                                          `2` = "2", 
                                          `3` = "3", 
                                          `4` = "4", 
                                          `5` = "5", 
                                          `6` = "6", 
                                          `7` = "Individuals should provide for themselves",
                                          .ordered=TRUE)
var_label(pgsw2019$socwelf) <- "Attitudes to social welfare"

fit_mo_1 <- brm(as.factor(votefor) | weights(weight) ~ mo(lgbtfree) + mo(socwelf), 
              data=pgsw2019, family=categorical(), save_all_pars=TRUE)

marg <- ggpredict(fit_mo, terms="socwelf")
plotdata <- marg
plotdata$x <- fct_inorder(plotdata$x, ordered=TRUE)
pdat <- melt(plotdata, measure.vars=c("x"))

cols <- c("PiS"="blue4", "KO"="orange", "PSL-Kukiz"="darkgreen", "Konfederacja" = "black", "Lewica" = "red", "Did not vote"="gray50")


p <- ggplot() +
  geom_linerange(pdat, mapping=aes(x=fct_inorder(value), ymin=conf.low, ymax=conf.high, color=response.level), 
                 position = position_dodge2(width = 0.5)) +
  geom_point(pdat, mapping=aes(x=fct_inorder(value), y=predicted, color=response.level), 
             size=3, position = position_dodge2(width = 0.5)) +
  scale_colour_manual(name="", values=cols,
                      breaks=c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Did not vote"),
                      labels=c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Did not vote")) +
  labs(x ="", y = "Probability of voting for party",
       subtitle = "Attitudes to social welfare", caption = "Source: PGSW (2019)") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "plot.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4.5)
ggsave(p, file = "plot.pdf", width = 7, height = 5, units = "cm", dpi = 320, scale = 4.5, device = cairo_pdf)

ggplot() +
  geom_point(marg, mapping=aes(x=predicted, ymin=conf.low, ymax=conf.high)) 

#####MERGE AND SAVE FILES#####
pgsw_all <- bind_rows(pgsw1997, pgsw2001, pgsw2005, pgsw2007, pgsw2011, pgsw2015)
write_stata(pgsw_all, path='/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW master.dta', version=14)
library(sjmisc)
pgsw_all <- add_rows(pgsw1997, pgsw2001, pgsw2005, pgsw2007, pgsw2011, pgsw2015)
save(pgsw_all, file = "/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW_master.RData")
