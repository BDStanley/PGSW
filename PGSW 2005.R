#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id("https://drive.google.com/file/d/1BwNXibqaKbI4xnGpf7AfGjaBi5vgeN7t/view?usp=sharing"), overwrite=TRUE)
read <- read_spss('PGSW 2005.sav')
read <- read[read$wave==2,]
pgsw2005 <- tibble(1:1201)

#####General variables#####
colnames(pgsw2005) <- "n"
var_label(pgsw2005$n) <- "ID number"

pgsw2005$year <- 2005
pgsw2005$year <- as_factor(pgsw2005$year)
var_label(pgsw2005$year) <- "Year of election"

pgsw2005$weight <- read$wght1
var_label(pgsw2005$weight) <- "Weight"

#####Voting behaviour#####
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
pgsw2005$likePiS <- add_labels(pgsw2005$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePiS) <- "Feeling toward PiS"  

pgsw2005$likePO <- dplyr::recode(read$p17f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePO <- add_labels(pgsw2005$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePO) <- "Feeling toward PO" 

pgsw2005$likePSL <- dplyr::recode(read$p17g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePSL <- add_labels(pgsw2005$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePSL) <- "Feeling toward PSL" 

pgsw2005$likeSLD <- dplyr::recode(read$p17e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSLD <- add_labels(pgsw2005$likeSLD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeSLD) <- "Feeling toward SLD" 

pgsw2005$likeSdPL <- dplyr::recode(read$p17c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSdPL <- add_labels(pgsw2005$likeSdPL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeSdPL) <- "Feeling toward SdPL" 

pgsw2005$likePD<- dplyr::recode(read$p17b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likePD <- add_labels(pgsw2005$likePD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likePD) <- "Feeling toward PD" 

pgsw2005$likeSRP<- dplyr::recode(read$p17h, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeSRP <- add_labels(pgsw2005$likeSRP, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeSRP) <- "Feeling toward SRP" 

pgsw2005$likeLPR<- dplyr::recode(read$p17a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$likeLPR <- add_labels(pgsw2005$likeLPR, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2005$likeLPR) <- "Feeling toward LPR" 

#####Values and attitudes#####
pgsw2005$leftrt <- dplyr::recode(read$p20, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$leftrt <- add_labels(pgsw2005$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2005$leftrt) <- "Left-right self-placement"

pgsw2005$euinteg <- dplyr::recode(read$p65g, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$euinteg <- add_labels(pgsw2005$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2005$euinteg) <- "European integration"

pgsw2005$chstdiv <- dplyr::recode(read$p65c, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$chstdiv <- add_labels(pgsw2005$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "The Church should be completely separate from the state" = 10))
var_label(pgsw2005$chstdiv) <- "Church/state divide" 

pgsw2005$taxreg <- dplyr::recode(read$p65f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$taxreg <- add_labels(pgsw2005$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "Flat tax regime" = 10))
var_label(pgsw2005$taxreg) <- "Tax regime" 

pgsw2005$socpol <- dplyr::recode(read$p65i, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$socpol <- add_labels(pgsw2005$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "People should take care of their own welfare" = 10))
var_label(pgsw2005$socpol) <- "Social policy" 

pgsw2005$unemp <- dplyr::recode(read$p65e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$unemp <- add_labels(pgsw2005$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw2005$unemp) <- "Unemployment" 

pgsw2005$private <- dplyr::recode(read$p65b, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$private <- add_labels(pgsw2005$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw2005$private) <- "Privatisation" 

pgsw2005$abort <- dplyr::recode(read$p65l, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2005$abort <- add_labels(pgsw2005$abort, labels = c("There should be no right to abortion" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 10))
var_label(pgsw2005$abort) <- "Abortion" 

pgsw2005$crime <- dplyr::recode(read$p65a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2005$crime <- add_labels(pgsw2005$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Fight crime, but with attention to citizens' rights" = 10))
var_label(pgsw2005$crime) <- "Crime" 

pgsw2005$immigr <- dplyr::recode(read$p65m, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
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

#####Socio-demographic variables#####
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
  mutate(union=replace(union, read$m32==4, "No")) %>%
  mutate(union=replace(union, read$m32 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2005$union <- fct_relevel(pgsw2005$union, "No", "Yes")
var_label(pgsw2005$union) <- "Union membership"

#Create codebook
sjPlot::view_df(pgsw2005, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2005.RData")