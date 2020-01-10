#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id("https://drive.google.com/file/d/1Ua1tDfTlAXsXijPW8L2SYBHwlgRN4hcq/view?usp=sharing"), overwrite=TRUE)
read <- read_spss('PGSW 2015.sav')
pgsw2015 <- tibble(1:1733)

#####General variables#####
colnames(pgsw2015) <- "n"
var_label(pgsw2015$n) <- "ID number"

pgsw2015$year <- 2015
pgsw2015$year <- as_factor(pgsw2015$year)
var_label(pgsw2015$year) <- "Year of election"

pgsw2015$weight <- read$WAGA
var_label(pgsw2015$weight) <- "Weight"


#####Voting behaviour#####
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
pgsw2015$likePiS <- add_labels(pgsw2015$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likePiS) <- "Feeling toward PiS"  

pgsw2015$likePO <- dplyr::recode(read$q45, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePO <- add_labels(pgsw2015$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likePO) <- "Feeling toward PO" 

pgsw2015$likePSL <- dplyr::recode(read$q48, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likePSL <- add_labels(pgsw2015$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likePSL) <- "Feeling toward PSL" 

pgsw2015$likeZL <- dplyr::recode(read$q51, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeZL <- add_labels(pgsw2015$likeZL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeZL) <- "Feeling toward ZL" 

pgsw2015$likeKukiz <- dplyr::recode(read$q54, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeKukiz <- add_labels(pgsw2015$likeKukiz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeKukiz) <- "Feeling toward Kukiz'15" 

pgsw2015$likeNowo<- dplyr::recode(read$q57, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeNowo <- add_labels(pgsw2015$likeNowo, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeNowo) <- "Feeling toward Nowoczesna" 

pgsw2015$likeKorwin<- dplyr::recode(read$q60, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeKorwin <- add_labels(pgsw2015$likeKorwin, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                  '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeKorwin) <- "Feeling toward KORWiN" 

pgsw2015$likeRazem<- dplyr::recode(read$q63, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$likeRazem <- add_labels(pgsw2015$likeRazem, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2015$likeRazem) <- "Feeling toward Razem" 

pgsw2015$lrPiS <- dplyr::recode(read$q72, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPiS <- add_labels(pgsw2015$lrPiS, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrPiS) <- "Left-right placement of PiS"  

pgsw2015$lrPO <- dplyr::recode(read$q75, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPO <- add_labels(pgsw2015$lrPO, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                      '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrPO) <- "Left-right placement of PO"  

pgsw2015$lrRazem <- dplyr::recode(read$q78, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrRazem <- add_labels(pgsw2015$lrRazem, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrRazem) <- "Left-right placement of Razem"  

pgsw2015$lrZL <- dplyr::recode(read$q87, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrZL <- add_labels(pgsw2015$lrZL, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                      '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrZL) <- "Left-right placement of United Left" 

pgsw2015$lrKukiz <- dplyr::recode(read$q90, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrKukiz <- add_labels(pgsw2015$lrKukiz, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrKukiz) <- "Left-right placement of Kukiz'15" 

pgsw2015$lrNowo <- dplyr::recode(read$q93, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrNowo <- add_labels(pgsw2015$lrNowo, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrNowo) <- "Left-right placement of Nowoczesna" 

pgsw2015$lrKorwin <- dplyr::recode(read$q81, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrKorwin <- add_labels(pgsw2015$lrKorwin, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrKorwin) <- "Left-right placement of KORWiN" 

pgsw2015$lrPSL <- dplyr::recode(read$q84, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$lrPSL <- add_labels(pgsw2015$lrPSL, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$lrPSL) <- "Left-right placement of PSL" 


#####2015: Values and attitudes#####
pgsw2015$leftrt <- dplyr::recode(read$q96, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$leftrt <- add_labels(pgsw2015$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2015$leftrt) <- "Left-right self-placement"  

pgsw2015$sollib <- dplyr::recode(read$q102, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$sollib <- add_labels(pgsw2015$sollib, labels = c("Solidaristic" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Liberal" = 10))
var_label(pgsw2015$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2015$euinteg <- dplyr::recode(read$q99, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2015$euinteg <- add_labels(pgsw2015$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2015$euinteg) <- "European integration"   

pgsw2015$chstdiv <- dplyr::recode(read$L1q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$chstdiv <- add_labels(pgsw2015$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "The Church should be completely separate from the state" = 7))
var_label(pgsw2015$chstdiv) <- "Church/state divide"                                                           

pgsw2015$taxreg <- dplyr::recode(read$L2q144, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2015$taxreg <- add_labels(pgsw2015$taxreg, labels = c("Progressive tax regime" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "Flat tax regime" = 7))
var_label(pgsw2015$taxreg) <- "Tax regime"                                                           

pgsw2015$forpol <- dplyr::recode(read$L3q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$forpol <- add_labels(pgsw2015$forpol, labels = c("Foreign policy should be based on political and economic independence" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "Foreign policy should be based on close cooperation with the EU" = 7))
var_label(pgsw2015$forpol) <- "Foreign policy"  

pgsw2015$immigr <- dplyr::recode(read$L4q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$immigr <- add_labels(pgsw2015$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "The state should encourage people from other countries to immigrate to Poland" = 7))
var_label(pgsw2015$immigr) <- "Immigration"  

pgsw2015$socpol <- dplyr::recode(read$L5q144, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2015$socpol <- add_labels(pgsw2015$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "People should take care of their own welfare" = 7))
var_label(pgsw2015$socpol) <- "Social policy"  

pgsw2015$private <- dplyr::recode(read$L6q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2015$private <- add_labels(pgsw2015$private, labels = c("A significant number of enterprises should remain in state hands" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "All state-owned enterprises should be privatised" = 7))
var_label(pgsw2015$private) <- "Privatisation"  

pgsw2015$abort <- dplyr::recode(read$L7q144, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
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
  mutate(union=replace(union, read$q333==5, "No")) %>%
  mutate(union=replace(union, read$q333 %in% c(1:4), "Yes")) %>%
  as_factor(union)
var_label(pgsw2015$union) <- "Union membership"

#Create codebook
sjPlot::view_df(pgsw2015, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2015.RData")