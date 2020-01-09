#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id('https://drive.google.com/open?id=1jJwiIn-6z2DmdN-N2DX72wYi_nScbqzx'), overwrite=TRUE)
read <- read_spss('PGSW2019_CAPI.sav')
pgsw2019 <- tibble(1:2003)

#General variables
colnames(pgsw2019) <- "n"
var_label(pgsw2019$n) <- "ID number"

pgsw2019$year <- 2019
pgsw2019$year <- as_factor(pgsw2019$year)
var_label(pgsw2019$year) <- "Year of election"

pgsw2019$weight <- read$waga
var_label(pgsw2019$weight) <- "Population weight"

#Voting behaviour
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

pgsw2019$votefor_t <- recode_factor(read$Q12LHb,
                                    `2` = "Right",
                                    `5` = "Liberal",
                                    `3` = "Left",
                                    `1` = "Other",
                                    `4` = "Right")
pgsw2019$votefor_t <- fct_drop(pgsw2019$votefor_t)
pgsw2019$votefor_t <- fct_expand(pgsw2019$votefor_t, "Did not vote")
pgsw2019$votefor_t[pgsw2019$voted=="No"] <- "Did not vote"
pgsw2019$votefor_t <- fct_relevel(pgsw2019$votefor_t, "Right", "Left", "Liberal", "Other", "Did not vote")
var_label(pgsw2019$votefor_t) <- "Type of party voted for in most recent parliamentary election"

pgsw2019$voted_pr <- recode_factor(read$Q13a,
                                   `5` = "No",
                                   `1` = "Yes",)
pgsw2019$voted_pr <- fct_drop(pgsw2019$voted_pr)
var_label(pgsw2019$voted_pr) <- "Voted in previous parliamentary election"

pgsw2019$votefor_pr <- recode_factor(read$Q13b,
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
var_label(pgsw2019$votefor_pr) <- "Party voted for in previous parliamentary election"

pgsw2019$votefor_t_pr <- recode_factor(read$Q13b,
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
var_label(pgsw2019$votefor_t_pr) <- "Type of party voted for in previous parliamentary election"

pgsw2019 <- mutate(pgsw2019, stability = if_else(pgsw2019$votefor_t=="Right" & pgsw2019$votefor_t_pr=="Right",  "Stable",
                                                 if_else(pgsw2019$votefor_t=="Left" & pgsw2019$votefor_t_pr=="Left", "Stable",
                                                         if_else(pgsw2019$votefor_t=="Liberal" & pgsw2019$votefor_t_pr=="Liberal", "Stable",
                                                                 if_else(pgsw2019$votefor_t=="Other" & pgsw2019$votefor_t_pr=="Other", "Stable",
                                                                         if_else(pgsw2019$votefor_t=="Did not vote" & pgsw2019$votefor_t_pr=="Did not vote", "Stable", "Unstable"))))))
pgsw2019$stability <- fct_relevel(pgsw2019$stability, "Unstable", "Stable")
var_label(pgsw2019$stability) <- "Stability of voting behaviour (type)"

pgsw2019 <- mutate(pgsw2019, partyid = factor(case_when(read$Q10a==1 ~ "Yes",
                                                        read$Q10a==2 ~ "No",
                                                        read$Q10a==7 ~ "Don't know")))
pgsw2019$partyid <- fct_relevel(pgsw2019$partyid, "Yes", "No", "Don't know")
var_label(pgsw2019$partyid) <- "Party identification"

pgsw2019 <- mutate(pgsw2019, vtpres_2 = factor(if_else(read$P18==3, "No", "Yes"))) %>%
  mutate(vtpres_2=replace(vtpres_2, read$P18==4, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read$P18==5, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read$P18==97, NA)) %>%
  mutate(vtpres_2=replace(vtpres_2, read$P18==98, NA))
var_label(pgsw2019$vtpres_2) <- "Voted in second round of most recent presidential election"

pgsw2019 <- mutate(pgsw2019, vtforpres_2 = factor(case_when(read$P18==1 ~ "Komorowski",
                                                            read$P18==2 ~ "Duda",
                                                            read$P18==3 ~ "Did not vote")))
var_label(pgsw2019$vtforpres_2) <- "Candidate voted for in second round of most recent presidential election"

pgsw2019$likePiS <- dplyr::recode(read$Q15a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePiS <- add_labels(pgsw2019$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePiS) <- "Feeling toward PiS"  

pgsw2019$likePO <- dplyr::recode(read$Q15b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePO <- add_labels(pgsw2019$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePO) <- "Feeling toward PO" 

pgsw2019$likePSL <- dplyr::recode(read$Q15c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePSL <- add_labels(pgsw2019$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePSL) <- "Feeling toward PSL" 

pgsw2019$likeSLD <- dplyr::recode(read$Q15d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeSLD <- add_labels(pgsw2019$likeSLD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeSLD) <- "Feeling toward SLD" 

pgsw2019$likeKukiz <- dplyr::recode(read$Q15e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKukiz <- add_labels(pgsw2019$likeKukiz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKukiz) <- "Feeling toward Kukiz'15" 

pgsw2019$likeWiosna <- dplyr::recode(read$Q15f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                    `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeWiosna <- add_labels(pgsw2019$likeWiosna, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeWiosna) <- "Feeling toward Wiosna" 

pgsw2019$likeKonf<- dplyr::recode(read$Q15g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKonf <- add_labels(pgsw2019$likeKonf, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKonf) <- "Feeling toward Konfederacja" 

pgsw2019$lrPiS <- dplyr::recode(read$Q17a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrPiS <- add_labels(pgsw2019$lrPiS, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrPiS) <- "Left-right placement of PiS"  

pgsw2019$lrPO <- dplyr::recode(read$Q17b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                               `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrPO <- add_labels(pgsw2019$lrPO, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                      '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrPO) <- "Left-right placement of PO"  

pgsw2019$lrPSL <- dplyr::recode(read$Q17c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrPSL <- add_labels(pgsw2019$lrPSL, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrPSL) <- "Left-right placement of PSL" 

pgsw2019$lrSLD <- dplyr::recode(read$Q17d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrSLD <- add_labels(pgsw2019$lrSLD, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrSLD) <- "Left-right placement of SLD"  

pgsw2019$lrKukiz <- dplyr::recode(read$Q17e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrKukiz <- add_labels(pgsw2019$lrKukiz, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrKukiz) <- "Left-right placement of Kukiz'15" 

pgsw2019$lrWiosna <- dplyr::recode(read$Q17f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrWiosna <- add_labels(pgsw2019$lrWiosna, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrWiosna) <- "Left-right placement of Nowoczesna" 

pgsw2019$lrKonf <- dplyr::recode(read$Q17g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$lrKonf <- add_labels(pgsw2019$lrKonf, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$lrKonf) <- "Left-right placement of KORWiN" 


#####Values and attitudes#####
pgsw2019$leftrt <- dplyr::recode(read$Q18, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$leftrt <- add_labels(pgsw2019$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$leftrt) <- "Left-right self-placement"  

pgsw2019$sollib <- dplyr::recode(read$P11, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$sollib <- add_labels(pgsw2019$sollib, labels = c("Solidaristic" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Liberal" = 10))
var_label(pgsw2019$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2019$euinteg <- dplyr::recode(read$P10, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$euinteg <- add_labels(pgsw2019$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2019$euinteg) <- "European integration"  

pgsw2019$climate <- dplyr::recode(read$I1a, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$climate <- add_labels(pgsw2019$climate, labels = c("Climate change and environmental degradation is not an important problem" = 1, 
                                                            '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                            "Climate change and environmental degradation is the most important problem facing Poland" = 7))
var_label(pgsw2019$climate) <- "Climate change"  

pgsw2019$taxreg <- dplyr::recode(read$I1b, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2019$taxreg <- add_labels(pgsw2019$taxreg, labels = c("Progressive tax regime" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "Flat tax regime" = 7))
var_label(pgsw2019$taxreg) <- "Tax regime"     

pgsw2019$forpol <- dplyr::recode(read$I1c, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$forpol <- add_labels(pgsw2019$forpol, labels = c("Foreign policy should be based on political and economic independence" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "Foreign policy should be based on close cooperation with the EU" = 7))
var_label(pgsw2019$forpol) <- "Foreign policy"  

pgsw2019$immigr <- dplyr::recode(read$I1d, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$immigr <- add_labels(pgsw2019$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "The state should encourage people from other countries to immigrate to Poland" = 7))
var_label(pgsw2019$immigr) <- "Immigration"  

pgsw2019$socpol <- dplyr::recode(read$I1e, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2019$socpol <- add_labels(pgsw2019$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                          "People should take care of their own welfare" = 7))
var_label(pgsw2019$socpol) <- "Social policy"  

pgsw2019$lgbt <- dplyr::recode(read$I1f, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$lgbt <- add_labels(pgsw2019$lgbt, labels = c("Same-sex couples should not have the same rights as heterosexuals to publicly display their lifestyle" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                            "Same-sex couples should have the same rights as heterosexuals to publicly display their lifestyle" = 7))
var_label(pgsw2019$lgbt) <- "LGBT rights"  

pgsw2019$abort <- dplyr::recode(read$I1g, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2019$abort <- add_labels(pgsw2019$abort, labels = c("There should be no right to abortion" = 1,
                                                        '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 7))
var_label(pgsw2019$abort) <- "Abortion"  

pgsw2019$strgman <- as.factor(dplyr::recode(read$P19_1, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2019$strgman <- add_labels(pgsw2019$strgman, labels = c("Definitely no" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Definitely yes" = 5))
var_label(pgsw2019$strgman) <- "Necessity of a strong leader"  

pgsw2019$elitpow <- as.factor(dplyr::recode(read$P19_2, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2019$elitpow <- add_labels(pgsw2019$elitpow, labels = c("Definitely no" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Definitely yes" = 5))
var_label(pgsw2019$elitpow) <- "Political elites arrogate power to themselves"  

pgsw2019$crisis <- as.factor(dplyr::recode(read$P19_3, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2019$crisis <- add_labels(pgsw2019$crisis, labels = c("Definitely no" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4,
                                                          "Definitely yes" = 5))
var_label(pgsw2019$crisis) <- "Last chance to rescue Poland from crisis"  

pgsw2019$easyref <- as.factor(dplyr::recode(read$S1_4, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2019$easyref <- add_labels(pgsw2019$easyref, labels = c("Definitely no" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Definitely yes" = 5))
var_label(pgsw2019$easyref) <- "Solving the problems before us is very easy"  

pgsw2019$goodevil <- as.factor(dplyr::recode(read$P19_6, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2019$goodevil <- add_labels(pgsw2019$goodevil, labels = c("Definitely no" = 1,
                                                              '2' = 2, '3' = 3, '4' = 4,
                                                              "Definitely yes" = 5))
var_label(pgsw2019$goodevil) <- "In politics everything is either good or evil"  

pgsw2019$unkelit <- as.factor(dplyr::recode(read$H10_1, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$unkelit <- add_labels(pgsw2019$unkelit, labels = c("Definitely no" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Definitely yes" = 5))
var_label(pgsw2019$unkelit) <- "It is not the government which rules, but unknown elites"  

pgsw2019$conspir <- as.factor(dplyr::recode(read$H10_2, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$conspir <- add_labels(pgsw2019$conspir, labels = c("Definitely no" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Definitely yes" = 5))
var_label(pgsw2019$conspir) <- "Those who see conspiracies against us are in many senses right"  


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


#Create codebook
sjPlot::view_df(pgsw2019, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2019.RData")
