#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id('https://drive.google.com/file/d/1Y7kjTpQkKyN_E0Ogj2yUfJu9gF4Dk98K/view?usp=sharing'), overwrite=TRUE)
1
read <- read_spss('PGSW2019_CAPI.sav')
pgsw2019 <- tibble(1:2003)

colnames(pgsw2019) <- "n"
var_label(pgsw2019$n) <- "ID number"

pgsw2019$year <- 2019
pgsw2019$year <- as_factor(pgsw2019$year)
var_label(pgsw2019$year) <- "Year of election"

pgsw2019$weight <- read$waga
var_label(pgsw2019$weight) <- "Population weight"

pgsw2019$polint <- dplyr::recode_factor(read$Q01, 
                                         `1` = "Very interested", 
                                         `2` = "Rather interested", 
                                         `3` = "Rather uninterested", 
                                         `4` = "Totally uninterested",
                                          .ordered=TRUE)
var_label(pgsw2019$polint) <- "Level of interest in politics"

pgsw2019$polatten <- dplyr::recode_factor(read$Q02, 
                                        `1` = "Very attentively", 
                                        `2` = "Rather attentively", 
                                        `3` = "Not very attentively", 
                                        `4` = "Not at all",
                                        .ordered=TRUE)
var_label(pgsw2019$polatten) <- "How attentively do you follow political topics on TV, radio, newspapers or the internet?"

pgsw2019$inteff <- dplyr::recode_factor(read$Q03, 
                                        `5` = "Definitely disagree", 
                                        `4` = "Rather disagree", 
                                        `3` = "Neither agree nor disagree", 
                                        `2` = "Rather agree", 
                                        `1` = "Definitely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$inteff) <- "I have the impression that I understand the most important political questions in our country"

pgsw2019$comprom <- dplyr::recode_factor(read$Q04_1, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$comprom) <- "What people call compromise in politics is just selling out one's principles"

pgsw2019$dontcare <- dplyr::recode_factor(read$Q04_2, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$dontcare) <- "Most politicians do not care about the people"

pgsw2019$poltrust <- dplyr::recode_factor(read$Q04_3, 
                                          `5` = "Definitely agree", 
                                          `4` = "Rather agree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather disagree", 
                                          `1` = "Definitely disagree",
                                          .ordered=TRUE)
var_label(pgsw2019$poltrust) <- "Most politicians are worthy of trust"

pgsw2019$polprob <- dplyr::recode_factor(read$Q04_4, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$polprob) <- "Poland's biggest problem is its politicians"

pgsw2019$strgld <- dplyr::recode_factor(read$Q04_5, 
                                        `5` = "Definitely disagree", 
                                        `4` = "Rather disagree", 
                                        `3` = "Neither agree nor disagree", 
                                        `2` = "Rather agree", 
                                        `1` = "Definitely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$strgld) <- "Having a strong leader is important for Poland, even if he bends the rules to get things done"

pgsw2019$peopdec <- dplyr::recode_factor(read$Q04_6, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$peopdec) <- "People, not politicians, should take the most important political decisions"

pgsw2019$richinf <- dplyr::recode_factor(read$Q04_7, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$richinf) <- "Most politicians look out for the interests only of the rich and influential"

pgsw2019$minorit <- dplyr::recode_factor(read$Q05_1, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$minorit) <- "Minorities should adapt to dominant Polish customs and traditions"

pgsw2019$willmaj <- dplyr::recode_factor(read$Q05_2, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$willmaj) <- "The will of the majority should always take precedence, even at the cost of minority rights"

pgsw2019$imgposit <- dplyr::recode_factor(read$Q05_3, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgposit) <- "Immigrants usually have a positive impact on the Polish economy"

pgsw2019$imgcult <- dplyr::recode_factor(read$Q05_4, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgcult) <- "The presence of immigrants has a negative impact on Polish culture"

pgsw2019$imgcrime <- dplyr::recode_factor(read$Q05_5, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgcrime) <- "Immigrants are increasing the level of crime in Poland"

pgsw2019$polborn <- dplyr::recode_factor(read$Q06_1,
                                          `4` = "Totally unimportant", 
                                          `3` = "Not very important", 
                                          `2` = "Rather important", 
                                          `1` = "Very important",
                                          .ordered=TRUE)
var_label(pgsw2019$polborn) <- "To be a true Pole it is necessary to: be born in Poland"

pgsw2019$polance <- dplyr::recode_factor(read$Q06_2,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polance) <- "To be a true Pole it is necessary to: have Polish ancestors"

pgsw2019$polspeak <- dplyr::recode_factor(read$Q06_3,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polspeak) <- "To be a true Pole it is necessary to: speak Polish"

pgsw2019$polhabit <- dplyr::recode_factor(read$Q06_4,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polhabit) <- "To be a true Pole it is necessary to: observe Polish customs and traditions"

pgsw2019$polcath <- dplyr::recode_factor(read$Q06_5,
                                          `4` = "Totally unimportant", 
                                          `3` = "Not very important", 
                                          `2` = "Rather important", 
                                          `1` = "Very important",
                                          .ordered=TRUE)
var_label(pgsw2019$polcath) <- "To be a true Pole it is necessary to: be a Catholic"

pgsw2019$polinst <- dplyr::recode_factor(read$Q06_6,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polinst) <- "To be a true Pole it is necessary to: respect Polish laws and institutions"

HS.model <- ' truepole  =~ polborn + polance + polspeak + polhabit + polcath + polinst'

fit <- cfa(HS.model, data=pgsw2019)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2019[idx, fs] <- fscores[ , fs]
}
pgsw2019$truepole <- scales::rescale(pgsw2019$truepole, c(0,1))
var_label(pgsw2019$truepole) <- "Index of attitudes about true Polishness"

pgsw2019$corrup <- dplyr::recode_factor(read$Q07,
                                         `4` = "Almost never occurs", 
                                         `3` = "Not that common", 
                                         `2` = "Rather common", 
                                         `1` = "Very common",
                                         `8` = "Hard to say")
var_label(pgsw2019$corrup) <- "How common is corruption among Polish politicians?"

pgsw2019$govegal <- dplyr::recode_factor(read$Q08, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$govegal) <- "The government should take action to reduce difference in incomes"

pgsw2019$govopin <- dplyr::recode_factor(read$Q09, 
                                         `4` = "Very bad", 
                                         `3` = "Bad", 
                                         `2` = "Good", 
                                         `1` = "Very good",
                                         `8` = "Hard to say")
var_label(pgsw2019$govopin) <- "How do you evaluate the performance of the government over the last four years?"

pgsw2019$repres <- recode_factor(read$Q10a,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw2019$repres) <- "Is there a party that represents your views?"

pgsw2019$ecsitpl <- dplyr::recode_factor(read$Q11, 
                                         `5` = "Got much worse", 
                                         `4` = "Got worse", 
                                         `3` = "Not changed", 
                                         `2` = "Got better",
                                         `1` = "Got much better",
                                         .ordered=TRUE)
var_label(pgsw2019$ecsitpl) <- "In the last year the economic situation in Poland has..."

pgsw2019$voted <- recode_factor(read$Q12LHa,
                                `5` = "No",
                                `6` = "No",
                                `1` = "Yes")
var_label(pgsw2019$voted) <- "Voted in most recent election to the Sejm"

pgsw2019$votefor <- recode_factor(read$Q12LHb,
                                  `2` = "PiS",
                                  `5` = "KO",
                                  `3` = "Lewica",
                                  `1` = "PSL-Kukiz",
                                  `4` = "Konfederacja")
pgsw2019$votefor <- fct_expand(pgsw2019$votefor, "Did not vote")
pgsw2019$votefor[pgsw2019$voted=="No"] <- "Did not vote"
var_label(pgsw2019$votefor) <- "Party voted for in most recent election to the Sejm"

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
var_label(pgsw2019$votefor_t) <- "Type of party voted for in most recent election to the Sejm"

pgsw2019$votedsen <- recode_factor(read$Q12UHa,
                                   `5` = "No",
                                   `6` = "No",
                                   `1` = "Yes")
var_label(pgsw2019$votedsen) <- "Voted in most recent election to the Senate"

pgsw2019$votefor_s <- recode_factor(read$Q12UHb,
                                    `2` = "PiS",
                                    `5` = "KO",
                                    `3` = "Lewica",
                                    `1` = "PSL-Kukiz",
                                    `4` = "Konfederacja")
pgsw2019$votefor_s <- fct_expand(pgsw2019$votefor_s, "Did not vote")
pgsw2019$votefor_s[pgsw2019$votedsen=="No"] <- "Did not vote"
var_label(pgsw2019$votefor_s) <- "Party voted for in most recent parliamentary election"

pgsw2019$voted_pr <- recode_factor(read$Q13a,
                                   `5` = "No",
                                   `1` = "Yes",)
pgsw2019$voted_pr <- fct_drop(pgsw2019$voted_pr)
var_label(pgsw2019$voted_pr) <- "Voted in previous election to the Sejm"

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
var_label(pgsw2019$votefor_pr) <- "Party voted for in previous election to the Sejm"

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
var_label(pgsw2019$votefor_t_pr) <- "Type of party voted for in previous election to the Sejm"

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

pgsw2019$exteff1 <- dplyr::recode_factor(read$Q14a, 
                                         `1` = "Has no meaning", 
                                         `2` = "2", 
                                         `3` = "3", 
                                         `4` = "4",
                                         `5` = "Has a lot of meaning",
                                         .ordered=TRUE)
var_label(pgsw2019$exteff1) <- "Extent to which who governs has meaning"

pgsw2019$exteff2 <- dplyr::recode_factor(read$Q14b, 
                                         `1` = "Does not matter at all", 
                                         `2` = "2", 
                                         `3` = "3", 
                                         `4` = "4",
                                         `5` = "Matters a great deal",
                                         .ordered=TRUE)
var_label(pgsw2019$exteff2) <- "Extent to which for whom you vote matters"

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

pgsw2019$likeKacz <- dplyr::recode(read$Q16a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKacz <- add_labels(pgsw2019$likeKacz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKacz) <- "Feeling toward Jarosław Kaczyński"  

pgsw2019$likeSche <- dplyr::recode(read$Q16b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeSche <- add_labels(pgsw2019$likeSche, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeSche) <- "Feeling toward Grzegorz Schetyna" 

pgsw2019$likeKosz <- dplyr::recode(read$Q16c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKosz <- add_labels(pgsw2019$likeKosz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKosz) <- "Feeling toward Władysław Kosiniak-Kamysz" 

pgsw2019$likeCzar <- dplyr::recode(read$Q16d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeCzar <- add_labels(pgsw2019$likeCzar, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeCzar) <- "Feeling toward Włodzimierz Czarzasty" 

pgsw2019$likePKukiz <- dplyr::recode(read$Q16e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                     `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likePKukiz <- add_labels(pgsw2019$likePKukiz, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                                  '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likePKukiz) <- "Feeling toward Paweł Kukiz" 

pgsw2019$likeBied <- dplyr::recode(read$Q16f, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeBied <- add_labels(pgsw2019$likeBied, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeBied) <- "Feeling toward Robert Biedroń" 

pgsw2019$likeTusk <- dplyr::recode(read$Q16g, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeTusk <- add_labels(pgsw2019$likeTusk, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeTusk) <- "Feeling toward Donald Tusk" 

pgsw2019$likeMora <- dplyr::recode(read$Q16h, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeMora <- add_labels(pgsw2019$likeMora, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeMora) <- "Feeling toward Mateusz Morawiecki" 

pgsw2019$likeKida <- dplyr::recode(read$Q16i, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKida <- add_labels(pgsw2019$likeKida, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKida) <- "Feeling toward Małgorzata Kidawa-Błońska" 

pgsw2019$likeKorw <- dplyr::recode(read$Q16j, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                   `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$likeKorw <- add_labels(pgsw2019$likeKorw, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2019$likeKorw) <- "Feeling toward Janusz Korwin-Mikke" 

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

pgsw2019$leftrt <- dplyr::recode(read$Q18, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$leftrt <- add_labels(pgsw2019$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2019$leftrt) <- "Left-right self-placement"

pgsw2019$funcdem <- dplyr::recode_factor(read$Q21, 
                                         `5` = "Very discontented", 
                                         `4` = "Rather discontented", 
                                         `3` = "Neither contented nor discontented", 
                                         `2` = "Rather contented",
                                         `1` = "Very contented",
                                         .ordered=TRUE)
var_label(pgsw2019$funcdem) <- "Are you contented with the functioning of democracy in Poland?"

pgsw2019$ptclose <- dplyr::recode_factor(read$Q22a, 
                                         `5` = "No", 
                                         `1` = "Yes")
var_label(pgsw2019$ptclose) <- "Is there a party you regard yourself as close to?"

pgsw2019$ptcloser <- dplyr::recode_factor(read$Q22b, 
                                         `5` = "No", 
                                         `1` = "Yes")
var_label(pgsw2019$ptcloser) <- "Is there a party you regard yourself as closer to than others?"

pgsw2019 <- mutate(pgsw2019, pid = ptclose) %>%
  mutate(pid=replace(pid, pgsw2019$ptcloser=="Yes", "Yes"))
var_label(pgsw2019$pid) <- "Party identification"

pgsw2019$pidstr <- dplyr::recode_factor(read$Q22d, 
                                          `1` = "Very strongly", 
                                          `2` = "Rather strongly",
                                          `3` = "Not that strongly")
var_label(pgsw2019$pidstr) <- "Is there a party you regard yourself as closer to than others?"

pgsw2019$dembest <- dplyr::recode_factor(read$Q23, 
                                         `4` = "Strongly disagree", 
                                         `3` = "Rather disagree", 
                                         `2` = "Rather agree",
                                         `1` = "Strongly agree",
                                         .ordered=TRUE)
var_label(pgsw2019$dembest) <- "Democracy has its problems, but it is a better system of government than any of the alternatives"

pgsw2019$sphealth <- dplyr::recode_factor(read$N1_1,
                                         `5` = "Much lower than at present",
                                         `4` = "A bit lower than at present", 
                                         `3` = "The same as at present", 
                                         `2` = "A bit higher than at present",
                                         `1` = "Much higher than at present",
                                         .ordered=TRUE)
var_label(pgsw2019$sphealth) <- "Public spending on the health service should be..."

pgsw2019$speduc <- dplyr::recode_factor(read$N1_2,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                          .ordered=TRUE)
var_label(pgsw2019$speduc) <- "Public spending on education should be..."

pgsw2019$spunemp <- dplyr::recode_factor(read$N1_3,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                        .ordered=TRUE)
var_label(pgsw2019$spunemp) <- "Public spending on the unemployed should be..."

pgsw2019$sparmy <- dplyr::recode_factor(read$N1_4,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                        .ordered=TRUE)
var_label(pgsw2019$sparmy) <- "Public spending on the army should be..."

pgsw2019$sppensi <- dplyr::recode_factor(read$N1_5,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                        .ordered=TRUE)
var_label(pgsw2019$sppensi) <- "Public spending on pensions should be..."

pgsw2019$spbusin <- dplyr::recode_factor(read$N1_6,
                                         `5` = "Much lower than at present",
                                         `4` = "A bit lower than at present", 
                                         `3` = "The same as at present", 
                                         `2` = "A bit higher than at present",
                                         `1` = "Much higher than at present",
                                         .ordered=TRUE)
var_label(pgsw2019$spbusin) <- "Public spending on subsidies and support for business should be..."

pgsw2019$sppoljus <- dplyr::recode_factor(read$N1_7,
                                         `5` = "Much lower than at present",
                                         `4` = "A bit lower than at present", 
                                         `3` = "The same as at present", 
                                         `2` = "A bit higher than at present",
                                         `1` = "Much higher than at present",
                                         .ordered=TRUE)
var_label(pgsw2019$sppoljus) <- "Public spending on the police and justice system should be..."

pgsw2019$spsocsc <- dplyr::recode_factor(read$N1_8,
                                          `5` = "Much lower than at present",
                                          `4` = "A bit lower than at present", 
                                          `3` = "The same as at present", 
                                          `2` = "A bit higher than at present",
                                          `1` = "Much higher than at present",
                                          .ordered=TRUE)
var_label(pgsw2019$spsocsc) <- "Public spending on social security should be..."

HS.model <- ' pubspend  =~ sphealth + speduc + spunemp + sparmy + sppensi + spbusin + sppoljus + spsocsc'

fit <- cfa(HS.model, data=pgsw2019)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2019[idx, fs] <- fscores[ , fs]
}
pgsw2019$pubspend <- scales::rescale(pgsw2019$pubspend, c(0,1))
var_label(pgsw2019$pubspend) <- "Index of attitudes to public spending"

pgsw2019$govinfl <- dplyr::recode_factor(read$N2,
                                         `4` = "Not at all", 
                                         `3` = "To a small degree", 
                                         `2` = "To a significant degree",
                                         `1` = "To a very large degree",
                                         .ordered=TRUE)
var_label(pgsw2019$govinfl) <- "To what extent is the financial situation of your family dependent on the recent actions of the government?"

pgsw2019$plecpst <- dplyr::recode_factor(read$N3a,
                                         `5` = "Considerably worsened",
                                         `4` = "Worsened", 
                                         `3` = "Did not change", 
                                         `2` = "Improved",
                                         `1` = "Considerably improved",
                                         .ordered=TRUE)
var_label(pgsw2019$plecpst) <- "Over the last year the economic situation in Poland..."

pgsw2019$famecpst <- dplyr::recode_factor(read$N3b,
                                         `5` = "Considerably worsened",
                                         `4` = "Worsened", 
                                         `3` = "Did not change", 
                                         `2` = "Improved",
                                         `1` = "Considerably improved",
                                         .ordered=TRUE)
var_label(pgsw2019$famecpst) <- "Over the last year your family's material situation..."

pgsw2019$pleccur <- dplyr::recode_factor(read$N4a,
                                         `5` = "Very bad",
                                         `4` = "Bad", 
                                         `3` = "Neither good nor bad", 
                                         `2` = "Good",
                                         `1` = "Very good",
                                         .ordered=TRUE)
var_label(pgsw2019$pleccur) <- "The economic situation in Poland is..."

pgsw2019$fameccur <- dplyr::recode_factor(read$N4b,
                                          `5` = "Very bad",
                                          `4` = "Bad", 
                                          `3` = "Neither good nor bad", 
                                          `2` = "Good",
                                          `1` = "Very good",
                                          .ordered=TRUE)
var_label(pgsw2019$fameccur) <- "Your family's current material situation is..."

pgsw2019$plecfut <- dplyr::recode_factor(read$N5a,
                                         `5` = "Get much worse",
                                         `4` = "Get worse", 
                                         `3` = "Remain the same", 
                                         `2` = "Get better",
                                         `1` = "Get much better",
                                         .ordered=TRUE)
var_label(pgsw2019$plecfut) <- "In the next year, the economic situation in Poland will..."

pgsw2019$famecfut <- dplyr::recode_factor(read$N5b,
                                          `5` = "Get much worse",
                                          `4` = "Get worse", 
                                          `3` = "Remain the same", 
                                          `2` = "Get better",
                                          `1` = "Get much better",
                                          .ordered=TRUE)
var_label(pgsw2019$famecfut) <- "In the next year, your family's current material situation will..."

pgsw2019$confrich <- dplyr::recode_factor(read$N8_1,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confrich) <- "Is there a conflict between rich and poor people in Poland?"

pgsw2019$confage <- dplyr::recode_factor(read$N8_2,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confage) <- "Is there a conflict between young and old people in Poland?"

pgsw2019$confrel <- dplyr::recode_factor(read$N8_3,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confrel) <- "Is there a conflict between believers and non-believers in Poland?"

pgsw2019$confsex <- dplyr::recode_factor(read$N8_4,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confsex) <- "Is there a conflict between men and women in Poland?"

pgsw2019$confwork <- dplyr::recode_factor(read$N8_5,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confwork) <- "Is there a conflict between workers and employers in Poland?"

pgsw2019$confpupr <- dplyr::recode_factor(read$N8_6,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confpupr) <- "Is there a conflict between public and private sector workers in Poland?"

pgsw2019$confeduc <- dplyr::recode_factor(read$N8_7,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confeduc) <- "Is there a conflict between the poorly educated and the well educated in Poland?"

pgsw2019$confcls <- dplyr::recode_factor(read$N8_8,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confcls) <- "Is there a conflict between the working class and the middle class in Poland?"

pgsw2019$conffrm <- dplyr::recode_factor(read$N8_9,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$conffrm) <- "Is there a conflict between farmers and urbanites in Poland?"

pgsw2019$confpatr <- dplyr::recode_factor(read$N8_10,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confpatr) <- "Is there a conflict between Polish patriots and those who do not respect Poland?"

HS.model <- ' conflict  =~ confrich + confage + confpupr + confeduc + confcls + conffrm + confpatr + confrel + confsex + confwork '

fit <- cfa(HS.model, data=pgsw2019)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2019[idx, fs] <- fscores[ , fs]
}
pgsw2019$conflict <- scales::rescale(pgsw2019$conflict, c(0,1))
var_label(pgsw2019$conflict) <- "Index of conflict perception"





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

pgsw2019$neword <- dplyr::recode_factor(read$P19_1,
                                          `5` = "Totally disagree", 
                                          `4` = "Disagree",
                                          `3` = "Neither agree nor disagree",
                                          `2` = "Agree",
                                          `1` = "Totally agree",
                                          .ordered=TRUE)
var_label(pgsw2019$neword) <- "Poland needs someone who will have the strength to change our system completely and introduce a new, just order."

pgsw2019$ordpeop <- dplyr::recode_factor(read$P19_2,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$ordpeop) <- "In Poland a few people arrogate to themselves the power that should belong to the ordinary people."

pgsw2019$crisis <- dplyr::recode_factor(read$P19_3,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$crisis) <- "It is the last moment to save Poland in the face of a threatening crisis."

pgsw2019$morals <- dplyr::recode_factor(read$P19_4,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$morals) <- "Nothing in Poland will improve until politicians return to good old-fashioned moral values."

pgsw2019$simple <- dplyr::recode_factor(read$P19_5,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$simple) <- "To solve our country's problems is very straightforward; we just need to give power to those who want to do this."

pgsw2019$goodev <- dplyr::recode_factor(read$P19_6,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$goodev) <- "Everything in politics is either good or evil; the choice is simple."

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

pgsw2019$theocr <- read$H14f
pgsw2019$theocr <- set_labels(pgsw2019$theocr, labels = c("...Biblical values and the instructions of the clergy" = 1, "...laws set by citizens and enshrined in the Constitution" = 7))
var_label(pgsw2019$theocr) <- "Our political and social life should be directed by..."


#####Socio-demographic variables#####
pgsw2019 <- mutate(pgsw2019, gender = factor(case_when(read$D02==1 ~ "Male",
                                                       read$D02==2 ~ "Female")))
pgsw2019$gender <- fct_relevel(pgsw2019$gender, "Male", "Female")
var_label(pgsw2019$gender) <- "Gender"

pgsw2019 <- mutate(pgsw2019, region = factor(case_when(read$wlk=="wieś" ~ "Village",
                                                       read$wlk=="miasto do 10tys" ~ "Town < 19999",
                                                       read$wlk=="miasto 10-20tys" ~ "Town < 19999",
                                                       read$wlk=="miasto 20-50tys" ~ "Town 20000 - 49999",
                                                       read$wlk=="miasto 50-100tys" ~ "Town 50000-99999",
                                                       read$wlk=="miasto 100-200tys" ~ "Town 100000 - 499999",
                                                       read$wlk=="miasto 200-500tys" ~ "Town 100000 - 499999",
                                                       read$wlk=="miasto 500tys+" ~ "Town > 500000")))
pgsw2019$region <- fct_relevel(pgsw2019$region, "Village", "Town < 19999", "Town 20000 - 49999", 
                               "Town 50000-99999", "Town 100000 - 499999", "Town > 500000")
var_label(pgsw2019$region) <- "Size of region in which respondent lives"

pgsw2019 <- mutate(pgsw2019, age = 2019-read$D01b)
pgsw2019$age<- remove_all_labels(pgsw2019$age)
var_label(pgsw2019$age) <- "Age"

pgsw2019 <- mutate(pgsw2019, edlevel = factor(case_when(read$D03==1 | read$D03==2  ~ "Basic or none",
                                                        read$D03==3 | read$D03==4 ~ "Lower secondary",
                                                        read$D03==5 | read$D03==6 ~ "Upper secondary",
                                                        read$D03==7 | read$D03==8 | read$D03==9  ~ "Higher")))
pgsw2019$edlevel <- fct_relevel(pgsw2019$edlevel, "Basic or none", "Lower secondary", "Upper secondary", "Higher")
var_label(pgsw2019$edlevel) <- "Level of education"

pgsw2019 <- mutate(pgsw2019, relig = factor(case_when(read$D11==1 ~ "Never",
                                                      read$D11==2 | read$D11==3  ~ "Seldom",
                                                      read$D11==4 | read$D11==5  ~ "Often",
                                                      read$D11==6 ~ "At least weekly")))
pgsw2019$relig <- fct_relevel(pgsw2019$relig, "Never", "Seldom", "Often", "At least weekly")
var_label(pgsw2019$relig) <- "Religious attendance"

pgsw2019<- mutate(pgsw2019, income = case_when(read$D09a==1 | read$D09>=0 & read$D09<=300 ~ 1,
                                                read$D09a==2 | read$D09>=301 & read$D09<=500 ~ 2,
                                                read$D09a==3 | read$D09>=501 & read$D09<=750 ~ 3,
                                                read$D09a==4 | read$D09>=751 & read$D09<=1000 ~ 4,
                                                read$D09a==5 | read$D09>=1001 & read$D09<=1250 ~ 5,
                                                read$D09a==6 | read$D09>=1251 & read$D09<=1500 ~ 6,
                                                read$D09a==7 | read$D09>=1501 & read$D09<=1750 ~ 7,
                                                read$D09a==8 | read$D09>=1751 & read$D09<=2000 ~ 8,
                                                read$D09a==9 | read$D09>=2001 & read$D09<=2250 ~ 9,
                                                read$D09a==10 | read$D09>=2251 & read$D09<=2500 ~ 10,
                                                read$D09a==11 | read$D09>=2501 & read$D09<=2750 ~ 11,
                                                read$D09a==12 | read$D09>=2751 & read$D09<=3000 ~ 12,
                                                read$D09a==13 | read$D09>=3001 & read$D09<=3500 ~ 13,
                                                read$D09a==14 | read$D09>=3501 & read$D09<=4000 ~ 14,
                                                read$D09a==15 | read$D09>=4001 & read$D09<=4500 ~ 15,
                                                read$D09a==16 | read$D09>=4501 & read$D09<=5000 ~ 16,
                                                read$D09a==17 | read$D09>=5001 & read$D09<=6000 ~ 17,
                                                read$D09a==18 | read$D09>=6001 & read$D09<=8000 ~ 18,
                                                read$D09a==19 | read$D09>=8001 & read$D09<=10000 ~ 19,
                                                read$D09a==20 | read$D09>=10000 & read$D09<=40000 ~ 20))
var_label(pgsw2019$income) <- "Income"

pgsw2019$hincq <- ntile(pgsw2019$income, 5)
var_label(pgsw2019$hincq) <- "Household income (quintile)"

pgsw2019 <- mutate(pgsw2019, occup = NA) %>%
  mutate(occup=replace(occup, read$D07 %in% c(10:39), "Managers and professionals")) %>%
  mutate(occup=replace(occup, read$D07 %in% c(40:96), "Blue collar and clerical")) %>%
  mutate(occup=replace(occup, read$D07 %in% c(22, 23, 26, 32, 34), "Sociocultural professionals")) %>%
  mutate(occup=replace(occup, read$D07==99, "Outside the labour market")) %>%
  as_factor(occup)
var_label(pgsw2019$occup) <- "Occupation"

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

#Create codebook
sjPlot::view_df(pgsw2019, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2019.RData")
write_stata(pgsw2019, path='/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW2019/PGSW_2019.dta', version=14)

