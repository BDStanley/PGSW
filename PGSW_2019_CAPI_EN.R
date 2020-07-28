pgsw2015$strgman <- as.factor(dplyr::recode(read_2015$L1q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$strgman <- add_labels(pgsw2015$strgman, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$strgman) <- "Necessity of a strong leader"  

pgsw2015$elitpow <- as.factor(dplyr::recode(read_2015$L2q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$elitpow <- add_labels(pgsw2015$elitpow, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$elitpow) <- "Political elites arrogate power to themselves"  

pgsw2015$crisis <- as.factor(dplyr::recode(read_2015$L3q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$crisis <- add_labels(pgsw2015$crisis, labels = c("Definitely no" = 1,
                                                          "Definitely yes" = 5))
var_label(pgsw2015$crisis) <- "Last chance to rescue Poland from crisis"  

pgsw2015$easyref <- as.factor(dplyr::recode(read_2015$L4q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$easyref <- add_labels(pgsw2015$easyref, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$easyref) <- "Solving the problems before us is very easy"  

pgsw2015$goodevil <- as.factor(dplyr::recode(read_2015$L5q120, `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L))
pgsw2015$goodevil <- add_labels(pgsw2015$goodevil, labels = c("Definitely no" = 1,
                                                              "Definitely yes" = 5))
var_label(pgsw2015$goodevil) <- "In politics everything is either good or evil"  

pgsw2015$unkelit <- as.factor(dplyr::recode(read_2015$L1q290, `1` = 5L, `2` = 4L, `5` = 3L, `3` = 2L, `4` = 1L))
pgsw2015$unkelit <- add_labels(pgsw2015$unkelit, labels = c("Definitely no" = 1,
                                                            "Definitely yes" = 5))
var_label(pgsw2015$unkelit) <- "It is not the government which rules, but unknown elites"  

pgsw2015$conspir <- as.factor(dplyr::recode(read_2015$L2q290, `1` = 5L, `2` = 4L, `5` = 3L, `3` = 2L, `4` = 1L))
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

pgsw2015$polpast <- dplyr::recode_factor(read_2015$L1q21, 
                                         `5` = "Got much worse", 
                                         `4` = "Got somewhat worse", 
                                         `3` = "Did not change", 
                                         `2` = "Got somewhat better", 
                                         `1` = "Got much better",
                                         .ordered=TRUE)
var_label(pgsw2015$polpast) <- "Attitudes to recent political situation in Poland" 

pgsw2015$ecpast <- dplyr::recode_factor(read_2015$L2q21, 
                                        `5` = "Got much worse", 
                                        `4` = "Got somewhat worse", 
                                        `3` = "Did not change", 
                                        `2` = "Got somewhat better", 
                                        `1` = "Got much better",
                                        .ordered=TRUE)
var_label(pgsw2015$ecpast) <- "Attitudes to recent economic situation in Poland" 

pgsw2015$hecpast <- dplyr::recode_factor(read_2015$L3q21, 
                                         `5` = "Got much worse", 
                                         `4` = "Got somewhat worse", 
                                         `3` = "Did not change", 
                                         `2` = "Got somewhat better", 
                                         `1` = "Got much better",
                                         .ordered=TRUE)
var_label(pgsw2015$hecpast) <- "Attitudes to recent household economic situation" 

pgsw2015$polcurr <- dplyr::recode_factor(read_2015$L1q23, 
                                         `5` = "Very bad", 
                                         `4` = "Bad", 
                                         `3` = "Neither good nor bad", 
                                         `2` = "Good", 
                                         `1` = "Very good",
                                         .ordered=TRUE)
var_label(pgsw2015$polcurr) <- "Attitudes to current political situation in Poland" 

pgsw2015$eccurr <- dplyr::recode_factor(read_2015$L2q23, 
                                        `5` = "Very bad", 
                                        `4` = "Bad", 
                                        `3` = "Neither good nor bad", 
                                        `2` = "Good", 
                                        `1` = "Very good",
                                        .ordered=TRUE)
var_label(pgsw2015$eccurr) <- "Attitudes to current economic situation in Poland" 

pgsw2015$heccurr <- dplyr::recode_factor(read_2015$L3q23, 
                                         `5` = "Very bad", 
                                         `4` = "Bad", 
                                         `3` = "Neither good nor bad", 
                                         `2` = "Good", 
                                         `1` = "Very good",
                                         .ordered=TRUE)
var_label(pgsw2015$heccurr) <- "Attitudes to current household economic situation" 

pgsw2015$polfut <- dplyr::recode_factor(read_2015$L1q25, 
                                        `5` = "Will get much worse", 
                                        `4` = "Will get somewhat worse", 
                                        `3` = "Will not change", 
                                        `2` = "Will get somewhat better", 
                                        `1` = "Will get much better",
                                        .ordered=TRUE)
var_label(pgsw2015$polfut) <- "Attitudes to future political situation in Poland" 

pgsw2015$ecfut <- dplyr::recode_factor(read_2015$L2q25, 
                                       `5` = "Will get much worse", 
                                       `4` = "Will get somewhat worse", 
                                       `3` = "Will not change", 
                                       `2` = "Will get somewhat better", 
                                       `1` = "Will get much better",
                                       .ordered=TRUE)
var_label(pgsw2015$ecfut) <- "Attitudes to future economic situation in Poland" 

pgsw2015$hecfut <- dplyr::recode_factor(read_2015$L3q25, 
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


pgsw2019$polint <- dplyr::recode_factor(read_2019$Q01, 
                                         `1` = "Very interested", 
                                         `2` = "Rather interested", 
                                         `3` = "Rather uninterested", 
                                         `4` = "Totally uninterested",
                                          .ordered=TRUE)
var_label(pgsw2019$polint) <- "Level of interest in politics"

pgsw2019$polatten <- dplyr::recode_factor(read_2019$Q02, 
                                        `1` = "Very attentively", 
                                        `2` = "Rather attentively", 
                                        `3` = "Not very attentively", 
                                        `4` = "Not at all",
                                        .ordered=TRUE)
var_label(pgsw2019$polatten) <- "How attentively do you follow political topics on TV, radio, newspapers or the internet?"

pgsw2019$inteff <- dplyr::recode_factor(read_2019$Q03, 
                                        `5` = "Definitely disagree", 
                                        `4` = "Rather disagree", 
                                        `3` = "Neither agree nor disagree", 
                                        `2` = "Rather agree", 
                                        `1` = "Definitely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$inteff) <- "I have the impression that I understand the most important political questions in our country"

pgsw2019$comprom <- dplyr::recode_factor(read_2019$Q04_1, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$comprom) <- "What people call compromise in politics is just selling out one's principles"

pgsw2019$dontcare <- dplyr::recode_factor(read_2019$Q04_2, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$dontcare) <- "Most politicians do not care about the people"

pgsw2019$poltrust <- dplyr::recode_factor(read_2019$Q04_3, 
                                          `5` = "Definitely agree", 
                                          `4` = "Rather agree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather disagree", 
                                          `1` = "Definitely disagree",
                                          .ordered=TRUE)
var_label(pgsw2019$poltrust) <- "Most politicians are worthy of trust"

pgsw2019$polprob <- dplyr::recode_factor(read_2019$Q04_4, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$polprob) <- "Poland's biggest problem is its politicians"

pgsw2019$strgld <- dplyr::recode_factor(read_2019$Q04_5, 
                                        `5` = "Definitely disagree", 
                                        `4` = "Rather disagree", 
                                        `3` = "Neither agree nor disagree", 
                                        `2` = "Rather agree", 
                                        `1` = "Definitely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$strgld) <- "Having a strong leader is important for Poland, even if he bends the rules to get things done"

pgsw2019$peopdec <- dplyr::recode_factor(read_2019$Q04_6, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$peopdec) <- "People, not politicians, should take the most important political decisions"

pgsw2019$richinf <- dplyr::recode_factor(read_2019$Q04_7, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$richinf) <- "Most politicians look out for the interests only of the rich and influential"

pgsw2019$minorit <- dplyr::recode_factor(read_2019$Q05_1, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$minorit) <- "Minorities should adapt to dominant Polish customs and traditions"

pgsw2019$willmaj <- dplyr::recode_factor(read_2019$Q05_2, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$willmaj) <- "The will of the majority should always take precedence, even at the cost of minority rights"

pgsw2019$imgposit <- dplyr::recode_factor(read_2019$Q05_3, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgposit) <- "Immigrants usually have a positive impact on the Polish economy"

pgsw2019$imgcult <- dplyr::recode_factor(read_2019$Q05_4, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgcult) <- "The presence of immigrants has a negative impact on Polish culture"

pgsw2019$imgcrime <- dplyr::recode_factor(read_2019$Q05_5, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$imgcrime) <- "Immigrants are increasing the level of crime in Poland"

pgsw2019$polborn <- dplyr::recode_factor(read_2019$Q06_1,
                                          `4` = "Totally unimportant", 
                                          `3` = "Not very important", 
                                          `2` = "Rather important", 
                                          `1` = "Very important",
                                          .ordered=TRUE)
var_label(pgsw2019$polborn) <- "To be a true Pole it is necessary to: be born in Poland"

pgsw2019$polance <- dplyr::recode_factor(read_2019$Q06_2,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polance) <- "To be a true Pole it is necessary to: have Polish ancestors"

pgsw2019$polspeak <- dplyr::recode_factor(read_2019$Q06_3,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polspeak) <- "To be a true Pole it is necessary to: speak Polish"

pgsw2019$polhabit <- dplyr::recode_factor(read_2019$Q06_4,
                                         `4` = "Totally unimportant", 
                                         `3` = "Not very important", 
                                         `2` = "Rather important", 
                                         `1` = "Very important",
                                         .ordered=TRUE)
var_label(pgsw2019$polhabit) <- "To be a true Pole it is necessary to: observe Polish customs and traditions"

pgsw2019$polcath <- dplyr::recode_factor(read_2019$Q06_5,
                                          `4` = "Totally unimportant", 
                                          `3` = "Not very important", 
                                          `2` = "Rather important", 
                                          `1` = "Very important",
                                          .ordered=TRUE)
var_label(pgsw2019$polcath) <- "To be a true Pole it is necessary to: be a Catholic"

pgsw2019$polinst <- dplyr::recode_factor(read_2019$Q06_6,
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

pgsw2019$corrup <- dplyr::recode_factor(read_2019$Q07,
                                         `4` = "Almost never occurs", 
                                         `3` = "Not that common", 
                                         `2` = "Rather common", 
                                         `1` = "Very common",
                                         `8` = "Hard to say")
var_label(pgsw2019$corrup) <- "How common is corruption among Polish politicians?"

pgsw2019$govegal <- dplyr::recode_factor(read_2019$Q08, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$govegal) <- "The government should take action to reduce difference in incomes"

pgsw2019$govopin <- dplyr::recode_factor(read_2019$Q09, 
                                         `4` = "Very bad", 
                                         `3` = "Bad", 
                                         `2` = "Good", 
                                         `1` = "Very good",
                                         `8` = "Hard to say")
var_label(pgsw2019$govopin) <- "How do you evaluate the performance of the government over the last four years?"

pgsw2019$repres <- recode_factor(read_2019$Q10a,
                                 `2` = "No",
                                 `1` = "Yes")
var_label(pgsw2019$repres) <- "Is there a party that represents your views?"

pgsw2019$ecsitpl <- dplyr::recode_factor(read_2019$Q11, 
                                         `5` = "Got much worse", 
                                         `4` = "Got worse", 
                                         `3` = "Not changed", 
                                         `2` = "Got better",
                                         `1` = "Got much better",
                                         .ordered=TRUE)
var_label(pgsw2019$ecsitpl) <- "In the last year the economic situation in Poland has..."

pgsw2019$exteff1 <- dplyr::recode_factor(read_2019$Q14a, 
                                         `1` = "Has no meaning", 
                                         `2` = "2", 
                                         `3` = "3", 
                                         `4` = "4",
                                         `5` = "Has a lot of meaning",
                                         .ordered=TRUE)
var_label(pgsw2019$exteff1) <- "Extent to which who governs has meaning"

pgsw2019$exteff2 <- dplyr::recode_factor(read_2019$Q14b, 
                                         `1` = "Does not matter at all", 
                                         `2` = "2", 
                                         `3` = "3", 
                                         `4` = "4",
                                         `5` = "Matters a great deal",
                                         .ordered=TRUE)
var_label(pgsw2019$exteff2) <- "Extent to which for whom you vote matters"





pgsw2019$funcdem <- dplyr::recode_factor(read_2019$Q21, 
                                         `5` = "Very discontented", 
                                         `4` = "Rather discontented", 
                                         `3` = "Neither contented nor discontented", 
                                         `2` = "Rather contented",
                                         `1` = "Very contented",
                                         .ordered=TRUE)
var_label(pgsw2019$funcdem) <- "Are you contented with the functioning of democracy in Poland?"

pgsw2019$ptclose <- dplyr::recode_factor(read_2019$Q22a, 
                                         `5` = "No", 
                                         `1` = "Yes")
var_label(pgsw2019$ptclose) <- "Is there a party you regard yourself as close to?"

pgsw2019$ptcloser <- dplyr::recode_factor(read_2019$Q22b, 
                                         `5` = "No", 
                                         `1` = "Yes")
var_label(pgsw2019$ptcloser) <- "Is there a party you regard yourself as closer to than others?"

pgsw2019 <- mutate(pgsw2019, pid = ptclose) %>%
  mutate(pid=replace(pid, pgsw2019$ptcloser=="Yes", "Yes"))
var_label(pgsw2019$pid) <- "Party identification"

pgsw2019$pidstr <- dplyr::recode_factor(read_2019$Q22d, 
                                          `1` = "Very strongly", 
                                          `2` = "Rather strongly",
                                          `3` = "Not that strongly")
var_label(pgsw2019$pidstr) <- "Is there a party you regard yourself as closer to than others?"

pgsw2019$dembest <- dplyr::recode_factor(read_2019$Q23, 
                                         `4` = "Strongly disagree", 
                                         `3` = "Rather disagree", 
                                         `2` = "Rather agree",
                                         `1` = "Strongly agree",
                                         .ordered=TRUE)
var_label(pgsw2019$dembest) <- "Democracy has its problems, but it is a better system of government than any of the alternatives"

pgsw2019$sphealth <- dplyr::recode_factor(read_2019$N1_1,
                                         `5` = "Much lower than at present",
                                         `4` = "A bit lower than at present", 
                                         `3` = "The same as at present", 
                                         `2` = "A bit higher than at present",
                                         `1` = "Much higher than at present",
                                         .ordered=TRUE)
var_label(pgsw2019$sphealth) <- "Public spending on the health service should be..."

pgsw2019$speduc <- dplyr::recode_factor(read_2019$N1_2,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                          .ordered=TRUE)
var_label(pgsw2019$speduc) <- "Public spending on education should be..."

pgsw2019$spunemp <- dplyr::recode_factor(read_2019$N1_3,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                        .ordered=TRUE)
var_label(pgsw2019$spunemp) <- "Public spending on the unemployed should be..."

pgsw2019$sparmy <- dplyr::recode_factor(read_2019$N1_4,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                        .ordered=TRUE)
var_label(pgsw2019$sparmy) <- "Public spending on the army should be..."

pgsw2019$sppensi <- dplyr::recode_factor(read_2019$N1_5,
                                        `5` = "Much lower than at present",
                                        `4` = "A bit lower than at present", 
                                        `3` = "The same as at present", 
                                        `2` = "A bit higher than at present",
                                        `1` = "Much higher than at present",
                                        .ordered=TRUE)
var_label(pgsw2019$sppensi) <- "Public spending on pensions should be..."

pgsw2019$spbusin <- dplyr::recode_factor(read_2019$N1_6,
                                         `5` = "Much lower than at present",
                                         `4` = "A bit lower than at present", 
                                         `3` = "The same as at present", 
                                         `2` = "A bit higher than at present",
                                         `1` = "Much higher than at present",
                                         .ordered=TRUE)
var_label(pgsw2019$spbusin) <- "Public spending on subsidies and support for business should be..."

pgsw2019$sppoljus <- dplyr::recode_factor(read_2019$N1_7,
                                         `5` = "Much lower than at present",
                                         `4` = "A bit lower than at present", 
                                         `3` = "The same as at present", 
                                         `2` = "A bit higher than at present",
                                         `1` = "Much higher than at present",
                                         .ordered=TRUE)
var_label(pgsw2019$sppoljus) <- "Public spending on the police and justice system should be..."

pgsw2019$spsocsc <- dplyr::recode_factor(read_2019$N1_8,
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

pgsw2019$govinfl <- dplyr::recode_factor(read_2019$N2,
                                         `4` = "Not at all", 
                                         `3` = "To a small degree", 
                                         `2` = "To a significant degree",
                                         `1` = "To a very large degree",
                                         .ordered=TRUE)
var_label(pgsw2019$govinfl) <- "To what extent is the financial situation of your family dependent on the recent actions of the government?"

pgsw2019$plecpst <- dplyr::recode_factor(read_2019$N3a,
                                         `5` = "Considerably worsened",
                                         `4` = "Worsened", 
                                         `3` = "Did not change", 
                                         `2` = "Improved",
                                         `1` = "Considerably improved",
                                         .ordered=TRUE)
var_label(pgsw2019$plecpst) <- "Over the last year the economic situation in Poland..."

pgsw2019$famecpst <- dplyr::recode_factor(read_2019$N3b,
                                         `5` = "Considerably worsened",
                                         `4` = "Worsened", 
                                         `3` = "Did not change", 
                                         `2` = "Improved",
                                         `1` = "Considerably improved",
                                         .ordered=TRUE)
var_label(pgsw2019$famecpst) <- "Over the last year your family's material situation..."

pgsw2019$pleccur <- dplyr::recode_factor(read_2019$N4a,
                                         `5` = "Very bad",
                                         `4` = "Bad", 
                                         `3` = "Neither good nor bad", 
                                         `2` = "Good",
                                         `1` = "Very good",
                                         .ordered=TRUE)
var_label(pgsw2019$pleccur) <- "The economic situation in Poland is..."

pgsw2019$fameccur <- dplyr::recode_factor(read_2019$N4b,
                                          `5` = "Very bad",
                                          `4` = "Bad", 
                                          `3` = "Neither good nor bad", 
                                          `2` = "Good",
                                          `1` = "Very good",
                                          .ordered=TRUE)
var_label(pgsw2019$fameccur) <- "Your family's current material situation is..."

pgsw2019$plecfut <- dplyr::recode_factor(read_2019$N5a,
                                         `5` = "Get much worse",
                                         `4` = "Get worse", 
                                         `3` = "Remain the same", 
                                         `2` = "Get better",
                                         `1` = "Get much better",
                                         .ordered=TRUE)
var_label(pgsw2019$plecfut) <- "In the next year, the economic situation in Poland will..."

pgsw2019$famecfut <- dplyr::recode_factor(read_2019$N5b,
                                          `5` = "Get much worse",
                                          `4` = "Get worse", 
                                          `3` = "Remain the same", 
                                          `2` = "Get better",
                                          `1` = "Get much better",
                                          .ordered=TRUE)
var_label(pgsw2019$famecfut) <- "In the next year, your family's current material situation will..."

pgsw2019$confrich <- dplyr::recode_factor(read_2019$N8_1,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confrich) <- "Is there a conflict between rich and poor people in Poland?"

pgsw2019$confage <- dplyr::recode_factor(read_2019$N8_2,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confage) <- "Is there a conflict between young and old people in Poland?"

pgsw2019$confrel <- dplyr::recode_factor(read_2019$N8_3,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confrel) <- "Is there a conflict between believers and non-believers in Poland?"

pgsw2019$confsex <- dplyr::recode_factor(read_2019$N8_4,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confsex) <- "Is there a conflict between men and women in Poland?"

pgsw2019$confwork <- dplyr::recode_factor(read_2019$N8_5,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$confwork) <- "Is there a conflict between workers and employers in Poland?"

pgsw2019$confpupr <- dplyr::recode_factor(read_2019$N8_6,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confpupr) <- "Is there a conflict between public and private sector workers in Poland?"

pgsw2019$confeduc <- dplyr::recode_factor(read_2019$N8_7,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confeduc) <- "Is there a conflict between the poorly educated and the well educated in Poland?"

pgsw2019$confcls <- dplyr::recode_factor(read_2019$N8_8,
                                          `1` = "No conflict", 
                                          `2` = "Moderate conflict",
                                          `3` = "Strong conflict",
                                          .ordered=TRUE)
var_label(pgsw2019$confcls) <- "Is there a conflict between the working class and the middle class in Poland?"

pgsw2019$conffrm <- dplyr::recode_factor(read_2019$N8_9,
                                         `1` = "No conflict", 
                                         `2` = "Moderate conflict",
                                         `3` = "Strong conflict",
                                         .ordered=TRUE)
var_label(pgsw2019$conffrm) <- "Is there a conflict between farmers and urbanites in Poland?"

pgsw2019$confpatr <- dplyr::recode_factor(read_2019$N8_10,
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

 

pgsw2019$hatept <- dplyr::recode_factor(read_2019$P15,
                                        `Platforma Obywatelska` = "PO/KO",
                                        `Koalicja Obywatelska` = "PO/KO",
                                        `Konfederacja Wolność i Niepodległość` = "Konfederacja",
                                        `Konfederacja Wolność i Niepodległość, Wiosna` = "Opposition",
                                        `Korwin` = "Konfederacja",
                                        `Kukiz'15` = "Kukiz'15",
                                        `Lewica Razem` = "Lewica",
                                        `Nie wiem` = "Don't know",
                                        `Odmowa` = "Refused",
                                        `Opozycja` = "Opposition",
                                        `Platforma Obywatelska, Koalicja Obywatelska` = "PO/KO",
                                        `Platforma Obywatelska, Lewica Razem` = "Opposition",
                                        `Platforma Obywatelska, Polskie Stronnictwo Ludowe` = "Opposition",
                                        `Platforma Obywatelska, Polskie Stronnictwo Ludowe, Sojusz Lewicy Demokratycznej` = "Opposition",
                                        `Platforma Obywatelska, Polskie Stronnictwo Ludowe, Wiosna` = "Opposition",
                                        `Platforma Obywatelska, Prawo i Sprawiedliwość` = "All",
                                        `Platforma Obywatelska, Sojusz Lewicy Demokratycznej` = "Opposition",
                                        `Platforma Obywatelska, Sojusz Lewicy Demokratycznej, Polskie Stronnictwo Ludowe` = "Opposition",
                                        `Platforma Obywatelska, Sojusz Lewicy Demokratycznej, Wiosna` = "Opposition",
                                        `Platforma Obywatelska, Wiosna` = "Opposition",
                                        `Platforma Obywatelska, Wiosna, Sojusz Lewicy Demokratycznej` = "Opposition",
                                        `Polskie Stronnictwo Ludowe` = "Other",
                                        `Prawo i Sprawiedliwość` = "PiS",
                                        `Prawo i Sprawiedliwość, Konfederacja Wolność i Niepodległość` = "Other",
                                        `Prawo i Sprawiedliwość, Kukiz’15` = "Other",
                                        `Prawo i Sprawiedliwość, Platforma Obywatelska` = "All",
                                        `Prawo i Sprawiedliwość, Sojusz Lewicy Demokratycznej` = "Other",
                                        `Sojusz Lewicy Demokratycznej, Platforma Obywatelska, Prawo i Sprawiedliwość` = "All",
                                        `Platforma Obywatelska, Sojusz Lewicy Demokratycznej, Polskie Stronnictwo Ludowe` = "Opposition",
                                        `Wiosna, Prawo i Sprawiedliwość` = "Other",
                                        `Wiosna, Kukiz’15` = "Other",
                                        `Wiosna, Koalicja Obywatelska, Konfederacja Wolność i Niepodległość` = "Other",
                                        `WSZYSTKIE - W SZCZEGÓLNOŚCI STAŁY KONFLIKT MIĘDZY PO A PIS'EM.` = "All",
                                        `Koalicja Obywatelska, Platforma Obywatelska` = "PO/KO",
                                        `Prawo i Sprawiedliwość, Kukiz’15` = "Other",
                                        `Wszystkie` = "All",
                                        `Sojusz Lewicy Demokratycznej` = "Lewica",
                                        `Wiosna` = "Lewica",
                                        `Wiosna, Platforma Obywatelska` = "Opposition",
                                        `Sojusz Lewicy Demokratycznej, Platforma Obywatelska` = "Other",
                                        `Większość` = "All")
var_label(pgsw2019$hatept) <- "Is there currently a party in Poland towards which you feel real aversion and anger? If yes, please give the name of that party."
pgsw2019$hatept <- fct_expand(pgsw2019$hatept, "No such party")
pgsw2019 <- pgsw2019 %>%
  mutate(hatept=replace(hatept, read_2019$P15_96==0, "Don't know")) %>%
  mutate(hatept=replace(hatept, read_2019$P15_96==1, "No such party"))
pgsw2019$hatept <- fct_drop(pgsw2019$hatept)
pgsw2019$hatept <- fct_relevel(pgsw2019$hatept, "PiS", "PO/KO", "Lewica", "Kukiz’15", "Konfederacja", "Opposition", "Other", "All", "No such party","Don't know", "Refused")

pgsw2019$banpt <- dplyr::recode_factor(read_2019$P16,
                                        `1` = "Yes", 
                                        `2` = "No",
                                        `97` = "Don't know")
var_label(pgsw2019$banpt) <- "Do you think that this party should be banned?"
pgsw2019$banpt <- fct_expand(pgsw2019$banpt, "Not applicable")
pgsw2019 <- pgsw2019 %>%
  mutate(banpt=replace(banpt, read_2019$P15_96==0, "Not applicable")) %>%
  mutate(banpt=replace(banpt, read_2019$P15_96==1, "Not applicable"))

pgsw2019$pres2015 <- dplyr::recode_factor(read_2019$P18,
                                       `1` = "Bronisław Komorowski", 
                                       `2` = "Andrzej Duda",
                                       `3` = "Did not vote",
                                       `4` = "Was not eligible to vote")
var_label(pgsw2019$pres2015) <- "Did you participate in the second round of the presidential election on 24th of May 2015? If yes, for which of the candidates did you vote?"

pgsw2019$neword <- dplyr::recode_factor(read_2019$P19_1,
                                          `5` = "Totally disagree", 
                                          `4` = "Disagree",
                                          `3` = "Neither agree nor disagree",
                                          `2` = "Agree",
                                          `1` = "Totally agree",
                                          .ordered=TRUE)
var_label(pgsw2019$neword) <- "Poland needs someone who will have the strength to change our system completely and introduce a new, just order."

pgsw2019$ordpeop <- dplyr::recode_factor(read_2019$P19_2,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$ordpeop) <- "In Poland a few people arrogate to themselves the power that should belong to the ordinary people."

pgsw2019$crisis <- dplyr::recode_factor(read_2019$P19_3,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$crisis) <- "It is the last moment to save Poland in the face of a threatening crisis."

pgsw2019$morals <- dplyr::recode_factor(read_2019$P19_4,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$morals) <- "Nothing in Poland will improve until politicians return to good old-fashioned moral values."

pgsw2019$simple <- dplyr::recode_factor(read_2019$P19_5,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$simple) <- "To solve our country's problems is very straightforward; we just need to give power to those who want to do this."

pgsw2019$goodev <- dplyr::recode_factor(read_2019$P19_6,
                                        `5` = "Totally disagree", 
                                        `4` = "Disagree",
                                        `3` = "Neither agree nor disagree",
                                        `2` = "Agree",
                                        `1` = "Totally agree",
                                        .ordered=TRUE)
var_label(pgsw2019$goodev) <- "Everything in politics is either good or evil; the choice is simple."

pgsw2019$lgov2018 <- dplyr::recode_factor(read_2019$P24,
                                          `1` = "Yes", 
                                          `2` = "No")
var_label(pgsw2019$lgov2018) <- "Did you participate in the most recent local elections, which took place in the autumn of 2018?"


pgsw2019$impdem_1 <- dplyr::recode(read_2019$D1_1, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_1 <- add_labels(pgsw2019$impdem_1, labels = c("This is completely unimportant for democracy in general" = 0,
                                                                    '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                                    "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_1) <- "Please say to what extent, in your opinion, it is important for democracy that parliamentary elections are free and fair"

pgsw2019$impdem_2 <- dplyr::recode(read_2019$D2_2, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_2 <- add_labels(pgsw2019$impdem_2, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_2) <- "Please say to what extent, in your opinion, it is important for democracy that political parties constitute a genuine alternative to each other; that they are significantly different from one another"

pgsw2019$impdem_3 <- dplyr::recode(read_2019$D3_3, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_3 <- add_labels(pgsw2019$impdem_3, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_3) <- "Please say to what extent, in your opinion, it is important for democracy that the media provide citizens with reliable information on the basis of which they can evaluate the actions of the government"

pgsw2019$impdem_4 <- dplyr::recode(read_2019$D4_4, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_4 <- add_labels(pgsw2019$impdem_4, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_4) <- "Please say to what extent, in your opinion, it is important for democracy that citizens have the deciding voice on the most important political questions by voting on them in a referendum"

pgsw2019$impdem_5 <- dplyr::recode(read_2019$D5_5, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_5 <- add_labels(pgsw2019$impdem_5, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_5) <- "Please say to what extent, in your opinion, it is important for democracy that constitutional courts have the possibility to restrain a government that is exceeding its prerogatives"

pgsw2019$impdem_6 <- dplyr::recode(read_2019$D6_6, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_6 <- add_labels(pgsw2019$impdem_6, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_6) <- "Please say to what extent, in your opinion, it is important for democracy that courts treat everyone equally"

pgsw2019$impdem_7 <- dplyr::recode(read_2019$D7_7, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_7 <- add_labels(pgsw2019$impdem_7, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_7) <- "Please say to what extent, in your opinion, it is important for democracy that the government protects everyone from poverty"

pgsw2019$impdem_8 <- dplyr::recode(read_2019$D8_8, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_8 <- add_labels(pgsw2019$impdem_8, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_8) <- "Please say to what extent, in your opinion, it is important for democracy that the government explains its decisions to the electorate"

pgsw2019$impdem_9 <- dplyr::recode(read_2019$D9_9, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_9 <- add_labels(pgsw2019$impdem_9, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_9) <- "Please say to what extent, in your opinion, it is important for democracy that the interpretation of laws is, in the last instance, reserved for religious authorities"

pgsw2019$impdem_10 <- dplyr::recode(read_2019$D10_10, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$impdem_10 <- add_labels(pgsw2019$impdem_10, labels = c("This is completely unimportant for democracy in general" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "This is of great importance for democracy in general" = 10))
var_label(pgsw2019$impdem_10) <- "Please say to what extent, in your opinion, it is important for democracy that people obey the government"

pgsw2019$psystem <- dplyr::recode_factor(read_2019$D9a,
                                        `1` = "One party forms the government", 
                                        `2` = "A coalition of two or more parties forms the government",
                                        `3` = "It depends upon the circumstances",
                                        `4` = "Don't know")
var_label(pgsw2019$psystem) <- "In some countries the government is comprised of one party, and in others by a coalition of two or more parties. Which of these two possibilities is, in your opinion, best for democracy in general?"

pgsw2019$actdem_1 <- dplyr::recode(read_2019$D10_1, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_1 <- add_labels(pgsw2019$actdem_1, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_1) <- "Elections to the Sejm are free and fair"

pgsw2019$actdem_2 <- dplyr::recode(read_2019$D11_2, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_2 <- add_labels(pgsw2019$actdem_2, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_2) <- "Political parties in Poland offer genuine alternatives; they can clearly be differentiated from each other"

pgsw2019$actdem_3 <- dplyr::recode(read_2019$D12_3, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_3 <- add_labels(pgsw2019$actdem_3, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_3) <- "The media in Poland give citizens reliable information on the basis of which they can evaluate the government"

pgsw2019$actdem_4 <- dplyr::recode(read_2019$D13_4, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_4 <- add_labels(pgsw2019$actdem_4, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_4) <- "Polish citizens have a decisive voice in important political questions, voting directly on them in referendums"

pgsw2019$actdem_5 <- dplyr::recode(read_2019$D14_5, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_5 <- add_labels(pgsw2019$actdem_5, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_5) <- "Polish courts treat everyone the same"

pgsw2019$actdem_6 <- dplyr::recode(read_2019$D15_6, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_6 <- add_labels(pgsw2019$actdem_6, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_6) <- "Constitutional courts have the ability to restrain the government when it exceeds its prerogatives"

pgsw2019$actdem_7 <- dplyr::recode(read_2019$D16_7, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_7 <- add_labels(pgsw2019$actdem_7, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_7) <- "The Polish government protects all citizens from poverty"

pgsw2019$actdem_8 <- dplyr::recode(read_2019$D17_8, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_8 <- add_labels(pgsw2019$actdem_8, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_8) <- "The Polish government explains its decisions to the electorate. "

pgsw2019$actdem_9 <- dplyr::recode(read_2019$D18_9, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_9 <- add_labels(pgsw2019$actdem_9, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                              '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                              "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_9) <- "In the last instance, the interpretation of laws is reserved for religious authorities"

pgsw2019$actdem_10 <- dplyr::recode(read_2019$D19_10, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2019$actdem_10 <- add_labels(pgsw2019$actdem_10, labels = c("Does not correspond to the situation in Poland at all" = 0,
                                                                '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                                "Completely corresponds to the situation in Poland" = 10))
var_label(pgsw2019$actdem_10) <- "Poles obey the government"

# pgsw2019$dispdem_1 <- abs(pgsw2019$impdem_1 - pgsw2019$actdem_1)
# pgsw2019$dispdem_2 <- abs(pgsw2019$impdem_2 - pgsw2019$actdem_2)
# pgsw2019$dispdem_3 <- abs(pgsw2019$impdem_3 - pgsw2019$actdem_3)
# pgsw2019$dispdem_4 <- abs(pgsw2019$impdem_4 - pgsw2019$actdem_4)
# pgsw2019$dispdem_5 <- abs(pgsw2019$impdem_5 - pgsw2019$actdem_5)
# pgsw2019$dispdem_6 <- abs(pgsw2019$impdem_6 - pgsw2019$actdem_6)
# pgsw2019$dispdem_7 <- abs(pgsw2019$impdem_7 - pgsw2019$actdem_7)
# pgsw2019$dispdem_8 <- abs(pgsw2019$impdem_8 - pgsw2019$actdem_8)
# pgsw2019$dispdem_9 <- abs(pgsw2019$impdem_9 - pgsw2019$actdem_9)
# pgsw2019$dispdem_10 <- abs(pgsw2019$impdem_10 - pgsw2019$actdem_10)
# pgsw2019$dispdem <- with(pgsw2019, rowMeans(data.frame(dispdem_1, dispdem_2, dispdem_3, dispdem_4, dispdem_5, dispdem_6, dispdem_7, dispdem_8, dispdem_9, dispdem_10), na.rm=TRUE))

pgsw2019$eqfree <- dplyr::recode_factor(read_2019$D21,
                                         `1` = "Definitely freedom", 
                                         `2` = "Rather freedom",
                                         `3` = "Rather equality",
                                         `4` = "Definitely equality")
var_label(pgsw2019$eqfree) <- "Which of these values is, in your opinion, the most important at present?"

pgsw2019$stateval <- dplyr::recode_factor(read_2019$D22,
                                        `1` = "A state which defends and supports traditional values", 
                                        `2` = "A state which supports social progress and modern values")
var_label(pgsw2019$stateval) <- "There are varying conceptions of the role of the contemporary state. Which of these is closer to your conception?"

pgsw2019$statesol <- dplyr::recode_factor(read_2019$D23,
                                          `1` = "A state which creates the foundations for social solidarity", 
                                          `2` = "A state which creates conditions favourable for the entrepreneurship of individuals")
var_label(pgsw2019$statesol) <- "There are varying conceptions of the role of the contemporary state. Which of these is closer to your conception?"

pgsw2019$discspouse <- dplyr::recode_factor(read_2019$T1,
                                          `0` = "Never",
                                          `1` = "Rarely",
                                          `2` = "Sometimes",
                                          `3` = "Often",
                                          `95` = "Not applicable")
var_label(pgsw2019$discspouse) <- "How often have you discussed the recent elections with your spouse or partner?"

pgsw2019$votespouse <- recode_factor(read_2019$T2,
                                  `2` = "PiS",
                                  `5` = "KO",
                                  `3` = "Lewica",
                                  `1` = "PSL-Kukiz",
                                  `4` = "Konfederacja",
                                  `0` = "Did not vote")
var_label(pgsw2019$votespouse) <- "Which party did your spouse or partner vote for in the most recent parliamentary elections?"

pgsw2019$discfam <- dplyr::recode_factor(read_2019$T3_1,
                                          `1` = "Often",
                                          `2` = "Sometimes",
                                          `3` = "Rarely",
                                          `4` = "Never",
                                          `95` = "Not applicable")
var_label(pgsw2019$discfam) <- "How often have you discussed the recent elections with your family?"

pgsw2019$discfre <- dplyr::recode_factor(read_2019$T3_2,
                                         `1` = "Often",
                                         `2` = "Sometimes",
                                         `3` = "Rarely",
                                         `4` = "Never",
                                         `95` = "Not applicable")
var_label(pgsw2019$discfre) <- "How often have you discussed the recent elections with your friends and acquaintances?"

pgsw2019$discneigh <- dplyr::recode_factor(read_2019$T3_3,
                                         `1` = "Often",
                                         `2` = "Sometimes",
                                         `3` = "Rarely",
                                         `4` = "Never",
                                         `95` = "Not applicable")
var_label(pgsw2019$discneigh) <- "How often have you discussed the recent elections with your neighbours?"

pgsw2019$discwork <- dplyr::recode_factor(read_2019$T3_4,
                                           `1` = "Often",
                                           `2` = "Sometimes",
                                           `3` = "Rarely",
                                           `4` = "Never",
                                           `95` = "Not applicable")
var_label(pgsw2019$discwork) <- "How often have you discussed the recent elections with your work colleagues?"

pgsw2019$samefam <- dplyr::recode_factor(read_2019$T4_1,
                                         `1` = "Supported the same party",
                                         `2` = "Supported a different party",
                                         `95` = "Not applicable",
                                         `96` = "Not applicable",
                                         `999` = "Not applicable",
                                         `97` = "Don't know")
var_label(pgsw2019$samefam) <- "Do you think that your family supported the same party as you, or supported another party?"

pgsw2019$samefre <- dplyr::recode_factor(read_2019$T4_2,
                                         `1` = "Supported the same party",
                                         `2` = "Supported a different party",
                                         `95` = "Not applicable",
                                         `96` = "Not applicable",
                                         `999` = "Not applicable",
                                         `97` = "Don't know")
var_label(pgsw2019$samefre) <- "Do you think that your friends and acquaintances supported the same party as you, or supported another party?"

pgsw2019$sameneigh <- dplyr::recode_factor(read_2019$T4_3,
                                           `1` = "Supported the same party",
                                           `2` = "Supported a different party",
                                           `95` = "Not applicable",
                                           `96` = "Not applicable",
                                           `999` = "Not applicable",
                                           `97` = "Don't know")
var_label(pgsw2019$sameneigh) <- "Do you think that your neighbours supported the same party as you, or supported another party?"

pgsw2019$samework <- dplyr::recode_factor(read_2019$T4_4,
                                          `1` = "Supported the same party",
                                          `2` = "Supported a different party",
                                          `95` = "Not applicable",
                                          `96` = "Not applicable",
                                          `999` = "Not applicable",
                                          `97` = "Don't know")
var_label(pgsw2019$samework) <- "Do you think that your work colleagues supported the same party as you, or supported another party?"

pgsw2019$discnumb <- read_2019$T5
var_label(pgsw2019$discnumb) <- "In total, with how many people did you discuss the most recent election campaign?"

pgsw2019$suppnumb <- dplyr::recode_factor(read_2019$T6,
                                          `3` = "All or almost all",
                                          `2` = "The majority",
                                          `1` = "Some",
                                          `0` = "None",
                                          `95` = "Not applicable")
var_label(pgsw2019$suppnumb) <- "Taking into consideration all your conversations about the last electoral campaign, how many of those you spoke with supported the same party as you?"

pgsw2019$voteadv <- dplyr::recode_factor(read_2019$T7,
                                          `0` = "No",
                                          `1` = "Yes, from the leader of a local civil society organisation",
                                          `2` = "Yes, from a member of the clergy",
                                          `3` = "Yes, from both")
var_label(pgsw2019$voteadv) <- "During the most recent electoral campaign, did you receive any advice from a leader of a local civil society organisation or member of the clergy, or from both, on which party to support in the parliamentary elections?"

pgsw2019$voteadv_p <- dplyr::recode_factor(read_2019$T8,
                                         `1` = "PiS",
                                         `2` = "PO",
                                         `3` = "Razem",
                                         `4` = "Konfederacja",
                                         `5` = "PSL",
                                         `6` = "Lewica",
                                         `7` = "Kukiz’15",
                                         `8` = "Nowoczesna",
                                         `9` = "Wiosna",
                                         `10` = "Other")
var_label(pgsw2019$voteadv_p) <- "Name of party"

pgsw2019$arguefam <- dplyr::recode_factor(read_2019$T9,
                                           `1` = "Yes, very often",
                                           `2` = "Yes, sometimes",
                                           `3` = "Rarely, almost never",
                                           `4` = "Never")
var_label(pgsw2019$arguefam) <- "During the last election campaign, did you experience any situations in which you avoided talking about politics with family members or friends because you wanted to avoid arguing with someone who has different views?"

pgsw2019$untruefam <- dplyr::recode_factor(read_2019$T10,
                                          `1` = "Yes, very often",
                                          `2` = "Yes, sometimes",
                                          `3` = "Rarely, almost never",
                                          `4` = "Never")
var_label(pgsw2019$untruefam) <- "During the last election campaign, did you experience any situations in which during a conversation about politics with family members or friends, someone said something that was obviously untrue?"

pgsw2019$oppofam <- dplyr::recode_factor(read_2019$T11,
                                           `1` = "Yes",
                                           `2` = "No")
var_label(pgsw2019$oppofam) <- "If yes, did you express opposition to this?"

pgsw2019$voteEP <- dplyr::recode_factor(read_2019$P25,
                                           `1` = "KE",
                                           `2` = "PiS",
                                           `3` = "Wiosna",
                                           `4` = "Konfederacja",
                                           `5` = "Kukiz’15",
                                           `9` = "Other",
                                           `95` = "Did not vote")
var_label(pgsw2019$voteEP) <- "Name of party"

pgsw2019$knowtrans <- dplyr::recode_factor(read_2019$KS1a,
                                         `1` = "Yes",
                                         `2` = "No")
var_label(pgsw2019$knowtrans) <- "Do you know someone who can help translate something into a foreign language?"

pgsw2019$knowempl <- dplyr::recode_factor(read_2019$KS1b,
                                           `1` = "Yes",
                                           `2` = "No")
var_label(pgsw2019$knowempl) <- "Do you know someone who sometimes can offer people employment?"

pgsw2019$knowadv <- dplyr::recode_factor(read_2019$KS1c,
                                          `1` = "Yes",
                                          `2` = "No")
var_label(pgsw2019$knowadv) <- "Do you know someone who can advise how to sort out issues with local administrative offices?"

pgsw2019$knowpress <- dplyr::recode_factor(read_2019$KS1d,
                                         `1` = "Yes",
                                         `2` = "No")
var_label(pgsw2019$knowpress) <- "Do you know someone who can bring important issues to the attention of the local press?"

pgsw2019$knowlend <- dplyr::recode_factor(read_2019$KS1e,
                                           `1` = "Yes",
                                           `2` = "No")
var_label(pgsw2019$knowlend) <- "Do you know someone who can lend a significant amount of money?"

pgsw2019$knowtrans_who <- dplyr::recode_factor(read_2019$KS2a,
                                           `1` = "Family member",
                                           `2` = "Friend or acquaintance",
                                           `97` = "Either family member or friend/acquaintance")
var_label(pgsw2019$knowtrans_who) <- "Do you know someone who can help translate something into a foreign language?"

pgsw2019$knowempl_who <- dplyr::recode_factor(read_2019$KS2b,
                                              `1` = "Family member",
                                              `2` = "Friend or acquaintance",
                                              `97` = "Either family member or friend/acquaintance")
var_label(pgsw2019$knowempl_who) <- "Do you know someone who sometimes can offer people employment?"

pgsw2019$knowadv_who <- dplyr::recode_factor(read_2019$KS2c,
                                             `1` = "Family member",
                                             `2` = "Friend or acquaintance",
                                             `97` = "Either family member or friend/acquaintance")
var_label(pgsw2019$knowadv_who) <- "Do you know someone who can advise how to sort out issues with local administrative offices?"

pgsw2019$knowpress_who <- dplyr::recode_factor(read_2019$KS2d,
                                               `1` = "Family member",
                                               `2` = "Friend or acquaintance",
                                               `97` = "Either family member or friend/acquaintance")
var_label(pgsw2019$knowpress_who) <- "Do you know someone who can bring important issues to the attention of the local press?"

pgsw2019$knowlend_who <- dplyr::recode_factor(read_2019$KS2e,
                                              `1` = "Family member",
                                              `2` = "Friend or acquaintance",
                                              `97` = "Either family member or friend/acquaintance")
var_label(pgsw2019$knowlend_who) <- "Do you know someone who can lend a significant amount of money?"

pgsw2019$infpapers <- dplyr::recode_factor(read_2019$H1_1,
                                              `4` = "Daily or almost daily",
                                              `3` = "3-4 times a week",
                                              `2` = "1-2 times a week",
                                           `1` = "Less than once a week",
                                           `0` = "Never")
var_label(pgsw2019$infpapers) <- "How often during the most recent election campaign did you pay attention to information on the election in newspapers or the internet sites of newspapers?"

pgsw2019$infradio <- dplyr::recode_factor(read_2019$H1_2,
                                           `4` = "Daily or almost daily",
                                           `3` = "3-4 times a week",
                                           `2` = "1-2 times a week",
                                           `1` = "Less than once a week",
                                           `0` = "Never")
var_label(pgsw2019$infradio) <- "How often during the most recent election campaign did you pay attention to information on the radio?"

pgsw2019$inftele <- dplyr::recode_factor(read_2019$H1_3,
                                          `4` = "Daily or almost daily",
                                          `3` = "3-4 times a week",
                                          `2` = "1-2 times a week",
                                          `1` = "Less than once a week",
                                          `0` = "Never")
var_label(pgsw2019$inftele) <- "How often during the most recent election campaign did you pay attention to information on the television?"

pgsw2019$infsocme <- dplyr::recode_factor(read_2019$H1_4,
                                         `4` = "Daily or almost daily",
                                         `3` = "3-4 times a week",
                                         `2` = "1-2 times a week",
                                         `1` = "Less than once a week",
                                         `0` = "Never")
var_label(pgsw2019$infsocme) <- "How often during the most recent election campaign did you pay attention to information on social media?"

pgsw2019$infsupp <- dplyr::recode_factor(read_2019$H2,
                                          `3` = "All or almost all",
                                          `2` = "Most",
                                          `1` = "Some",
                                          `0` = "None",
                                         `95` = "No party supported")
var_label(pgsw2019$infsupp) <- "Please estimate to what extent the media outlets you consulted to gain information about the election campaign supported the same party you support"

#H3-H5 needs doing

pgsw2019$socmed <- dplyr::recode_factor(read_2019$H7,
                                         `0` = "Never",
                                         `1` = "Almost never",
                                         `2` = "Sometimes",
                                         `3` = "Often")
var_label(pgsw2019$socmed) <- "During the last election campaign, did you share your thoughts or opinions about a particular candidate by email, text message or on social media?"


pgsw2019$unkelit <- as.factor(dplyr::recode(read_2019$H10_1, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$unkelit <- add_labels(pgsw2019$unkelit, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$unkelit) <- "It isn’t the government who governs over us; those who really control us are not known to us"  

pgsw2019$conspir <- as.factor(dplyr::recode(read_2019$H10_2, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$conspir <- add_labels(pgsw2019$conspir, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$conspir) <- "Those who claim that there are powerful organisations in the world which conspire against Poland have a good point"  

pgsw2019$obeypar <- as.factor(dplyr::recode(read_2019$H10_3, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$obeypar <- add_labels(pgsw2019$obeypar, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$obeypar) <- "The most important thing to teach children is complete obedience to their parents"  

pgsw2019$sevmana <- as.factor(dplyr::recode(read_2019$H10_4, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$sevmana <- add_labels(pgsw2019$sevmana, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$sevmana) <- "Every good manager who wants to gain respect and attention should be severe and demanding towards the people he/she supervises"  

pgsw2019$strweak <- as.factor(dplyr::recode(read_2019$H10_5, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$strweak <- add_labels(pgsw2019$strweak, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$strweak) <- "If you think about it, you have to admit that there are only really two types of person in the world: strong people and weak people"  

pgsw2019$specads <- as.factor(dplyr::recode(read_2019$H10_6, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$specads <- add_labels(pgsw2019$specads, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$specads) <- "In this complicated world the only way to gain an understanding of how to proceed is to consult specialists and advisors"  

pgsw2019$nosense <- as.factor(dplyr::recode(read_2019$H10_7, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$nosense <- add_labels(pgsw2019$nosense, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$nosense) <- "It is increasingly difficult to find any sense in the world around us"  

pgsw2019$uncert <- as.factor(dplyr::recode(read_2019$H10_8, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$uncert <- add_labels(pgsw2019$uncert, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$uncert) <- "The only thing you can be certain of these days is that you cannot be certain of anything"  

pgsw2019$whatbel <- as.factor(dplyr::recode(read_2019$H10_9, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$whatbel <- add_labels(pgsw2019$whatbel, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$whatbel) <- "Amid so many different ideas, theories and worldviews, it is often difficult to have any idea what to believe"  

pgsw2019$isolate <- as.factor(dplyr::recode(read_2019$H10_10, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$isolate <- add_labels(pgsw2019$isolate, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$isolate) <- "I feel increasingly isolated and disoriented in our current socio-political system"  

pgsw2019$currpol <- as.factor(dplyr::recode(read_2019$H10_11, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$currpol <- add_labels(pgsw2019$currpol, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$currpol) <- "I often have the feeling that the current policies of the government are my policies"  

pgsw2019$cogpol <- as.factor(dplyr::recode(read_2019$H10_12, `1` = 5L, `2` = 4L, `7` = 3L, `3` = 2L, `4` = 1L))
pgsw2019$cogpol <- add_labels(pgsw2019$cogpol, labels = c("Disagree" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4,
                                                            "Agree" = 5))
var_label(pgsw2019$cogpol) <- "We are all meaningless cogs in the political machine"  

pgsw2019$simp_1 <- dplyr::recode_factor(read_2019$S1_1,
                                        `1` = "Strongly disagree",
                                        `2` = "Rather disagree",
                                        `3` = "Hard to say",
                                        `4` = "Rather agree",
                                        `5` = "Strongly agree")
var_label(pgsw2019$simp_1) <- "Instead of taking effective action, politicians constantly over-complicate simple matters"

pgsw2019$simp_2 <- dplyr::recode_factor(read_2019$S1_2,
                                        `1` = "Strongly disagree",
                                        `2` = "Rather disagree",
                                        `3` = "Hard to say",
                                        `4` = "Rather agree",
                                        `5` = "Strongly agree")
var_label(pgsw2019$simp_2) <- "Scientists and experts from various disciplines over-complicate things which are actually rather straightforward"

pgsw2019$simp_3 <- dplyr::recode_factor(read_2019$S1_3,
                                        `1` = "Strongly disagree",
                                        `2` = "Rather disagree",
                                        `3` = "Hard to say",
                                        `4` = "Rather agree",
                                        `5` = "Strongly agree")
var_label(pgsw2019$simp_3) <- "At present, the medical profession just confuses people rather than helps them; to be healthy it’s enough to live in a natural way and listen to people similar to us"

pgsw2019$simp_4 <- dplyr::recode_factor(read_2019$S1_4,
                                        `1` = "Strongly disagree",
                                        `2` = "Rather disagree",
                                        `3` = "Hard to say",
                                        `4` = "Rather agree",
                                        `5` = "Strongly agree")
var_label(pgsw2019$simp_4) <- "Solving our country’s problems is very simple, we just have to give power to people who want to do this"

pgsw2019$simp_5 <- dplyr::recode_factor(read_2019$S1_5,
                                        `1` = "Strongly disagree",
                                        `2` = "Rather disagree",
                                        `3` = "Hard to say",
                                        `4` = "Rather agree",
                                        `5` = "Strongly agree")
var_label(pgsw2019$simp_5) <- "The work of parliaments consists of endless debate over the details of bills, but in some cases the regulation of particular issues is simple enough that any of us could do it"

pgsw2019$simp_6 <- dplyr::recode_factor(read_2019$S1_6,
                                        `1` = "Strongly disagree",
                                        `2` = "Rather disagree",
                                        `3` = "Hard to say",
                                        `4` = "Rather agree",
                                        `5` = "Strongly agree")
var_label(pgsw2019$simp_6) <- "Politics is a fierce battle between good, represented by the ordinary people, and evil, resulting from the actions of a corrupt elite"

pgsw2019$diffview <- dplyr::recode_factor(read_2019$H11,
                                        `1` = "Parties should stick firmly to their convictions, even if other parties disagree with them",
                                        `2` = "Parties should be prepared to cooperate with others, even if that would mean significant compromises with respect to their own convictions",
                                        `96` = "Don't know")
var_label(pgsw2019$diffview) <- "In politics parties often have different views. Which of the following alternatives do you think is better?"

pgsw2019$exparmy <- dplyr::recode_factor(read_2019$PZ1,
                                        `1` = "Strongly agree",
                                        `2` = "Rather agree",
                                        `3` = "Rather disagree",
                                        `4` = "Strongly disagree",
                                        `97` = "Don't know")
var_label(pgsw2019$exparmy) <- "Poland should increase expenditure on the army and the purchase of military equipment, even at the cost of reducing money spent on social services like pensions and healthcare"

pgsw2019$sidepol <- dplyr::recode_factor(read_2019$PZ3,
                                         `1` = "On the side of the EU",
                                         `2` = "On the side of the USA",
                                         `3` = "Poland should not get involved in such conflicts",
                                         `97` = "Don't know")
var_label(pgsw2019$sidepol) <- "On whose side should Poland stand if there is a serious disagreement between the European Union and the United States of America on key issues in the international arena? "

pgsw2019$ambition <- dplyr::recode_factor(read_2019$H12_1,
                                         `1` = "Absolutely necessary",
                                         `2` = "Very important",
                                         `3` = "Rather important",
                                         `4` = "Not very important",
                                         `5` = "Totally unimportant")
var_label(pgsw2019$ambition) <- "Importance for success in life: ambition"

pgsw2019$knowright <- dplyr::recode_factor(read_2019$H12_2,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$knowright) <- "Importance for success in life: knowing the right people"

pgsw2019$hardwork <- dplyr::recode_factor(read_2019$H12_3,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$hardwork) <- "Importance for success in life: hard work"

pgsw2019$polinf <- dplyr::recode_factor(read_2019$H12_4,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$polinf) <- "Importance for success in life: access to political influence"

pgsw2019$richfam <- dplyr::recode_factor(read_2019$H12_5,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$richfam) <- "Importance for success in life: coming from a rich family"

pgsw2019$goodeduc <- dplyr::recode_factor(read_2019$H12_6,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$goodeduc) <- "Importance for success in life: a good education"

pgsw2019$goodluck <- dplyr::recode_factor(read_2019$H12_7,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$goodluck) <- "Importance for success in life: luck"

pgsw2019$talent <- dplyr::recode_factor(read_2019$H12_8,
                                          `1` = "Absolutely necessary",
                                          `2` = "Very important",
                                          `3` = "Rather important",
                                          `4` = "Not very important",
                                          `5` = "Totally unimportant")
var_label(pgsw2019$talent) <- "Importance for success in life: in-born talent"

pgsw2019$talent <- dplyr::recode_factor(read_2019$H12_8,
                                        `1` = "Absolutely necessary",
                                        `2` = "Very important",
                                        `3` = "Rather important",
                                        `4` = "Not very important",
                                        `5` = "Totally unimportant")
var_label(pgsw2019$talent) <- "Importance for success in life: in-born talent"

pgsw2019$polsyspl <- read_2019$H13_1
pgsw2019$polsyspl <- set_labels(pgsw2019$polsyspl, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$polsyspl) <- "The political system in Poland works as it should"

pgsw2019$polpdes <- read_2019$H13_2
pgsw2019$polpdes <- set_labels(pgsw2019$polpdes, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$polpdes) <- "In general, people in Poland get what they deserve"

pgsw2019$polrich <- read_2019$H13_3
pgsw2019$polrich <- set_labels(pgsw2019$polrich, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$polrich) <- "In Poland today everyone has a more or less equal opportunity to be rich and happy"

pgsw2019$poljust <- read_2019$H13_4
pgsw2019$poljust <- set_labels(pgsw2019$poljust, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$poljust) <- "In general, our society is ruled over justly"

pgsw2019$polgood <- read_2019$H13_5
pgsw2019$polgood <- set_labels(pgsw2019$polgood, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$polgood) <- "Politics usually serves good goals"

pgsw2019$radchg <- read_2019$H13_6
pgsw2019$radchg <- set_labels(pgsw2019$radchg, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$radchg) <- "The organisation of our society demands radical change"

pgsw2019$socworse <- read_2019$H13_7
pgsw2019$socworse <- set_labels(pgsw2019$socworse, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$socworse) <- "Year on year, our society gets worse"

pgsw2019$polbest <- read_2019$H13_8
pgsw2019$polbest <- set_labels(pgsw2019$polbest, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$polbest) <- "For me, Poland is the best country to live in"

pgsw2019$lawjust <- read_2019$H13_9
pgsw2019$lawjust <- set_labels(pgsw2019$lawjust, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$lawjust) <- "Polish law is just, and applies the same to everyone"

pgsw2019$lawabide <- read_2019$H13_10
pgsw2019$lawabide <- set_labels(pgsw2019$lawabide, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$lawabide) <- "People who abide by the law have it better in Poland than people who break the law"

pgsw2019$sucdeser <- read_2019$H13_11
pgsw2019$sucdeser <- set_labels(pgsw2019$sucdeser, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$sucdeser) <- "In Poland, those who have the most success are those who do not deserve it"

pgsw2019$polinco <- read_2019$H13_12
pgsw2019$polinco <- set_labels(pgsw2019$polinco, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$polinco) <- "At all levels, people holding political power are too often incompetent"

pgsw2019$rulinte <- read_2019$H13_13
pgsw2019$rulinte <- set_labels(pgsw2019$rulinte, labels = c("I definitely do not agree" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, "I definitely agree" = 7))
var_label(pgsw2019$rulinte) <- "Today’s Poland is ruled over by people who are only in it for their own interests"

pgsw2019$execut <- read_2019$H14a
pgsw2019$execut <- set_labels(pgsw2019$execut, labels = c("The executive branch (PM and ministers) is more important than the judiciary and legislative branches" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                            "The executive branch is equally important as the judiciary and legislative branches" = 7))
var_label(pgsw2019$execut) <- "The importance of the executive"

pgsw2019$violconst <- read_2019$H14b
pgsw2019$violconst <- set_labels(pgsw2019$violconst, labels = c("Over the last four years the PiS government may have violated the Constitution and bent the law, but in doing so it helped the poor" = 1,
                                                            '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                            "The Constitution is sacred; there is no justification for violating it and for bending the law, even if you help the poor while doing so" = 7))
var_label(pgsw2019$violconst) <- "PiS's violation of the Constitution"

pgsw2019$healthsy <- read_2019$H14c
pgsw2019$healthsy <- set_labels(pgsw2019$healthsy, labels = c("The terrible state of the health system is the result of incorrect policy priorities and spending on other superfluous goals" = 1,
                                                                '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                                "The health system is fine; the government’s policy in this domain is credible" = 7))
var_label(pgsw2019$healthsy) <- "The healthcare system"

pgsw2019$retage <- read_2019$H14d
pgsw2019$retage <- set_labels(pgsw2019$retage, labels = c("The lowering of the retirement age is a very good idea, as people should be able to get the most out of old age" = 1,
                                                              '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                              "The lowering of the retirement age is a disastrous decision, which will negatively affect the economy and result in low pensions for the elderly" = 7))
var_label(pgsw2019$retage) <- "The lowering of the retirement age"

pgsw2019$intarena <- read_2019$H14e
pgsw2019$intarena <- set_labels(pgsw2019$intarena, labels = c("Poland's position in the international arena is in a disastrous state; we have no allies in Europe" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "Poland’s position in Europe is very good, and the US considers Poland its most important ally" = 7))
var_label(pgsw2019$intarena) <- "Poland's position in the international arena"

pgsw2019$theocr <- read_2019$H14f
pgsw2019$theocr <- set_labels(pgsw2019$theocr, labels = c("Our social and political life should be directed by the values expressed in the Bible, and by the instructions of the clergy" = 1,
                                                              '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                              "Our social and political life should be directed by laws created by citizens and protected by the Constitution" = 7))
var_label(pgsw2019$theocr) <- "The role of the Church in politics"

pgsw2019$diffinc <- dplyr::recode_factor(read_2019$K1_a, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$diffinc) <- "Large differences in people’s incomes are acceptable to properly reward differences in talents and efforts"

pgsw2019$livstand <- dplyr::recode_factor(read_2019$K1_b, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$livstand) <- "For a society to be fair, differences in people’s standard of living should be small"

pgsw2019$diffinco <- dplyr::recode_factor(read_2019$K1_c, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$diffinco) <- "The government should take measures to reduce differences in income levels"

pgsw2019$obeyauth <- dplyr::recode_factor(read_2019$K1_d, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$obeyauth) <- "Schools must teach children to obey authority"

pgsw2019$breaklaw <- dplyr::recode_factor(read_2019$K1_e, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$breaklaw) <- "People who break the law should be given much harsher sentences than they are these days"

pgsw2019$imprterr <- dplyr::recode_factor(read_2019$K1_f, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$imprterr) <- "If a man is suspected of planning a terrorist attack in [country], the police should have the power to keep him in prison until they are satisfied he was not involved"

pgsw2019$immweur <- dplyr::recode_factor(read_2019$K1_g, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$immweur) <- "Should Poland allow people from West European countries to come and live here?"

pgsw2019$immcee <- dplyr::recode_factor(read_2019$K1_h, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$immcee) <- "Should Poland allow people from East European countries to come and live here?"

pgsw2019$immnone <- dplyr::recode_factor(read_2019$K1_i, 
                                        `5` = "Definitely disagree", 
                                        `4` = "Rather disagree", 
                                        `3` = "Neither agree nor disagree", 
                                        `2` = "Rather agree", 
                                        `1` = "Definitely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$immnone) <- "Should Poland allow people from poor non-European countries to come and live here?"


pgsw2019$denounce <- dplyr::recode_factor(read_2019$K1_mx, 
                                         `5` = "Definitely disagree", 
                                         `4` = "Rather disagree", 
                                         `3` = "Neither agree nor disagree", 
                                         `2` = "Rather agree", 
                                         `1` = "Definitely agree",
                                         .ordered=TRUE)
var_label(pgsw2019$denounce) <- "One ought to denounce people who articulate critical opinions about selected facts from our history or criticize Polish politicians"

pgsw2019$suptpol <- dplyr::recode_factor(read_2019$K1_my, 
                                          `5` = "Definitely disagree", 
                                          `4` = "Rather disagree", 
                                          `3` = "Neither agree nor disagree", 
                                          `2` = "Rather agree", 
                                          `1` = "Definitely agree",
                                          .ordered=TRUE)
var_label(pgsw2019$suptpol) <- "The Polish government ought to be supported in the international arena even if it clearly is wrong"

pgsw2019$eubenef <- read_2019$NW1
pgsw2019$eubenef <- set_labels(pgsw2019$eubenef, labels = c("More benefits" = 1,
                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, 
                                                          "More dangers" = 7))
var_label(pgsw2019$eubenef) <- "Do you believe that over the next ten years Poland’s presence in the European Union will bring more benefits for Poland than dangers? "

pgsw2019$westbest <- dplyr::recode_factor(read_2019$NW2, 
                                         `4` = "Strongly disagree", 
                                         `3` = "Disagree", 
                                         `2` = "Agree", 
                                         `1` = "Strongly agree",
                                         .ordered=TRUE)
var_label(pgsw2019$westbest) <- "Western European states have their problems, but they are still a better model for Poland’s future growth than the model provided by states to the east"

pgsw2019$ladder_wealth <- read_2019$P67a
pgsw2019$ladder_wealth  <- set_labels(pgsw2019$ladder_wealth, labels = c("The poorest" = 1, 
                                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                                          "The wealthiest" = 10))
var_label(pgsw2019$ladder_wealth) <- "Where would you place yourself on each of the following “ladders”, where the lowest rung means a low social position, and the highest a high social position?"

pgsw2019$ladder_infl <- read_2019$P67b
pgsw2019$ladder_infl  <- set_labels(pgsw2019$ladder_infl, labels = c("Those with the least influence" = 1, 
                                                                          '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                                          "The most influential" = 10))
var_label(pgsw2019$ladder_infl) <- "Where would you place yourself on each of the following “ladders”, where the lowest rung means a low social position, and the highest a high social position?"

pgsw2019$ladder_resp <- read_2019$P67c
pgsw2019$ladder_resp  <- set_labels(pgsw2019$ladder_resp, labels = c("The least well-respected" = 1, 
                                                                      '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                                      "The most well-respected" = 10))
var_label(pgsw2019$ladder_resp) <- "Where would you place yourself on each of the following “ladders”, where the lowest rung means a low social position, and the highest a high social position?"

pgsw2019$ladder_chance <- read_2019$P67d
pgsw2019$ladder_chance  <- set_labels(pgsw2019$ladder_chance, labels = c("Those with the worst life chances" = 1, 
                                                                     '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                                     "Those with the best life chances" = 10))
var_label(pgsw2019$ladder_chance) <- "Where would you place yourself on each of the following “ladders”, where the lowest rung means a low social position, and the highest a high social position?"

pgsw2019$pchg_wealth <- dplyr::recode_factor(read_2019$P68_1, 
                                         `0` = "Significantly improved", 
                                         `1` = "Rather improved", 
                                         `2` = "Remained the same", 
                                         `3` = "Rather worsened", 
                                         `4` = "Significantly worsened",
                                         .ordered=TRUE)
var_label(pgsw2019$pchg_wealth) <- "Has your personal position on the ladder of wealth changed?"

pgsw2019$pchg_infl <- dplyr::recode_factor(read_2019$P68_2, 
                                             `0` = "Significantly improved", 
                                             `1` = "Rather improved", 
                                             `2` = "Remained the same", 
                                             `3` = "Rather worsened", 
                                             `4` = "Significantly worsened",
                                             .ordered=TRUE)
var_label(pgsw2019$pchg_infl) <- "Has your personal position on the ladder of influence changed?"

pgsw2019$pchg_resp <- dplyr::recode_factor(read_2019$P68_3, 
                                           `0` = "Significantly improved", 
                                           `1` = "Rather improved", 
                                           `2` = "Remained the same", 
                                           `3` = "Rather worsened", 
                                           `4` = "Significantly worsened",
                                           .ordered=TRUE)
var_label(pgsw2019$pchg_resp) <- "Has your personal position on the ladder of respect changed?"

pgsw2019$pchg_chance <- dplyr::recode_factor(read_2019$P68_4, 
                                           `0` = "Significantly improved", 
                                           `1` = "Rather improved", 
                                           `2` = "Remained the same", 
                                           `3` = "Rather worsened", 
                                           `4` = "Significantly worsened",
                                           .ordered=TRUE)
var_label(pgsw2019$pchg_chance) <- "Has your personal position on the ladder of life chances changed?"

#P69 not coded

pgsw2019$hapresult <- dplyr::recode_factor(read_2019$X1, 
                                             `1` = "The result was much better than I expected", 
                                             `2` = "The result was a bit better than I expected", 
                                             `3` = "The result was the same as I expected", 
                                             `4` = "The result was worse than I expected", 
                                             `5` = "The result was much worse than I expected")
var_label(pgsw2019$hapresult) <- "On the 13th of October there were elections to the Polish parliament. Bearing in mind the result of those elections for the electoral committee you voted for, to what extent are you happy with the result it obtained?"

pgsw2019$motivel <- dplyr::recode_factor(read_2019$X2, 
                                           `1` = "I did not take into consideration which party or candidate I was voting for, I simply cast a vote for a particular electoral committee", 
                                           `2` = "I voted above all for a specific candidate who happened to appear on a particular electoral list", 
                                           `3` = "I voted above all for a political party; and only to a lesser extent for an electoral committee or a specific candidate")
var_label(pgsw2019$motivel) <- "Which of the following corresponds most closely to the action you took when voting?"

pgsw2019$polknow_1 <- dplyr::recode_factor(read_2019$Y1, 
                                           `1` = "The parliament (Sejm)", 
                                           `2` = "The Monetary Policy Council", 
                                           `3` = "The president", 
                                           `4` = "The government", 
                                           `5` = "The National Bank of Poland")
var_label(pgsw2019$polknow_1) <- "The body responsible for preparing and implementing the budget is..."

pgsw2019$polknow_2 <- dplyr::recode_factor(read_2019$Y2, 
                                           `1` = "Half are chosen by the Sejm, and the other half are chosen by local government assemblies", 
                                           `2` = "They are chosen in general elections by all citizens who are 18 or older", 
                                           `3` = "They are designated by the prime minister", 
                                           `4` = "They are chosen in general elections by all citizens who are 25 or older", 
                                           `5` = "Don't know")
var_label(pgsw2019$polknow_2) <- "How are Polish members of the European Parliament elected?"

pgsw2019$polknow_3 <- dplyr::recode_factor(read_2019$Y3, 
                                           `1` = "Half are chosen by the Sejm, and the other half are chosen by local government assemblies", 
                                           `2` = "They are chosen in general elections by all citizens who are 18 or older", 
                                           `3` = "Don't know")
var_label(pgsw2019$polknow_3) <- "Does every person born on Polish territory automatically gain Polish citizenship, regardless of the citizenship of their parents?"

