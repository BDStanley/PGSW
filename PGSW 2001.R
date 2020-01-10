#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id("https://drive.google.com/file/d/1TWrkWhPwbVLGHm3CPcOsxMPygcxWyC_0/view?usp=sharing"), overwrite=TRUE)
read <- read_spss('PGSW 2001.sav')
pgsw2001 <- tibble(1:1794)

#####General variables#####
colnames(pgsw2001) <- "n"
var_label(pgsw2001$n) <- "ID number"

pgsw2001$year <- 2001
pgsw2001$year <- as_factor(pgsw2001$year)
var_label(pgsw2001$year) <- "Year of election"

pgsw2001$weight <- read$wghtsamp
var_label(pgsw2001$weight) <- "Weight"

#####Voting behaviour#####
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
pgsw2001$likeSLD  <- add_labels(pgsw2001$likeSLD , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeSLD) <- "Feeling toward SLD"  

pgsw2001$likePSL <- dplyr::recode(read$likps057, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likePSL  <- add_labels(pgsw2001$likePSL , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likePSL) <- "Feeling toward PSL"  

pgsw2001$likeUW <- dplyr::recode(read$likuw054, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeUW  <- add_labels(pgsw2001$likeUW , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeUW) <- "Feeling toward UW"  

pgsw2001$likeSRP <- dplyr::recode(read$liksa055, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeSRP  <- add_labels(pgsw2001$likeSRP , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeSRP) <- "Feeling toward SRP"  

pgsw2001$likeAWS <- dplyr::recode(read$likaw053, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeAWS  <- add_labels(pgsw2001$likeAWS , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeAWS) <- "Feeling toward AWS"  

pgsw2001$likeLPR <- dplyr::recode(read$liklp059, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likeLPR  <- add_labels(pgsw2001$likeLPR , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                              '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likeLPR) <- "Feeling toward LPR"

pgsw2001$likePO <- dplyr::recode(read$likpo058, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$likePO  <- add_labels(pgsw2001$likePO , labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2001$likePO) <- "Feeling toward PO"

#####Values and attitudes#####
pgsw2001$leftrt <- dplyr::recode(read$lrslf073, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$leftrt <- add_labels(pgsw2001$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2001$leftrt) <- "Left-right self-placement"

pgsw2001$euinteg <- dplyr::recode(read$p_eu0101, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$euinteg <- add_labels(pgsw2001$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2001$euinteg) <- "European integration"

pgsw2001$chstdiv <- dplyr::recode(read$p_rel097, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$chstdiv <- add_labels(pgsw2001$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "The Church should be completely separate from the state" = 10))
var_label(pgsw2001$chstdiv) <- "Church/state divide" 

pgsw2001$taxreg <- dplyr::recode(read$p_tax100, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$taxreg <- add_labels(pgsw2001$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Flat tax regime" = 10))
var_label(pgsw2001$taxreg) <- "Tax regime" 

pgsw2001$socpol <- dplyr::recode(read$p_soc103, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$socpol <- add_labels(pgsw2001$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "People should take care of their own welfare" = 10))
var_label(pgsw2001$socpol) <- "Social policy" 

pgsw2001$unemp <- dplyr::recode(read$p_une099, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2001$unemp <- add_labels(pgsw2001$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw2001$unemp) <- "Unemployment" 

pgsw2001$private <- dplyr::recode(read$p_prv096, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2001$private <- add_labels(pgsw2001$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw2001$private) <- "Privatisation" 

pgsw2001$crime <- dplyr::recode(read$p_crm095, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
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

#####Socio-demographic variables#####
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
  mutate(union=replace(union, read$tum01357==4, "No")) %>%
  mutate(union=replace(union, read$tum01357 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2001$union <- fct_relevel(pgsw2001$union, "No", "Yes")
var_label(pgsw2001$union) <- "Union membership"

#Create codebook
sjPlot::view_df(pgsw2001, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2001.RData")

