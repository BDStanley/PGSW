#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id("https://drive.google.com/file/d/1rHjST-HBIgjEHxO4qE8dLeZiGqjLG5HN/view?usp=sharing"), overwrite=TRUE)
read <- read_spss('PGSW 2007.sav')
pgsw2007 <- tibble(1:1817)

#####General variables#####
colnames(pgsw2007) <- "n"
var_label(pgsw2007$n) <- "ID number"

pgsw2007$year <- 2007
pgsw2007$year <- as_factor(pgsw2007$year)
var_label(pgsw2007$year) <- "Year of election"

pgsw2007$weight <- read$waga
var_label(pgsw2007$weight) <- "Weight"

#####Voting behaviour#####
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
pgsw2007$likePiS <- add_labels(pgsw2007$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likePiS) <- "Feeling toward PiS"  

pgsw2007$likePO <- dplyr::recode(read$c14b, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePO <- add_labels(pgsw2007$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likePO) <- "Feeling toward PO" 

pgsw2007$likePSL <- dplyr::recode(read$c14c, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likePSL <- add_labels(pgsw2007$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likePSL) <- "Feeling toward PSL" 

pgsw2007$likeLiD <- dplyr::recode(read$c14d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$likeLiD <- add_labels(pgsw2007$likeLiD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2007$likeLiD) <- "Feeling toward LiD" 

#####2007: Values and attitudes#####
pgsw2007$leftrt <- dplyr::recode(read$c17, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$leftrt <- add_labels(pgsw2007$leftrt, labels = c("Left" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Right" = 10))
var_label(pgsw2007$leftrt) <- "Left-right self-placement"

pgsw2007$euinteg <- dplyr::recode(read$p49f, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$euinteg <- add_labels(pgsw2007$euinteg, labels = c("Anti-integration" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Pro-integration" = 10))
var_label(pgsw2007$euinteg) <- "European integration"

pgsw2007$taxreg <- dplyr::recode(read$p49e, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$taxreg <- add_labels(pgsw2007$taxreg, labels = c("Progressive tax regime" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                          "Flat tax regime" = 10))
var_label(pgsw2007$taxreg) <- "Tax regime" 

pgsw2007$unemp <- dplyr::recode(read$p49d, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2007$unemp <- add_labels(pgsw2007$unemp, labels = c("Employment should be an absolute policy priority" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "Employment is less important than other policy domains" = 10))
var_label(pgsw2007$unemp) <- "Unemployment" 

pgsw2007$private <- dplyr::recode(read$p49b, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                  `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$private <- add_labels(pgsw2007$private, labels = c("A significant number of enterprises should remain in state hands" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                            "All state-owned enterprises should be privatised" = 10))
var_label(pgsw2007$private) <- "Privatisation" 

pgsw2007$abort <- dplyr::recode(read$p49i, `0` = 10L, `1` = 9L, `2` = 8L, `3` = 7L, `4` = 6L, `5` = 5L, 
                                `6` = 4L, `7` = 3L, `8` = 2L, `9` = 1L, `10` = 0L)
pgsw2007$abort <- add_labels(pgsw2007$abort, labels = c("There should be no right to abortion" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, '7' = 7, '8' = 8, '9' = 9,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 10))
var_label(pgsw2007$abort) <- "Abortion" 

pgsw2007$crime <- dplyr::recode(read$p49a, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
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
pgsw2007$age<- remove_all_labels(pgsw2007$age)
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
  mutate(union=replace(union, read$m38==4, "No")) %>%
  mutate(union=replace(union, read$m38 %in% c(1:3), "Yes")) %>%
  as_factor(union)
pgsw2007$union <- fct_relevel(pgsw2007$union, "No", "Yes")
var_label(pgsw2007$union) <- "Union membership"

#Create codebook
sjPlot::view_df(pgsw2007, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2007.RData")
