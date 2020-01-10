#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id("https://drive.google.com/file/d/1Y43Uyv5tSvlWij7N3OklHAo6xY-zNA4R/view?usp=sharing"), overwrite=TRUE)
read <- read_spss('PGSW 2011.sav')
pgsw2011 <- tibble(1:1919)

#####General variables#####
colnames(pgsw2011) <- "n"
var_label(pgsw2011$n) <- "ID number"

pgsw2011$year <- 2011
pgsw2011$year <- as_factor(pgsw2011$year)
var_label(pgsw2011$year) <- "Year of election"

pgsw2011$weight <- read$waga
var_label(pgsw2011$weight) <- "Weight"

#####Voting behaviour#####
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
pgsw2011$likePiS <- add_labels(pgsw2011$likePiS, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likePiS) <- "Feeling toward PiS"  

pgsw2011$likePO <- dplyr::recode(read$C12B, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePO <- add_labels(pgsw2011$likePO, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likePO) <- "Feeling toward PO" 

pgsw2011$likePSL <- dplyr::recode(read$C12C, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likePSL <- add_labels(pgsw2011$likePSL, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likePSL) <- "Feeling toward PSL" 

pgsw2011$likeSLD <- dplyr::recode(read$C12D, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likeSLD <- add_labels(pgsw2011$likeSLD, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likeSLD) <- "Feeling toward SLD" 

pgsw2011$likeRP <- dplyr::recode(read$C12E, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$likeRP <- add_labels(pgsw2011$likeRP, labels = c("Dislike" = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, '7' = 7, '8' = 8, '9' = 9, "Like" = 10))
var_label(pgsw2011$likeRP) <- "Feeling toward RP" 

#####Values and attitudes#####
pgsw2011$leftrt <- dplyr::recode(read$C15, `0` = 0L, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                 `6` = 6L, `7` = 7L, `8` = 8L, `9` = 9L, `10` = 10L)
pgsw2011$leftrt <- add_labels(pgsw2011$leftrt, labels = c("Left" = 0, "Right" = 10))
var_label(pgsw2011$leftrt) <- "Left-right self-placement"  

pgsw2011$euinteg <- dplyr::recode(read$PI36E, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, 
                                  `6` = 6L, `7` = 7L)
pgsw2011$euinteg <- add_labels(pgsw2011$euinteg, labels = c("Anti-integration" = 1, '2' = 2, '3' = 3, '4' = 4, "Pro-integration" = 5))
var_label(pgsw2011$euinteg) <- "European integration"   

pgsw2011$chstdiv <- dplyr::recode(read$PI36B, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$chstdiv <- add_labels(pgsw2011$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "The Church should be completely separate from the state" = 7))
var_label(pgsw2011$chstdiv) <- "Church/state divide"                                                           

pgsw2011$taxreg <- dplyr::recode(read$PI36D, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$taxreg <- add_labels(pgsw2011$taxreg, labels = c("Progressive tax regime" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "Flat tax regime" = 7))
var_label(pgsw2011$taxreg) <- "Tax regime"                                                           

pgsw2011$unemp <- dplyr::recode(read$PI36C, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$unemp <- add_labels(pgsw2011$unemp, labels = c("Employment should be an absolute policy priority" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                        '6' = 6, "Employment is less important than other policy domains" = 7))
var_label(pgsw2011$unemp) <- "Unemployment"  

pgsw2011$socpol <- dplyr::recode(read$PI36G, `1` = 1L, `2` = 2L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 7L)
pgsw2011$socpol <- add_labels(pgsw2011$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                          '6' = 6, "People should take care of their own welfare" = 7))
var_label(pgsw2011$socpol) <- "Social policy"  

pgsw2011$private <- dplyr::recode(read$PI36A, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
pgsw2011$private <- add_labels(pgsw2011$private, labels = c("A significant number of enterprises should remain in state hands" = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5,
                                                            '6' = 6, "All state-owned enterprises should be privatised" = 7))
var_label(pgsw2011$private) <- "Privatisation"  

pgsw2011$abort <- dplyr::recode(read$PI36H, `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L, `5` = 3L, `6` = 2L, `7` = 1L)
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
  mutate(union=replace(union, read$M38==5, "No")) %>%
  mutate(union=replace(union, read$M38 %in% c(1:4), "Yes")) %>%
  as_factor(union)
pgsw2011$union <- fct_relevel(pgsw2011$union, "No", "Yes")
var_label(pgsw2011$union) <- "Union membership"

#Create codebook
sjPlot::view_df(pgsw2011, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

#Save data as R image
save.image(file = "PGSW2011.RData")

