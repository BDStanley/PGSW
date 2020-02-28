#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id('https://drive.google.com/file/d/1Nn3SiYZmk0XLlFFVUammCtkOyX-ccZ_6/view?usp=sharing'), overwrite=TRUE)
1
read <- read_spss('PGSW2019_CAWI_2.sav')
pgsw2019 <- tibble(1:1500)

colnames(pgsw2019) <- "n"
var_label(pgsw2019$n) <- "ID number"

pgsw2019$year <- 2019
pgsw2019$year <- as_factor(pgsw2019$year)
var_label(pgsw2019$year) <- "Year of election"

pgsw2019$weight <- read$waga
var_label(pgsw2019$weight) <- "Population weight"

#Lewis populism variable
pgsw2019$poplew <- dplyr::recode_factor(read$Q22, 
                                        `1` = "Completely disagree", 
                                        `2` = "2", 
                                        `3` = "3", 
                                        `4` = "4",
                                        `5` = "5",
                                        `6` = "6",
                                        `7` = "Completely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$poplew) <- "Poland is divided between the ordinary people and the corrupt elite that exploits them"

#Castanho Silva / Littvay populism scale
pgsw2019$pop_1 <- dplyr::recode_factor(read$Q23a, 
                                        `1` = "Completely disagree", 
                                        `2` = "2", 
                                        `3` = "3", 
                                        `4` = "4",
                                        `5` = "5",
                                        `6` = "6",
                                        `7` = "Completely agree",
                                        .ordered=TRUE)
var_label(pgsw2019$pop_1) <- "Politycy powinni zawsze uważnie słuchać problemów ludzi."

pgsw2019$pop_2 <- dplyr::recode_factor(read$Q23b, 
                                       `7` = "Completely agree", 
                                       `6` = "2", 
                                       `5` = "3", 
                                       `4` = "4",
                                       `3` = "5",
                                       `2` = "6",
                                       `1` = "Completely disagree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_2) <- "Politycy nie muszą spędzać czasu wśród zwykłych ludzi, aby wykonywać dobrą robotę."

pgsw2019$pop_3 <- dplyr::recode_factor(read$Q23c, 
                                       `1` = "Completely disagree", 
                                       `2` = "2", 
                                       `3` = "3", 
                                       `4` = "4",
                                       `5` = "5",
                                       `6` = "6",
                                       `7` = "Completely agree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_3) <- "Wola ludzi powinna być najwyższą zasadą w polityce tego kraju."

pgsw2019$pop_4 <- dplyr::recode_factor(read$Q23d, 
                                       `1` = "Completely disagree", 
                                       `2` = "2", 
                                       `3` = "3", 
                                       `4` = "4",
                                       `5` = "5",
                                       `6` = "6",
                                       `7` = "Completely agree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_4) <- "Rząd jest w dużym stopniu zarządzany przez kilka dużych grup interesu, które szukają własnych korzyści."

pgsw2019$pop_5 <- dplyr::recode_factor(read$Q23e, 
                                       `7` = "Completely agree", 
                                       `6` = "2", 
                                       `5` = "3", 
                                       `4` = "4",
                                       `3` = "5",
                                       `2` = "6",
                                       `1` = "Completely disagree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_5) <- "Urzędnicy państwowi korzystają ze swojej władzy, aby starać się poprawić jakość życia ludzi.."

pgsw2019$pop_6 <- dplyr::recode_factor(read$Q23f, 
                                       `1` = "Completely disagree", 
                                       `2` = "2", 
                                       `3` = "3", 
                                       `4` = "4",
                                       `5` = "5",
                                       `6` = "6",
                                       `7` = "Completely agree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_6) <- "Całkiem sporo członków rządu to oszuści."

pgsw2019$pop_7 <- dplyr::recode_factor(read$Q23g, 
                                       `1` = "Completely disagree", 
                                       `2` = "2", 
                                       `3` = "3", 
                                       `4` = "4",
                                       `5` = "5",
                                       `6` = "6",
                                       `7` = "Completely agree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_7) <- "Można stwierdzić czy ktoś jest dobrą czy złą osobą, znając jej poglądy polityczne."

pgsw2019$pop_8 <- dplyr::recode_factor(read$Q23h, 
                                       `7` = "Completely agree", 
                                       `6` = "2", 
                                       `5` = "3", 
                                       `4` = "4",
                                       `3` = "5",
                                       `2` = "6",
                                       `1` = "Completely disagree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_8) <- "Ludzie, z którymi nie zgadzam się w kwestiach politycznych, nie są źli."

pgsw2019$pop_9 <- dplyr::recode_factor(read$Q23i, 
                                       `7` = "Completely agree", 
                                       `6` = "2", 
                                       `5` = "3", 
                                       `4` = "4",
                                       `3` = "5",
                                       `2` = "6",
                                       `1` = "Completely disagree",
                                       .ordered=TRUE)
var_label(pgsw2019$pop_9) <- "Ludzie, z którymi nie zgadzam się w kwestiach politycznych, są po prostu źle poinformowani."


HS.model <- ' populism  =~ pop_1 + pop_2 + pop_3 + pop_4 + pop_5 + pop_6 + pop_7 + pop_8 + pop_9 '

fit <- cfa(HS.model, data=pgsw2019)

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  pgsw2019[idx, fs] <- fscores[ , fs]
}

pgsw2019$populism <- scales::rescale(pgsw2019$populism, c(0,1))
var_label(pgsw2019$populism) <- "Index of populism"

#Save data as R image
save.image(file = "PGSW2019.RData")
write_stata(pgsw2019, path='/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW2019/PGSW_2019_CAWI_2.dta', version=14)
