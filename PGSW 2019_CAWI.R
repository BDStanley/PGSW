#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive); library(MVN); library(psych)

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

#Test populism and simplism scales
S1 <- read$S1a %>%
  set_label("Politycy zamiast skutecznie działać ciągle gmatwają proste sprawy.")
S2 <- read$S1b %>%
  set_label("Naukowcy i eksperci różnych dziedzin zbytnio komplikują sprawy, które są dość proste.")
S3 <- read$S1c %>%
  set_label("Obecnie medycyna zamiast pomagać tylko mąci ludziom w głowach; by być zdrowym wystarczy żyć naturalnie i słuchać ludzi podobnych do nas.")
S4 <- read$S1d %>%
  set_label("Rozwiązanie problemów naszego kraju jest bardzo prostą rzeczą, trzeba tylko dać władze tym, którzy będą chcieli tego dokonać.")
S5 <- read$S1e %>%
  set_label("Prace parlamentu to niekończące się debatowanie nad szczegółami ustaw, a tymczasem regulacje niektórych spraw są tak proste, że każdy z nas mógłby je rozwiązać.")
S6 <- read$S1f %>%
  set_label("W polityce toczy się ostra walka między dobrem, reprezentowanym przez zwykłych ludzi, a złem – wynikającym z działań często skorumpowanych elit.")
P1 <- read$Q23a %>%
  set_label("Politycy powinni zawsze uważnie słuchać problemów ludzi.")
P2 <- 8-read$Q23b %>%
  set_label("Politycy nie muszą spędzać czasu wśród zwykłych ludzi, aby wykonywać dobrą robotę. [Pytanie odwrócone]")
P3 <- read$Q23c %>%
  set_label("Wola ludzi powinna być najwyższą zasadą w polityce tego kraju.")
P4 <- read$Q23d %>%
  set_label("Rząd jest w dużym stopniu zarządzany przez kilka dużych grup interesu, które szukają własnych korzyści.")
P5 <- 8-read$Q23e %>%
  set_label("Urzędnicy państwowi korzystają ze swojej władzy, aby starać się poprawić jakość życia ludzi.")
P6 <- read$Q23f %>%
  set_label("Całkiem sporo członków rządu to oszuści")
P7 <- read$Q23g %>%
  set_label("Można stwierdzić czy ktoś jest dobrą czy złą osobą, znając jej poglądy polityczne.")
P8 <- 8-read$Q23h %>%
  set_label("Ludzie, z którymi nie zgadzam się w kwestiach politycznych, nie są źli. [Pytanie odwrócone]")
P9 <- 8-read$Q23i %>%
  set_label("Ludzie, z którymi nie zgadzam się w kwestiach politycznych, są po prostu źle poinformowani. [Pytanie odwrócone]")
P10 <- read$Q22 %>%
  set_label("Polska jest podzielona na zwykłych ludzi i skorumpowane elity, które ich wykorzystują.")
weight <- as.numeric(unlist(pgsw2019$weight))
popsimp <- data.frame(S1, S2, S3, S4, S5, S6, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)
labs <- get_label(popsimp)

mvn(popsimp)
KMO(popsimp)
cortest.bartlett(popsimp)
scree <- scree(popsimp, main="Scree plot populism / simplism: CAWI")
parallel <- fa.parallel(popsimp, fm = "pa", fa="both", cor="poly", correct=0)
poly_model <- fa(popsimp, nfactor=5, cor="poly", fm="pa", correct=0, rotate = "oblimin")
poly_model_res <- parameters(poly_model, threshold=0.3, sort=TRUE, labels = labs)

simplism <- data.frame(S1, S2, S3, S4, S5, S6)
item_analysis_simplism <- tab_itemscale(simplism)
likert_plot_simplism <- plot_likert(simplism, catcount=5, 
                           sort.frq="pos.asc", reverse.scale = TRUE)

populism <- data.frame(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)
item_analysis_populism <- tab_itemscale(populism)
likert_plot_populism <- plot_likert(populism, catcount=7, 
                           sort.frq="pos.asc", reverse.scale = TRUE)


#Save data as R image
save.image(file = "PGSW2019.RData")
write_stata(pgsw2019, path='/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW2019/PGSW_2019_CAWI_2.dta', version=14)
