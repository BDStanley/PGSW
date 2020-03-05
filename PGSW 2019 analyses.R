load("PGSW2019.RData")
library(tidyverse); library(easystats); library(ggeffects); library(rio); library(scales); library(sjlabelled); library(labelled); library(sjPlot); library(report)
library(FactoMineR); library(psych); library(nnet)

#Polychoric factor analysis of populism and simplism variables
#data preparation
S1_1 <- as.numeric(unlist(pgsw2019$simp_1)) %>%
  set_label("Politycy zamiast skutecznie działać ciągle gmatwają proste sprawy.")
S1_2 <- as.numeric(unlist(pgsw2019$simp_2)) %>%
  set_label("Naukowcy i eksperci różnych dziedzin zbytnio komplikują sprawy, które są dość proste.")
S1_3 <- as.numeric(unlist(pgsw2019$simp_3)) %>%
  set_label("Obecnie medycyna zamiast pomagać tylko mąci ludziom w głowach; by być zdrowym wystarczy żyć naturalnie i słuchać ludzi podobnych do nas.")
S1_4 <- as.numeric(unlist(pgsw2019$simp_4)) %>%
  set_label("Rozwiązanie problemów naszego kraju jest bardzo prostą rzeczą, trzeba tylko dać władze tym, którzy będą chcieli tego dokonać.")
S1_5 <- as.numeric(unlist(pgsw2019$simp_5)) %>%
  set_label("Prace parlamentu to niekończące się debatowanie nad szczegółami ustaw, a tymczasem regulacje niektórych spraw są tak proste, że każdy z nas mógłby je rozwiązać.")
S1_6 <- as.numeric(unlist(pgsw2019$simp_6)) %>%
  set_label("W polityce toczy się ostra walka między dobrem, reprezentowanym przez zwykłych ludzi, a złem – wynikającym z działań często skorumpowanych elit.")

P12_1 <- as.numeric(unlist(pgsw2019$neword)) %>%
  set_label("Polsce potrzebny jest ktoś, kto będzie miał dość siły, by zmienić całkowicie nasz system władzy i zaprowadzić nowy, sprawiedliwy ład i porządek.")
P12_2 <- as.numeric(unlist(pgsw2019$ordpeop)) %>%
  set_label("W Polsce nieliczni zawłaszczają władzę, przynależną zwykłym ludziom.")
P12_3 <- as.numeric(unlist(pgsw2019$crisis)) %>%
  set_label("Jest ostatnia chwila, by uchronić Polskę przed grożącą katastrofą.")
P12_4 <- as.numeric(unlist(pgsw2019$morals)) %>%
  set_label("W Polsce nic się nie poprawi, dopóki politycy nie powrócą do starych, dobrych wartości moralnych.")
P12_5 <- as.numeric(unlist(pgsw2019$simple)) %>%
  set_label("Rozwiązanie problemów naszego kraju jest bardzo prostą rzeczą, trzeba tylko dać władzę tym, którzy będą chcieli tego dokonać.")
P12_6 <- as.numeric(unlist(pgsw2019$goodev)) %>%
  set_label("Wszystko w polityce jest jednoznacznie dobre lub złe, wybór jest jasny.")
P23 <- as.numeric(unlist(pgsw2019$stateval)) %>%
  set_label("Która z dwóch wizji jest Panu/Pani bliższe?") %>%
  set_labels(labels=c("Państwo, które chroni i wspiera tradycyjne wartości" = 1, "Państwo, które wspiera postęp społeczny i nowoczesność" = 2))
P24 <- as.numeric(unlist(pgsw2019$statesol)) %>%
  set_label("Która z dwóch wizji jest Panu/Pani bliższe?") %>%
  set_labels(labels=c("Państwo, które tworzy podstawy do solidarności społecznej" = 1, "Państwo, które stwarza korzystne warunki dla przedsiębiorczości ludzi" = 2))


weight <- as.numeric(unlist(pgsw2019$weight))
popsimp <- data.frame(S1_1, S1_2, S1_3, S1_4, S1_5, S1_6,
                      P12_1, P12_2, P12_3, P12_4, P12_5, P12_6,
                      P23, P24)
varlab <- get_label(popsimp)

mvn(popsimp)
KMO(popsimp)
cortest.bartlett(popsimp)
scree <- scree(popsimp)
parallel <- fa.parallel(popsimp, fm = "pa", fa="both", cor="poly", correct=0)

#factor analysis
poly_model <- fa(popsimp, nfactor=4, cor="poly", fm="ml", rotate = "varimax", correct=0, n.obs=2003, weight=weight)

#interpretation and presentation of results
poly_model_res <- parameters(poly_model, threshold=0.3, labels=varlab)
factor.groups <- sjt.fa(popsimp, nmbr.fctr = 4)$factor.index
item_analysis <- tab_itemscale(popsimp, factor.groups)
labs <- get_labels(pgsw2019$simp_1)
likert_plot <- plot_likert(popsimp, groups=factor.groups, catcount=5, 
                           sort.frq="pos.asc", reverse.scale = TRUE,
                           legend.labels = labs)

#factor scores
scores <- predict(poly_model, data=popsimp)
pgsw2019$simplism <- scales::rescale(scores[,1], c(0,1))
pgsw2019$simplism <- set_labels(pgsw2019$simplism, labels = c("Low" = 0, "High" = 1))
var_label(pgsw2019$simplism) <- "Level of simplism"
pgsw2019$populism <- scales::rescale(scores[,2], c(0,1))
pgsw2019$populism <- set_labels(pgsw2019$populism, labels = c("Low" = 0, "High" = 1))
var_label(pgsw2019$populism) <- "Level of populism"

#example models
model_reg <- lm(populism ~ simplism, data=pgsw2019, weight=weight)
marg_reg <- ggemmeans(model_reg, terms=c("simplism"))

model_mnl <- multinom(votefor ~ simplism + populism, data=pgsw2019, weight=weight)
marg_mnl <- ggemmeans(model_mnl, terms=c("populism", "simplism"))