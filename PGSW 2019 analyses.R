load("PGSW2019.RData")
library(tidyverse); library(easystats); library(ggeffects); library(rio); library(scales); library(sjlabelled); library(labelled); library(sjPlot); library(report)
library(FactoMineR); library(psych); library(nnet)

#Polychoric factor analysis of populism and simplism variables
#data preparation
S1_1 <- as.numeric(unlist(pgsw2019$simp_1))
S1_2 <- as.numeric(unlist(pgsw2019$simp_2))
S1_3 <- as.numeric(unlist(pgsw2019$simp_3))
S1_4 <- as.numeric(unlist(pgsw2019$simp_4))
S1_5 <- as.numeric(unlist(pgsw2019$simp_5))
S1_6 <- as.numeric(unlist(pgsw2019$simp_6))
P19_1 <- as.numeric(unlist(pgsw2019$neword))
P19_2 <- as.numeric(unlist(pgsw2019$ordpeop))
P19_3 <- as.numeric(unlist(pgsw2019$crisis))
P19_4 <- as.numeric(unlist(pgsw2019$morals))
P19_5 <- as.numeric(unlist(pgsw2019$simple))
P19_6 <- as.numeric(unlist(pgsw2019$goodev))
weight <- as.numeric(unlist(pgsw2019$weight))
popsimp <- data.frame(S1_1, S1_2, S1_3, S1_4, S1_5, S1_6,
                      P19_1, P19_2, P19_3, P19_4, P19_5, P19_6)

#factor analysis
poly_model <- fa(popsimp, nfactor=2, cor="poly", fm="ml", rotate = "varimax", correct=0, n.obs=2003, weight=weight)

#interpretation and presentation of results
poly_model_res <- parameters(poly_model, threshold=0.3, sort=TRUE)
factor.groups <- sjt.pca(popsimp, nmbr.fctr = 2)$factor.index
item_analysis <- tab_itemscale(popsimp, factor.groups)
labs <- get_labels(pgsw2019$simp_1)
likert_plot <- plot_likert(popsimp, groups=factor.groups, catcount=5, 
                           sort.frq="pos.asc", reverse.scale = TRUE, 
                           groups.titles = c("Simplism", "Populism"),
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
marg_mnl <- ggemmeans(model, terms=c("populism", "simplism"))