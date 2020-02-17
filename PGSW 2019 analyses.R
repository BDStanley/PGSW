load("PGSW2019.RData")
library(tidyverse); library(psych); library(statar); library(scales); library(nnet); library(sjlabelled); library(labelled); library(ggeffects)

#Polychoric factor analysis of populism and simplism variables
simp_1 <- as.numeric(unlist(pgsw2019$simp_1))
simp_2 <- as.numeric(unlist(pgsw2019$simp_2))
simp_3 <- as.numeric(unlist(pgsw2019$simp_3))
simp_4 <- as.numeric(unlist(pgsw2019$simp_4))
simp_5 <- as.numeric(unlist(pgsw2019$simp_5))
simp_6 <- as.numeric(unlist(pgsw2019$simp_6))
pop_1 <- as.numeric(unlist(pgsw2019$neword))
pop_2 <- as.numeric(unlist(pgsw2019$ordpeop))
pop_3 <- as.numeric(unlist(pgsw2019$crisis))
pop_4 <- as.numeric(unlist(pgsw2019$morals))
pop_5 <- as.numeric(unlist(pgsw2019$simple))
pop_6 <- as.numeric(unlist(pgsw2019$gender))

weight <- as.numeric(unlist(pgsw2019$weight))

popsimp <- data.frame(simp_1, simp_2, simp_3, simp_4, simp_5, simp_6,
                      pop_1, pop_2, pop_3, pop_4, pop_5, pop_6)

popsimp_poly <- polychoric(popsimp)
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
poly_model <- fa(popsimp, nfactor=2, cor="poly", fm="mle", rotate = "varimax", correct=0, n.obs=2003)
scores <- predict(poly_model, data=popsimp)
pgsw2019$simplism <- rescale(scores[,1], c(0,1))
pgsw2019$simplism <- set_labels(pgsw2019$simplism, labels = c("Low" = 0, "High" = 1))
var_label(pgsw2019$simplism) <- "Level of simplism"
pgsw2019$populism <- rescale(scores[,2], c(0,1))
pgsw2019$populism <- set_labels(pgsw2019$populism, labels = c("Low" = 0, "High" = 1))
var_label(pgsw2019$populism) <- "Level of populism"

model <- multinom(votefor ~ simplism*populism, data=pgsw2019, weight=weight)
marg <- ggemmeans(model, terms=c("populism", "simplism"))