library(psych)

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
poly_model <- fa(popsimp, nfactor=2, cor="poly", fm="mle", rotate = "varimax")

polframe <- data.frame(predictions, pgsw2019$votefor, pgsw2019$weight)

model <- multinom(pgsw2019.votefor ~ ML1*ML2, data=polframe, weight=pgsw2019.weight)