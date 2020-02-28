load("PGSW2019.RData")
library(tidyverse); library(psych); library(statar); library(scales); library(nnet); library(sjlabelled); 
library(labelled); library(ggeffects); library(FactoMineR)

#Polychoric factor analysis of populism and simplism variables
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

popsimp_poly <- polychoric(popsimp)
parallel <- fa.parallel(popsimp_poly$rho, cor="poly", fm="minres", fa="fa", main = "Scree Plot")
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]
sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')
eigendat = rbind(obs,sim)

apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  #Add lines connecting data points
  geom_line()+
  #Add the data points.
  geom_point(size=4)+
  #Label the y-axis 'Eigenvalue'
  scale_y_continuous(name='Eigenvalue')+
  #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
  scale_shape_manual(values=c(16,1)) +
  #Add vertical line indicating parallel analysis suggested max # of factors to retain
  geom_vline(xintercept = parallel$nfact, linetype = 'dashed')+
  #Apply our apa-formatting theme
  apatheme


plot_pca <- PCA(popsimp)
poly_model <- fa(popsimp, nfactor=2, cor="poly", fm="minres", rotate = "varimax", correct=0, n.obs=2003, weight=weight)

scores <- predict(poly_model, data=popsimp)
pgsw2019$simplism <- scales::rescale(scores[,1], c(0,1))
pgsw2019$simplism <- set_labels(pgsw2019$simplism, labels = c("Low" = 0, "High" = 1))
var_label(pgsw2019$simplism) <- "Level of simplism"
pgsw2019$populism <- scales::rescale(scores[,2], c(0,1))
pgsw2019$populism <- set_labels(pgsw2019$populism, labels = c("Low" = 0, "High" = 1))
var_label(pgsw2019$populism) <- "Level of populism"

model_reg <- lm(simplism ~ populism, data=pgsw2019, weight=weight)
marg_reg <- ggemmeans(model_reg, terms=c("populism"))

model <- multinom(votefor ~ simplism + populism, data=pgsw2019, weight=weight)
marg <- ggemmeans(model, terms=c("populism", "simplism"))