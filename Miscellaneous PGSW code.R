#depmixS4 code for calculating SES
mod <- mix(list(edlevel~1, hincq~1, occup~1), data=pgsw1997, nstates=3,
           family=list(multinomial("logit"), multinomial("logit"), multinomial("logit")),
           respstart=runif(9))
set.seed(780045)
ses <- fit(mod, weight=weight, emcontrol=em.control(maxit=1000))
posterior.states <- depmixS4::posterior(ses)
posterior.states$state <- as.factor(posterior.states$state)
pgsw1997$ses <- posterior.states$state
pgsw1997$ses <- fct_recode(pgsw1997$ses, "Low"="2", "Medium"="1", "High"="3")
pgsw1997$ses <- fct_relevel(pgsw1997$ses, "Low", "Medium", "High")
var_label(pgsw1997$ses) <- "Socio-economic status"


read <- read_spss('/Users/benstanley/Google Drive/Resources/Datasets/Poland/pgsw2019/PGSW2019_CAWI_wave_1.sav')
read <- read_spss('~/Desktop/Parallels Shared Folders/Google Drive/Resources/Datasets/Poland/pgsw2019/PGSW2019_CAWI_wave_1.sav')
read$populism <- as.ordered(read$P26)
read$vote <- as.factor(read$P2_sejm)
read$leftrt <- as.ordered(read$P14a)
read$relig <- as.ordered(read$P47)
read$weight <- as.numeric(read$waga)
read$wiek <- as.numeric(read$wiek)
sjPlot::view_df(read, show.frq=TRUE, show.prc=TRUE, weight.by="waga", show.wtd.frq=TRUE, show.wtd.prc=TRUE, max.len=50, use.viewer=FALSE)

fit <- brm(ordered(populism) | weights(waga) ~ vote, 
           data=read, family=cumulative(), save_all_pars=TRUE)

fit_mo <- brm(as.factor(vote) | weights(waga) ~ mo(relig), 
              data=read, family=categorical(), save_all_pars=TRUE)

HS.model <- ' simplism  =~ S1a + S1b + S1c + S1d + S1e + S1f '

read$listprob <- read$Q23a
read$amongp <- 8-read$Q23b
read$willp <- read$Q23c
read$intgrp <- read$Q23d
read$admins <- 8-read$Q23e
read$cheats <- read$Q23f
read$goodbad <- read$Q23g
read$disevil <- 8-read$Q23h
read$disinfo <- 8-read$Q23i

fit <- cfa(HS.model, data=read, ordered=c("S1a", "S1b", "S1c", "S1d", "S1e",
                                          "S1f"))

idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  read[idx, fs] <- fscores[ , fs]
}


#Monotonic independent variable
fit_mo_1 <- brm(as.factor(votefor) | weights(weight) ~ mo(lgbt) + mo(socwelf), 
                data=pgsw2019, family=categorical(), save_all_pars=TRUE)

marg <- ggpredict(fit_mo, terms="socwelf")
plotdata <- marg
plotdata$x <- fct_inorder(plotdata$x, ordered=TRUE)
pdat <- melt(plotdata, measure.vars=c("x"))

cols <- c("PiS"="blue4", "KO"="orange", "PSL-Kukiz"="darkgreen", "Konfederacja" = "black", "Lewica" = "red", "Did not vote"="gray50")

p <- ggplot() +
  geom_linerange(pdat, mapping=aes(x=fct_inorder(value), ymin=conf.low, ymax=conf.high, color=response.level), 
                 position = position_dodge2(width = 0.5)) +
  geom_point(pdat, mapping=aes(x=fct_inorder(value), y=predicted, color=response.level), 
             size=3, position = position_dodge2(width = 0.5)) +
  scale_colour_manual(name="", values=cols,
                      breaks=c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Did not vote"),
                      labels=c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Did not vote")) +
  labs(x ="", y = "Probability of voting for party",
       subtitle = "Attitudes to social welfare", caption = "Source: PGSW (2019)") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "plot.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4.5)
ggsave(p, file = "plot.pdf", width = 7, height = 5, units = "cm", dpi = 320, scale = 4.5, device = cairo_pdf)

ggplot() +
  geom_point(marg, mapping=aes(x=predicted, ymin=conf.low, ymax=conf.high)) 