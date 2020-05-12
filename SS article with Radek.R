#####Prepare workspace#####
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(easystats)
library(sjmisc); library(sjPlot); library(rio); library(brms);
library(tidybayes); library(modelr); library(broom); library(hrbrthemes)

options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

load(file = "PGSW2019_CAPI_PL.RData")
cols <- c("PiS"="blue4", "KO"="orange", "PSL/Kukiz'15"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "Nie zagłosował(a)"="gray50")


#####Recode variables#####
pgsw2019$H10_3 <- recode(pgsw2019$H10_3, `1`= 5L, `2`= 4L, `7` = 3L, `3` = 2L, `4` = 1L)
pgsw2019$H10_4 <- recode(pgsw2019$H10_4, `1`= 5L, `2`= 4L, `7` = 3L, `3` = 2L, `4` = 1L)
pgsw2019$H10_5 <- recode(pgsw2019$H10_5, `1`= 5L, `2`= 4L, `7` = 3L, `3` = 2L, `4` = 1L)
pgsw2019$autoryt <- (pgsw2019$H10_3 + pgsw2019$H10_4 + pgsw2019$H10_5)/3
pgsw2019$autoryt <- remove_all_labels(pgsw2019$autoryt)

pgsw2019$P19_1 <- recode(pgsw2019$P19_1, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L)
pgsw2019$P19_2 <- recode(pgsw2019$P19_2, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L)
pgsw2019$P19_3 <- recode(pgsw2019$P19_3, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L)
pgsw2019$P19_4 <- recode(pgsw2019$P19_4, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L)
pgsw2019$P19_5 <- recode(pgsw2019$P19_5, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L)
pgsw2019$P19_6 <- recode(pgsw2019$P19_6, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L)
pgsw2019$populizm <- (pgsw2019$P19_1 + pgsw2019$P19_2 + pgsw2019$P19_3 + pgsw2019$P19_5 + pgsw2019$P19_6)/5
pgsw2019$populizm <- remove_all_labels(pgsw2019$populizm)

pgsw2019 <- pgsw2019 %>% mutate(K1_d = recode(K1_d, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L, `9` = 3L),
                    K1_e = recode(K1_e, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L, `9` = 3L),
                    K1_f = recode(K1_f, `1`= 5L, `2`= 4L, `3` = 3L, `4` = 2L, `5` = 1L, `7` = 3L, `8` = 3L, `9` = 3L)
)
pgsw2019$rygoryzm <- (pgsw2019$K1_d + pgsw2019$K1_e + pgsw2019$K1_f)/3

pgsw2019$D21 <- recode(pgsw2019$D21, `1`= 4L, `2`= 3L, `3` = 2L, `4` = 1L)
pgsw2019$D22 <- recode(pgsw2019$D22, 2, 1)
pgsw2019$D22 <- set_labels(pgsw2019$D22, labels=c("Państwo, które wspiera postęp społeczny i nowoczesność", "Państwo, które chroni i wspiera tradycyjne wartości"))
pgsw2019$D22 <- factorize(pgsw2019$D22)
pgsw2019$D23 <- recode(pgsw$D23, 2, 1)
pgsw2019$D23 <- set_labels(pgsw2019$D23, labels=c("Państwo, które stwarza korzystne warunki dla przedsiębiorczości ludzi", "Państwo, które tworzy podstawy do solidarności społecznej"))
pgsw2019$D23 <- factorize(pgsw2019$D23)

pgsw2019 <- pgsw2019 %>% 
  to_dummy(Q12LHb, var.name = "vote") %>%
  bind_cols(pgsw2019)

pgsw2019<- pgsw2019 %>%
  mutate(gr_wiek = fct_relevel(gr_wiek, "46-65"),
         gr_wykszt = fct_relevel(gr_wykszt, "Policealne"),
         wlm = fct_relevel(wlm, "Miasto 100-500 tys."),
         relig = fct_relevel(relig, "Kilka razy w roku")
  )


#####Analyses#####
fit_PiS <- brm(Q15a | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=gaussian())
pars_PiS <- parameters::parameters(fit_PiS, ci=0.95)
pars_table_PiS <- parameters_table(pars_PiS, stars=TRUE, pretty_names=TRUE)
perf_PiS <- performance(fit_PiS)
ame_PiS_relig <- bayes_dydx.factor(fit_PiS, variable="relig")
ame_PiS_relig$group <- "PiS"
plotdata <- ame_PiS_relig %>% 
  group_by(resp) %>% 
  summarise(median = median(est), lower = quantile(est, probs = .025), upper = quantile(est, probs = .975))

fit_PO <- brm(Q15b | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=gaussian())
pars_PO <- parameters::parameters(fit_PO, ci=0.95)
pars_table_PO <- parameters_table(pars_PO, stars=TRUE, pretty_names=TRUE)
perf_PO <- performance(fit_PO)
ame_PO_relig <- bayes_dydx.factor(fit_PO, variable="relig")
ame_PO_relig$group <- "PO"
plotdata <- ame_PO_relig %>% 
  group_by(resp) %>% 
  summarise(median = median(est), lower = quantile(est, probs = .025), upper = quantile(est, probs = .975))

fit_PSL <- brm(Q15c | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=gaussian())
pars_PSL <- parameters::parameters(fit_PSL, ci=0.95)
pars_table_PSL <- parameters_table(pars_PSL, stars=TRUE, pretty_names=TRUE)
perf_PSL <- performance(fit_PSL)
ame_PSL_relig <- bayes_dydx.factor(fit_PSL, variable="relig")
ame_PSL_relig$group <- "PSL"
plotdata <- ame_PSL_relig %>% 
  group_by(resp) %>% 
  summarise(median = median(est), lower = quantile(est, probs = .025), upper = quantile(est, probs = .975))

fit_Lewica <- brm(Q15d | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=gaussian())
pars_Lewica <- parameters::parameters(fit_Lewica, ci=0.95)
pars_table_Lewica <- parameters_table(pars_Lewica, stars=TRUE, pretty_names=TRUE)
perf_Lewica <- performance(fit_Lewica)
ame_Lewica_relig <- bayes_dydx.factor(fit_Lewica, variable="relig")
ame_Lewica_relig$group <- "Lewica"
plotdata <- ame_Lewica_relig %>% 
  group_by(resp) %>% 
  summarise(median = median(est), lower = quantile(est, probs = .025), upper = quantile(est, probs = .975))

fit_Konfederacja <- brm(Q15g | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=gaussian())
pars_Konfederacja <- parameters::parameters(fit_Konfederacja, ci=0.95)
pars_table_Konfederacja <- parameters_table(pars_Konfederacja, stars=TRUE, pretty_names=TRUE)
perf_Konfederacja <- performance(fit_Konfederacja)
ame_Konfederacja_relig <- bayes_dydx.factor(fit_Konfederacja, variable="relig")
ame_Konfederacja_relig$group <- "Konfederacja"
plotdata <- ame_Konfederacja_relig %>% 
  group_by(resp) %>% 
  summarise(median = median(est), lower = quantile(est, probs = .025), upper = quantile(est, probs = .975))

plotdata <- full_join(ame_PiS_relig, ame_PO_relig)
plotdata <- full_join(plotdata, ame_PSL_relig)
plotdata <- full_join(plotdata, ame_Lewica_relig)
plotdata <- full_join(plotdata, ame_Konfederacja_relig)

plot_relig <- ggplot(plotdata, aes(x = resp, y = est, color=group)) +
  geom_abline(intercept=0, slope=0, color="darkgray") +
  stat_pointinterval(position = position_dodge(width = .4), .width=c(0.95)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "", y = "Poziom lubienia PiS (zmiana względem poziomu bazowego)", 
       subtitle = "",
       color="",
       caption = "Poziom bazowy - Kilka razy w roku") +
  theme_minimal() +
  theme_ipsum_rc()


#Vote choice multinomial model
fit_vote_choice <- brm(Q12LHb | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, 
           data=pgsw2019, family=categorical(), control = list(adapt_delta = 0.95), cores=4)

labs <- c("PSL/Kukiz'15", "PiS", "Lewica", "Konfederacja", "KO", "Nie zagłosował")

plotdata <- pgsw2019 %>%
  data_grid(autoryt = seq_range(autoryt, n = 10), .model=fit_vote_choice) %>%
  add_fitted_draws(fit_vote_choice) 
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)
plotdata$.category <- fct_relevel(plotdata$.category, "PiS", "KO", "Lewica", "PSL/Kukiz'15", "Konfederacja", "Nie zagłosował")

plot <- ggplot(plotdata, aes(x = autoryt, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_grey() +
  scale_color_grey() +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,5), labels=c("Niski", "Wysoki")) +
  labs(x = "Autorytaryzm", y = "Prawdopodobieństwo", 
       subtitle = "",
       fill="",
       caption = "Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "autorytaryzm_2019.png", width = 8, height = 5, units = "cm", dpi = 320, scale = 4)


#Vote choice logit models
fit_logit_PSL <- brm(Q12LHb_1 | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=bernoulli())
pars_logit_PSL <- parameters::parameters(fit_logit_PSL, ci=0.95)
pars_table_logit_PSL <- parameters_table(pars_logit_PSL, stars=TRUE, pretty_names=TRUE)
perf_PSL <- performance(fit_logit_PSL)

fit_logit_PiS <- brm(Q12LHb_2 | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=bernoulli())
pars_logit_PiS <- parameters::parameters(fit_logit_PiS, ci=0.95)
pars_table_logit_PiS <- parameters_table(pars_logit_PiS, stars=TRUE, pretty_names=TRUE)
perf_PiS <- performance(fit_logit_PiS)

fit_logit_Lewica <- brm(Q12LHb_3 | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=bernoulli())
pars_logit_Lewica <- parameters::parameters(fit_logit_Lewica, ci=0.95)
pars_table_logit_Lewica <- parameters_table(pars_logit_Lewica, stars=TRUE, pretty_names=TRUE)
perf_Lewica <- performance(fit_logit_Lewica)

fit_logit_Konfederacja <- brm(Q12LHb_4 | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=bernoulli())
pars_logit_Konfederacja <- parameters::parameters(fit_logit_Konfederacja, ci=0.95)
pars_table_logit_Konfederacja <- parameters_table(pars_logit_Konfederacja, stars=TRUE, pretty_names=TRUE)
perf_Konfederacja <- performance(fit_logit_Konfederacja)

fit_logit_KO <- brm(Q12LHb_5 | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=bernoulli())
pars_logit_KO <- parameters::parameters(fit_logit_KO, ci=0.95)
pars_table_logit_KO <- parameters_table(pars_logit_KO, stars=TRUE, pretty_names=TRUE)
perf_KO <- performance(fit_logit_KO)

fit_logit_nieglos <- brm(Q12LHb_6 | weights(waga) ~ autoryt + populizm + rygoryzm + D21 + D22 + D23 + plec + gr_wiek + gr_wykszt + relig + wlm, data=pgsw2019, family=bernoulli())
pars_logit_nieglos <- parameters::parameters(fit_logit_nieglos, ci=0.95)
pars_table_logit_nieglos <- parameters_table(pars_logit_nieglos, stars=TRUE, pretty_names=TRUE)
perf_nieglos <- performance(fit_logit_nieglos)

plotdata_PiS <- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE),
            D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości",  
            D23="Państwo, które tworzy podstawy do solidarności społecznej", plec="Kobieta",
            gr_wiek="46-65", wlm="Miasto 100-500 tys.", gr_wykszt="Policealne", .model=fit_logit) %>%
  add_fitted_draws(fit_logit_PiS) %>%
  mutate(group = "PiS")

plotdata_KO <- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE),
            D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości",  
            D23="Państwo, które tworzy podstawy do solidarności społecznej", plec="Kobieta",
            gr_wiek="46-65", wlm="Miasto 100-500 tys.", gr_wykszt="Policealne", .model=fit_logit) %>%
  add_fitted_draws(fit_logit_KO) %>%
  mutate(group = "KO")
  
plotdata_PSL <- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE),
            D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości",  
            D23="Państwo, które tworzy podstawy do solidarności społecznej", plec="Kobieta",
            gr_wiek="46-65", wlm="Miasto 100-500 tys.", gr_wykszt="Policealne", .model=fit_logit) %>%
  add_fitted_draws(fit_logit_PSL) %>%
  mutate(group = "PSL/Kukiz'15")

plotdata_Lewica <- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE),
            D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości",  
            D23="Państwo, które tworzy podstawy do solidarności społecznej", plec="Kobieta",
            gr_wiek="46-65", wlm="Miasto 100-500 tys.", gr_wykszt="Policealne", .model=fit_logit) %>%
  add_fitted_draws(fit_logit_Lewica) %>%
  mutate(group = "Lewica")

plotdata_Konfederacja <- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE),
            D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości",  
            D23="Państwo, które tworzy podstawy do solidarności społecznej", plec="Kobieta",
            gr_wiek="46-65", wlm="Miasto 100-500 tys.", gr_wykszt="Policealne", .model=fit_logit) %>%
  add_fitted_draws(fit_logit_Konfederacja) %>%
  mutate(group = "Konfederacja")

plotdata_nieglos<- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE),
            D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości",  
            D23="Państwo, które tworzy podstawy do solidarności społecznej", plec="Kobieta",
            gr_wiek="46-65", wlm="Miasto 100-500 tys.", gr_wykszt="Policealne", .model=fit_logit) %>%
  add_fitted_draws(fit_logit_nieglos) %>%
  mutate(group = "Nie zagłosował(a)")

plotdata <- full_join(plotdata_PiS, plotdata_KO)
plotdata <- full_join(plotdata, plotdata_PSL)
plotdata <- full_join(plotdata, plotdata_Lewica)
plotdata <- full_join(plotdata, plotdata_Konfederacja)
plotdata <- full_join(plotdata, plotdata_nieglos)

plotdata$relig <- fct_relevel(plotdata$relig, "Nigdy / Raz w roku")
plotdata$group <- fct_relevel(plotdata$group, "PiS", "KO", "Lewica", "PSL/Kukiz'15", "Konfederacja", "Nie zagłosował(a)")

ggplot(plotdata, aes(x = relig, y = .value, color=group)) +
  stat_pointinterval(position = position_dodge(width = .4), .width=c(0.66, 0.95)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_manual(values=cols) +
  labs(x = "", y = "Prawdopodobieństwo", 
       subtitle = "",
       color="",
       caption = "") +
  theme_minimal() +
  theme_ipsum_rc()

