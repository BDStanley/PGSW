#####Prepare workspace#####
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(easystats)
library(sjmisc); library(sjPlot); library(rio); library(brms);
library(tidybayes); library(modelr); library(broom); library(hrbrthemes); library(psych)

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

pgsw2019$D22 <- recode(pgsw2019$D22, 2, 1)
pgsw2019$D22 <- set_labels(pgsw2019$D22, labels=c("Państwo, które wspiera postęp społeczny i nowoczesność", "Państwo, które chroni i wspiera tradycyjne wartości"))
pgsw2019$D22 <- factorize(pgsw2019$D22)
pgsw2019$D23 <- recode(pgsw2019$D23, 2, 1)
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

frame_egot <- with(pgsw2019, data.frame(N3b, N4b, N5b))
frame_socjot <- with(pgsw2019, data.frame(N3a, N4a, N5a))

egot_model <- fa(frame_egot, nfactor=1, cor="poly", fm="pa", correct=0, rotate = "oblimin")
egot_res <- parameters::parameters(egot_model, threshold=0.3, sort=TRUE)
egot_scores <- egot_model$scores 
pgsw2019$egotropic <- scales::rescale(egot_scores, c(0,1))
pgsw2019$egotropic <- 1-pgsw2019$egotropic
var_label(pgsw2019$egotropic) <- "Głosowanie egotropiczne"

socjot_model <- fa(frame_socjot, nfactor=1, cor="poly", fm="pa", correct=0, rotate = "oblimin")
socjot_res <- parameters::parameters(socjot_model, threshold=0.3, sort=TRUE)
socjot_scores <- socjot_model$scores 
pgsw2019$socjotropic <- scales::rescale(socjot_scores, c(0,1))
pgsw2019$socjotropic <- 1-pgsw2019$socjotropic
var_label(pgsw2019$socjotropic) <- "Głosowanie socjotropiczne"


#####Analyses#####
pgsw2019$Q12LHb <- recode(pgsw2019$Q12LHb, `1`= 4L, `2`= 1L, `3` = 3L, `4` = 5L, `5` =2L, `6` = 6L)
pgsw2019$Q12LHb <- as.factor(pgsw2019$Q12LHb)
pgsw2019$Q12LHb <- fct_relevel(pgsw2019$Q12LHb, "2", "5", "3", "1", "4", "6") 
labs <- c("PiS", "KO", "Lewica", "PSL/Kukiz'15", "Konfederacja", "Nie zagłosował(a)")
pgsw2019 <- set_labels(pgsw2019, Q12LHb, labels=labs)

# Models
fit_mnl_1_b <- brm(Q12LHb | weights(waga) ~ autoryt + populizm + rygoryzm + mo(D21) + D22*D23, 
                   data=pgsw2019, family=categorical(), cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15), iter=4000)
fit_mnl_2_b <- brm(Q12LHb | weights(waga) ~ autoryt + populizm + rygoryzm + mo(D21) + D22*D23 + 
                     egotropic + socjotropic + I1b + I1e, 
                   data=pgsw2019, family=categorical(), cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15), iter=4000)
fit_mnl_3_b <- brm(Q12LHb | weights(waga) ~ autoryt + populizm + rygoryzm + mo(D21) + D22*D23 + 
                     egotropic + socjotropic + I1b + I1e +
                     plec + wiek + relig + P67a, 
                   data=pgsw2019, family=categorical(), cores=4, control = list(adapt_delta = 0.99, max_treedepth = 15), iter=4000)

pars_1 <- parameters::parameters(fit_mnl_1_b, ci=0.95, test="p_map")
pars_1 <- tibble(parameters_table(pars_1, stars=TRUE, pretty_names = TRUE))
export(pars_1, "pars_1.csv")

pars_2 <- parameters::parameters(fit_mnl_2_b, ci=0.95, test="p_map")
pars_2 <- tibble(parameters_table(pars_2, stars=TRUE, pretty_names = TRUE))
export(pars_2, "pars_2.csv")

pars_3 <- parameters::parameters(fit_mnl_3_b, ci=0.95, test="p_map")
pars_3 <- tibble(parameters_table(pars_3, stars=TRUE, pretty_names = TRUE))
export(pars_3, "pars_3.csv")


# Plot autoryt
plotdata <- pgsw2019 %>%
  data_grid(autoryt, populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej") %>%
  add_fitted_draws(fit_mnl_1_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = autoryt, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,5), labels=c("Niski", "Wysoki")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Autorytaryzm",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "autoryt.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot populizm
plotdata <- pgsw2019 %>%
  data_grid(populizm, autoryt=median(autoryt, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej") %>%
  add_fitted_draws(fit_mnl_1_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = populizm, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,5), labels=c("Niski", "Wysoki")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Populizm",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "populizm.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot rygoryzm
plotdata <- pgsw2019 %>%
  data_grid(rygoryzm, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej") %>%
  add_fitted_draws(fit_mnl_1_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = rygoryzm, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,5), labels=c("Niski", "Wysoki")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Rygoryzm",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "rygoryzm.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot wolność / równość
plotdata <- pgsw2019 %>%
  data_grid(D21, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm=TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej") %>%
  add_fitted_draws(fit_mnl_1_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = D21, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,4), labels=c("Zdecydowanie\nwolność", "Zdecydowanie\nrówność")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Wolność czy równość?",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "wolnoscrownosc.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot role of the state
plotdata <- pgsw2019 %>%
  group_by(D23) %>%
  data_grid(D22, autoryt=median(autoryt, na.rm = TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21)) %>%
  add_fitted_draws(fit_mnl_1_b) %>%
  summarise(median = median(.value), lower = quantile(.value, probs = .025), upper = quantile(.value, probs = .975))
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

pd <- position_dodge(0.4)
plot <- ggplot(plotdata, aes(x = .row, y = median, ymin=lower, ymax=upper, color=.category), position=pd) +
  geom_linerange(position=pd) +
  geom_point(position=pd, size=2) +
  scale_x_discrete(limits=c(1:4), labels=c("Postęp społeczny /\nWarunki dla przedsiębiorczości", "Tradycyjne wartości /\nWarunki dla przedsiębiorczości", "Postęp społeczny /\nSolidarność społeczna", "Tradycyjne wartości /\nSolidarność społeczna")) +
  scale_color_manual(values=cols) +
  labs(x = "", y = "Prawdodpodobieństwo",
       title = "Która z dwóch wizji jest Panu/Pani bliższe?",
       subtitle = "Państwo, które wspiera postęp społeczny i nowoczesność, czy państwo, które chroni i wspiera tradycyjne wartości?\nPaństwo, które stwarza korzystne warunki dla przedsiębiorczości ludzi, czy państwo, które tworzy podstawy do solidarności społecznej?",
       color="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "panstwo.png", width = 8, height = 5, units = "cm", dpi = 320, scale = 4.5)

# Plot egotropic
plotdata <- pgsw2019 %>%
  data_grid(egotropic, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej",
            socjotropic=median(socjotropic, na.rm = TRUE), I1b=median(I1b, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE)) %>%
  add_fitted_draws(fit_mnl_2_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = egotropic, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(0,1), labels=c("Bardzo zła", "Bardzo dobra")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Egotropowa ocena sytuacji gospodarki",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "egotropic.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot socjotropic
plotdata <- pgsw2019 %>%
  data_grid(socjotropic, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej",
            egotropic=median(egotropic, na.rm = TRUE), I1b=median(I1b, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE)) %>%
  add_fitted_draws(fit_mnl_2_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = socjotropic, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(0,1), labels=c("Bardzo zła", "Bardzo dobra")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Socjotropowa ocena sytuacji gospodarki",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "socjotropic.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot I1b
plotdata <- pgsw2019 %>%
  data_grid(I1b, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej",
            egotropic=median(egotropic, na.rm = TRUE), socjotropic=median(socjotropic, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE)) %>%
  add_fitted_draws(fit_mnl_2_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = I1b, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,7), labels=c("Progresywne", "Regresywne")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Podatek dochodowy",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "podatek.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Plot I1e
plotdata <- pgsw2019 %>%
  data_grid(I1e, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej",
            egotropic=median(egotropic, na.rm = TRUE), socjotropic=median(socjotropic, na.rm = TRUE), I1b=median(I1e, na.rm = TRUE)) %>%
  add_fitted_draws(fit_mnl_2_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = I1e, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  scale_x_continuous(breaks=c(1,7), labels=c("Państwo", "Obywatele")) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Świadczenia społeczne",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "spoleczne.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)


# Płeć
plotdata <- pgsw2019 %>%
  data_grid(plec, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21, na.rm=TRUE), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej", 
            egotropic=median(egotropic, na.rm = TRUE), socjotropic=median(socjotropic, na.rm = TRUE), I1b=median(I1b, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE),
            wiek=median(wiek, na.rm = TRUE), relig="Kilka razy w roku", P67a=median(P67a, na.rm = TRUE), .model=fit_mnl_3_b) %>%
  add_fitted_draws(fit_mnl_3_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = plec, y = .value, color=.category)) +
  stat_pointinterval(position = position_dodge(width = .4), .width=c(0.66, 0.95)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_manual(values=cols) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Płeć",
       color="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "plec.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Wiek
plotdata <- pgsw2019 %>%
  data_grid(wiek, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21, na.rm=TRUE), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej", 
            egotropic=median(egotropic, na.rm = TRUE), socjotropic=median(socjotropic, na.rm = TRUE), I1b=median(I1b, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE),
            plec="Kobieta", relig="Kilka razy w roku", P67a=median(P67a, na.rm = TRUE), .model=fit_mnl_3_b) %>%
  add_fitted_draws(fit_mnl_3_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = wiek, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Wiek",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "wiek.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Relig
plotdata <- pgsw2019 %>%
  data_grid(relig, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21, na.rm=TRUE), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej", 
            egotropic=median(egotropic, na.rm = TRUE), socjotropic=median(socjotropic, na.rm = TRUE), I1b=median(I1b, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE),
            plec="Kobieta", wiek=median(wiek, na.rm = TRUE), P67a=median(P67a, na.rm = TRUE), .model=fit_mnl_3_b) %>%
  add_fitted_draws(fit_mnl_3_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)
plotdata$relig <- fct_relevel(plotdata$relig, "Nigdy / Raz w roku")

plot <- ggplot(plotdata, aes(x = relig, y = .value, color=.category)) +
  stat_pointinterval(position = position_dodge(width = .4), .width=c(0.66, 0.95)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_manual(values=cols) +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Religijność",
       color="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "relig.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)

# Zamożność
plotdata <- pgsw2019 %>%
  data_grid(P67a, autoryt=median(autoryt, na.rm=TRUE), populizm=median(populizm, na.rm = TRUE), rygoryzm=median(rygoryzm, na.rm=TRUE), D21=median(D21, na.rm=TRUE), D22="Państwo, które chroni i wspiera tradycyjne wartości", D23="Państwo, które tworzy podstawy do solidarności społecznej", 
            egotropic=median(egotropic, na.rm = TRUE), socjotropic=median(socjotropic, na.rm = TRUE), I1b=median(I1b, na.rm = TRUE), I1e=median(I1e, na.rm = TRUE),
            plec="Kobieta", relig="Kilka razy w roku", wiek=median(wiek, na.rm = TRUE), .model=fit_mnl_3_b) %>%
  add_fitted_draws(fit_mnl_3_b)
plotdata <- set_labels(plotdata, .category, labels=labs)
plotdata$.category <- factorize(plotdata$.category)

plot <- ggplot(plotdata, aes(x = P67a, y = .value, color=.category)) +
  stat_lineribbon(aes(y = .value, fill=.category), .width=c(0.66, 0.95), show.legend=FALSE, alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(breaks=c(1,10), labels=c("Najniższa", "Najwyższa")) +
  facet_wrap(~.category, nrow=2, scales="free_y") +
  labs(x = "", y = "Prawdopodobieństwo zagłosowania", 
       subtitle = "Zamożność",
       fill="",
       caption = "PGSW 2019, Centrum Studiów nad Demokracją, SWPS") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot, file = "zamoznosc.png", width = 9, height = 5, units = "cm", dpi = 320, scale = 5.2)





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

ggplot(plotdata, aes(x=factor, y=AME, colour=autoryt, group=autoryt)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3)
