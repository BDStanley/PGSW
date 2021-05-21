rm(list=ls())
library(tidyverse); library(rio); library(sjPlot); library(easystats); library(googledrive); library(sjPlot); library(sjlabelled); library(labelled); library(sjmisc); library(lindia); library(gvlma)
library(ggeffects); library(brms); library(tidybayes); library(modelr); library(psych); library(hrbrthemes); library(wesanderson); library(cmdstanr); library(MplusAutomation); library(mice); library(naniar) 
options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.4") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}


#####Import data#####
import <- drive_download(as_id('https://drive.google.com/file/d/1Y7kjTpQkKyN_E0Ogj2yUfJu9gF4Dk98K/view?usp=sharing'), overwrite=TRUE)
1
read_2019 <- import('PGSW2019_CAPI.sav')

#####Clean and prepare data#####
read_2019$voted <- recode_factor(read_2019$Q12LHa,
                                 `5` = "No",
                                 `6` = "No",
                                 `1` = "Yes")
var_label(read_2019$voted) <- "Voted in most recent election to the Sejm"

read_2019$votefor <- recode_factor(read_2019$Q12LHb,
                                   `2` = "PiS",
                                   `5` = "KO",
                                   `3` = "Lewica",
                                   `1` = "PSL-Kukiz",
                                   `4` = "Konfederacja")
read_2019$votefor <- fct_expand(read_2019$votefor, "Did not vote")
read_2019$votefor[read_2019$voted=="No"] <- "Did not vote"
var_label(read_2019$votefor) <- "Party voted for in most recent election to the Sejm"

read_2019 <- read_2019 %>%
  mutate(across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q05_3, Q05_4, Q05_5), ~ replace(., . %in% 7:9, NA)),
         across(c(Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6, Q07), ~ replace(., . %in% 5:9, NA)),
         # Q04_3 = case_when(Q04_3==1 ~ 7,
         #                   Q04_3==2 ~ 6,
         #                   Q04_3==3 ~ 5,
         #                   Q04_3==4 ~ 4,
         #                   Q04_3==5 ~ 3,
         #                   Q04_3==6 ~ 2,
         #                   Q04_3==7 ~ 1),
         # Q05_3 = case_when(Q05_3==1 ~ 5,
         #                   Q05_3==2 ~ 4,
         #                   Q05_3==3 ~ 3,
         #                   Q05_3==4 ~ 2,
         #                   Q05_3==5 ~ 1),
         across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6), ~ fct_drop(as_factor(.))),
         #across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6), ~ as.ordered(.))
  )


#####Export to Mplus#####
populism_frame <- with(read_2019, data.frame(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q07, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6)) 
prepareMplusData(populism_frame, "/Users/benstanley/Google Drive/Resources/Mplus/PGSW_indexes_CSES_populism.dat")



#####EFA models#####
populism_frame <- with(read_2019, data.frame(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q07, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6)) 

populism_frame_num <- populism_frame %>%
  mutate(across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q07, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6), ~ as.numeric(.)))

poly_model <- fa(populism_frame_num, nfactor=3, rotate = "oblimin")
poly_model_res <- parameters::parameters(poly_model, threshold=0.3, sort=TRUE)
fs_fa <- factor.scores(populism_frame_num, poly_model)
read_2019$nativism <- scales::rescale(fs_fa$scores)[,1]
read_2019$nativism <- 1-read_2019$nativism
read_2019$antielite <- scales::rescale(fs_fa$scores)[,2]
read_2019$antielite <- 1-read_2019$antielite
read_2019$pluralism <- scales::rescale(fs_fa$scores)[,3]


#####Voting behaviour model#####
bprior <- c(prior_string("normal(0,1)", class="b"))

fit <- brm(votefor | weights(waga) ~ nativism + antielite + pluralism, data=read_2019, prior=bprior, sample_prior = TRUE, family=categorical())

#####Plots#####
cols <- c("PiS"="blue4", "KO"="orange", "PSL-Kukiz"="darkgreen", "Konfederacja" = "darkgoldenrod4", "Lewica" = "red", "Did not vote" = "grey50")

nativism_plot <- read_2019 %>%
  data_grid(antielite=mean(antielite, na.rm=TRUE), pluralism=mean(pluralism, na.rm=TRUE),
            nativism=seq(0, 1, by=0.05)) %>% 
  add_fitted_draws(fit) %>%
  ggplot() +
  stat_lineribbon(aes(x = nativism, y = .value, fill=.category, color=.category), .width=c(0.95), alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  labs(x = "Nativist attitudes", y = "Predicted probability of voting", 
       title="Nativist attitudes and voting behaviour in Poland, 2019", subtitle="",
       color="",
       fill="",
       caption = "Polish National Election Study, 2019") +
  guides(fill=guide_legend(override.aes = list(alpha=1))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(nativism_plot, file = "nativism_plot.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5.5)

antielite_plot <- read_2019 %>%
  data_grid(antielite=seq(0, 1, by=0.05), pluralism=mean(pluralism, na.rm=TRUE),
            nativism=mean(nativism, na.rm=TRUE)) %>% 
  add_fitted_draws(fit) %>%
  ggplot() +
  stat_lineribbon(aes(x = antielite, y = .value, fill=.category, color=.category), .width=c(0.95), alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  labs(x = "Anti-elite attitudes", y = "Predicted probability of voting", 
       title="Anti-elite attitudes and voting behaviour in Poland, 2019", subtitle="",
       color="",
       fill="",
       caption = "Polish National Election Study, 2019") +
  guides(fill=guide_legend(override.aes = list(alpha=1))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(antielite_plot, file = "antielite_plot.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5.5)

pluralism_plot <- read_2019 %>%
  data_grid(pluralism=seq(0, 1, by=0.05), antielite=mean(antielite, na.rm=TRUE),
            nativism=mean(nativism, na.rm=TRUE)) %>% 
  add_fitted_draws(fit) %>%
  ggplot() +
  stat_lineribbon(aes(x = pluralism, y = .value, fill=.category, color=.category), .width=c(0.95), alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  labs(x = "Pro-pluralism attitudes", y = "Predicted probability of voting", 
       title="Pro-pluralism attitudes and voting behaviour in Poland, 2019", subtitle="",
       color="",
       fill="",
       caption = "Polish National Election Study, 2019") +
  guides(fill=guide_legend(override.aes = list(alpha=1))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(pluralism_plot, file = "pluralism_plot.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5.5)


#####Simplism#####
simplism_frame <- with(read_2019, data.frame(S1_1, S1_2, S1_3, S1_4, S1_5, S1_6)) 

simplism_frame_num <- simplism_frame %>%
  mutate(across(c(S1_1, S1_2, S1_3, S1_4, S1_5, S1_6), ~ as.numeric(.)))

simplism_frame_2 <- with(read_2019, data.frame(S1_1, S1_2, S1_4, S1_5, S1_6)) 

simplism_frame_2_num <- simplism_frame_2 %>%
  mutate(across(c(S1_1, S1_2, S1_4, S1_5, S1_6), ~ as.numeric(.)))

read_2019 <- mutate(read_2019, edlevel = factor(case_when(read_2019$D03==1 | read_2019$D03==2  ~ "Basic or none",
                                                        read_2019$D03==3 | read_2019$D03==4 ~ "Lower secondary",
                                                        read_2019$D03==5 | read_2019$D03==6 ~ "Upper secondary",
                                                        read_2019$D03==7 | read_2019$D03==8 | read_2019$D03==9  ~ "Higher")))
read_2019$edlevel <- fct_relevel(read_2019$edlevel, "Basic or none", "Lower secondary", "Upper secondary", "Higher")
var_label(read_2019$edlevel) <- "Level of education"


HS.model <- ' simplism  =~ S1_1 + S1_2 + S1_3 + S1_4 + S1_5 + S1_6'
fit <- cfa(HS.model, data=simplism_frame_num)
idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  simplism_frame_num[idx, fs] <- fscores[ , fs]
}
read_2019$simplism <- scales::rescale(simplism_frame_num$simplism, c(0,1))
var_label(read_2019$simplism ) <- "Index of simplism"

HS.model <- ' simplism  =~ S1_1 + S1_2 + S1_4 + S1_5 + S1_6'
fit <- cfa(HS.model, data=simplism_frame_2_num)
idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit)
for (fs in colnames(fscores)) {
  simplism_frame_2_num[idx, fs] <- fscores[ , fs]
}
read_2019$simplism_alt <- scales::rescale(simplism_frame_2_num$simplism, c(0,1))
var_label(read_2019$simplism_alt) <- "Index of simplism (without S1_3)"

bprior <- c(prior_string("normal(0,1)", class="b"))

fit <- brm(votefor | weights(waga) ~ simplism_alt, data=read_2019, prior=bprior, sample_prior = TRUE, family=categorical())

simplism_plot <- read_2019 %>%
  data_grid(simplism_alt=seq(0, 1, by=0.05)) %>% 
  add_fitted_draws(fit) %>%
  ggplot() +
  stat_lineribbon(aes(x = simplism_alt, y = .value, fill=.category, color=.category), .width=c(0.95), alpha=1/2) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  labs(x = "Simplism", y = "Predicted probability of voting", 
       title="Simplism and voting behaviour in Poland, 2019", subtitle="",
       color="",
       fill="",
       caption = "Polish National Election Study, 2019") +
  guides(fill=guide_legend(override.aes = list(alpha=1))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(simplism_plot, file = "simplism_plot.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5.5)

fit <- brm(simplism | weights(waga) ~ edlevel, data=read_2019, prior=bprior, sample_prior = TRUE)

simplism_plot <- read_2019 %>%
  data_grid(edlevel) %>% 
  add_fitted_draws(fit) %>%
  ggplot() +
  stat_pointinterval(aes(x = edlevel, y = .value), .width=c(0.95), alpha=1/2) +
  labs(x = "Level of education", y = "Value of simplism", 
       title="Simplism and education in Poland, 2019", subtitle="",
       color="",
       fill="",
       caption = "Polish National Election Study, 2019") +
  guides(fill=guide_legend(override.aes = list(alpha=1))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(simplism_plot, file = "simplism_ed_plot.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5.5)

