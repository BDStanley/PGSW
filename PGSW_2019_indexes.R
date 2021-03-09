rm(list=ls())
library(tidyverse); library(rio); library(sjPlot); library(easystats); library(googledrive); library(sjPlot); library(sjlabelled); library(sjmisc); library(lindia); library(gvlma)
library(ggeffects); library(brms); library(tidybayes); library(modelr); library(psych); library(hrbrthemes); library(wesanderson); library(cmdstanr); library(MplusAutomation); library(mice); library(naniar) 
options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.4") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

import <- drive_download(as_id('https://drive.google.com/file/d/1Y7kjTpQkKyN_E0Ogj2yUfJu9gF4Dk98K/view?usp=sharing'), overwrite=TRUE)
1
read_2019 <- import('PGSW2019_CAPI.sav')

read_2019 <- read_2019 %>%
  mutate(across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q05_3, Q05_4, Q05_5), ~ replace(., . %in% 7:9, NA)),
         across(c(Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6), ~ replace(., . %in% 5:9, NA)),
         Q04_3 = case_when(Q04_3==1 ~ 7,
                           Q04_3==2 ~ 6,
                           Q04_3==3 ~ 5,
                           Q04_3==4 ~ 4,
                           Q04_3==5 ~ 3,
                           Q04_3==6 ~ 2,
                           Q04_3==7 ~ 1),
         Q05_3 = case_when(Q05_3==1 ~ 5,
                           Q05_3==2 ~ 4,
                           Q05_3==3 ~ 3,
                           Q05_3==4 ~ 2,
                           Q05_3==5 ~ 1),
         across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6), ~ fct_drop(as_factor(.))),
         #across(c(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7, Q05_1, Q05_2, Q05_3, Q05_4, Q05_5, Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6), ~ as.ordered(.))
  )


populism_frame <- with(read_2019, data.frame(Q04_1, Q04_2, Q04_3, Q04_4, Q04_5, Q04_6, Q04_7)) 
prepareMplusData(populism_frame, "/Users/benstanley/Google Drive/Resources/Mplus/PGSW_indexes_CSES_populism.dat")

migrants_frame <- with(read_2019, data.frame(Q05_1, Q05_2, Q05_3, Q05_4, Q05_5)) 
prepareMplusData(migrants_frame, "/Users/benstanley/Google Drive/Resources/Mplus/PGSW_indexes_migrants.dat")

realpole_frame <- with(read_2019, data.frame(Q06_1, Q06_2, Q06_3, Q06_4, Q06_5, Q06_6)) 
prepareMplusData(realpole_frame, "/Users/benstanley/Google Drive/Resources/Mplus/PGSW_indexes_realpole.dat")

