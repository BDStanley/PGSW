#####Prepare workspace#####
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); library(sjmisc)
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive); library(rio)


import <- drive_download(as_id('https://drive.google.com/file/d/1Y7kjTpQkKyN_E0Ogj2yUfJu9gF4Dk98K/view?usp=sharing'), overwrite=TRUE)
1
read_2019 <- import('PGSW2019_CAPI.sav')


names(read_2019)