#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive)

#Download and read data
import <- drive_download(as_id('https://drive.google.com/file/d/1jcZZwd0T7j4n_m-ujfNM5fAeRFF0_naW/view?usp=sharing'), overwrite=TRUE)
1
read <- read_spss('PGSW2019_CAPI_old.sav')
pgsw2019 <- tibble(1:2003)

#Save data as R image
save.image(file = "PGSW2019_old.RData")
write_stata(read, path='/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW2019/PGSW_2019_old.dta', version=14)
