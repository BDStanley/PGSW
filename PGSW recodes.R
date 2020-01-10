###########PREPARE WORKSPACE##########
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(sjmisc); library(sjPlot)

#####Read in files#####
load("PGSW1997.RData")
load("PGSW2001.RData")
load("PGSW2005.RData")
load("PGSW2007.RData")
load("PGSW2011.RData")
load("PGSW2015.RData")
load("PGSW2019.RData")

#####Rescale inconsistent variables#####
pgsw1997$leftrt <- rescale(pgsw1997$leftrt, c(0,1))
pgsw1997$leftrt <- set_labels(pgsw1997$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw1997$leftrt) <- "Left-right self-placement"

pgsw1997$euinteg <- rescale(pgsw1997$euinteg, c(0,1))
pgsw1997$euinteg <- set_labels(pgsw1997$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw1997$euinteg) <- "European integration"

pgsw1997$chstdiv <- rescale(pgsw1997$chstdiv, c(0,1))
pgsw1997$chstdiv <- set_labels(pgsw1997$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw1997$chstdiv) <- "Church/state divide" 

pgsw1997$taxreg <- rescale(pgsw1997$taxreg, c(0,1))
pgsw1997$taxreg <- set_labels(pgsw1997$taxreg, labels = c("Progressive tax regime" = 0, "Flat tax regime" = 1))
var_label(pgsw1997$taxreg) <- "Tax regime" 

pgsw1997$socpol <- rescale(pgsw1997$socpol, c(0,1))
pgsw1997$socpol <- set_labels(pgsw1997$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw1997$socpol) <- "Social policy" 

pgsw1997$unemp <- rescale(pgsw1997$unemp, c(0,1))
pgsw1997$unemp <- set_labels(pgsw1997$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw1997$unemp) <- "Unemployment" 

pgsw1997$private <- rescale(pgsw1997$private, c(0,1))
pgsw1997$private <- set_labels(pgsw1997$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw1997$private) <- "Privatisation" 

pgsw1997$abort <- rescale(pgsw1997$abort, c(0,1))
pgsw1997$abort <- set_labels(pgsw1997$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw1997$abort) <- "Abortion" 

pgsw1997$crime <- rescale(pgsw1997$crime, c(0,1))
pgsw1997$crime <- set_labels(pgsw1997$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw1997$crime) <- "Crime" 

pgsw2001$leftrt <- rescale(pgsw2001$leftrt, c(0,1))
pgsw2001$leftrt <- set_labels(pgsw2001$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2001$leftrt) <- "Left-right self-placement"

pgsw2001$euinteg <- rescale(pgsw2001$euinteg, c(0,1))
pgsw2001$euinteg <- set_labels(pgsw2001$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2001$euinteg) <- "European integration"

pgsw2001$chstdiv <- rescale(pgsw2001$chstdiv, c(0,1))
pgsw2001$chstdiv <- set_labels(pgsw2001$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2001$chstdiv) <- "Church/state divide" 

pgsw2001$taxreg <- rescale(pgsw2001$taxreg, c(0,1))
pgsw2001$taxreg <- set_labels(pgsw2001$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2001$taxreg) <- "Tax regime" 

pgsw2001$socpol <- rescale(pgsw2001$socpol, c(0,1))
pgsw2001$socpol <- set_labels(pgsw2001$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2001$socpol) <- "Social policy" 

pgsw2001$unemp <- rescale(pgsw2001$unemp, c(0,1))
pgsw2001$unemp <- set_labels(pgsw2001$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2001$unemp) <- "Unemployment" 

pgsw2001$private <- rescale(pgsw2001$private, c(0,1))
pgsw2001$private <- set_labels(pgsw2001$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2001$private) <- "Privatisation" 

pgsw2001$crime <- rescale(pgsw2001$crime, c(0,1))
pgsw2001$crime <- set_labels(pgsw2001$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2001$crime) <- "Crime" 

pgsw2005$leftrt <- rescale(pgsw2005$leftrt, c(0,1))
pgsw2005$leftrt <- set_labels(pgsw2005$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2005$leftrt) <- "Left-right self-placement"

pgsw2005$euinteg <- rescale(pgsw2005$euinteg, c(0,1))
pgsw2005$euinteg <- set_labels(pgsw2005$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2005$euinteg) <- "European integration"

pgsw2005$chstdiv <- rescale(pgsw2005$chstdiv, c(0,1))
pgsw2005$chstdiv <- set_labels(pgsw2005$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2005$chstdiv) <- "Church/state divide" 

pgsw2005$taxreg <- rescale(pgsw2005$taxreg, c(0,1))
pgsw2005$taxreg <- set_labels(pgsw2005$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2005$taxreg) <- "Tax regime" 

pgsw2005$socpol <- rescale(pgsw2005$socpol, c(0,1))
pgsw2005$socpol <- set_labels(pgsw2005$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2005$socpol) <- "Social policy" 

pgsw2005$unemp <- rescale(pgsw2005$unemp, c(0,1))
pgsw2005$unemp <- set_labels(pgsw2005$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2005$unemp) <- "Unemployment" 

pgsw2005$private <- rescale(pgsw2005$private, c(0,1))
pgsw2005$private <- set_labels(pgsw2005$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2005$private) <- "Privatisation" 

pgsw2005$abort <- rescale(pgsw2005$abort, c(0,1))
pgsw2005$abort <- set_labels(pgsw2005$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2005$abort) <- "Abortion" 

pgsw2005$crime <- rescale(pgsw2005$crime, c(0,1))
pgsw2005$crime <- set_labels(pgsw2005$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2005$crime) <- "Crime" 

pgsw2005$immigr <- rescale(pgsw2005$immigr, c(0,1))
pgsw2005$immigr <- set_labels(pgsw2005$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0,
                                                          "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2005$immigr) <- "Immigration" 

pgsw2007$leftrt <- rescale(pgsw2007$leftrt, c(0,1))
pgsw2007$leftrt <- set_labels(pgsw2007$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2007$leftrt) <- "Left-right self-placement"

pgsw2007$euinteg <- rescale(pgsw2007$euinteg, c(0,1))
pgsw2007$euinteg <- set_labels(pgsw2007$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2007$euinteg) <- "European integration"

pgsw2007$taxreg <- rescale(pgsw2007$taxreg, c(0,1))
pgsw2007$taxreg <- set_labels(pgsw2007$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2007$taxreg) <- "Tax regime" 

pgsw2007$unemp <- rescale(pgsw2007$unemp, c(0,1))
pgsw2007$unemp <- set_labels(pgsw2007$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2007$unemp) <- "Unemployment" 

pgsw2007$private <- rescale(pgsw2007$private, c(0,1))
pgsw2007$private <- set_labels(pgsw2007$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2007$private) <- "Privatisation" 

pgsw2007$abort <- rescale(pgsw2007$abort, c(0,1))
pgsw2007$abort <- set_labels(pgsw2007$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2007$abort) <- "Abortion" 

pgsw2007$crime <- rescale(pgsw2007$crime, c(0,1))
pgsw2007$crime <- set_labels(pgsw2007$crime, labels = c("Tough fight against crime, even at the expense of citizens' rights" = 0,
                                                        "Fight crime, but with attention to citizens' rights" = 1))
var_label(pgsw2007$crime) <- "Crime"

pgsw2011$leftrt <- rescale(pgsw2011$leftrt, c(0,1))
pgsw2011$leftrt <- set_labels(pgsw2011$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2011$leftrt) <- "Left-right self-placement"  

pgsw2011$euinteg <- rescale(pgsw2011$euinteg, c(0,1))
pgsw2011$euinteg <- set_labels(pgsw2011$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2011$euinteg) <- "European integration"   

pgsw2011$chstdiv <- rescale(pgsw2011$chstdiv, c(0,1))
pgsw2011$chstdiv <- set_labels(pgsw2011$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2011$chstdiv) <- "Church/state divide"                                                           

pgsw2011$taxreg <- rescale(pgsw2011$taxreg, c(0,1))
pgsw2011$taxreg <- set_labels(pgsw2011$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2011$taxreg) <- "Tax regime"                                                           

pgsw2011$unemp <- rescale(pgsw2011$unemp, c(0,1))
pgsw2011$unemp <- set_labels(pgsw2011$unemp, labels = c("Employment should be an absolute policy priority" = 0,
                                                        "Employment is less important than other policy domains" = 1))
var_label(pgsw2011$unemp) <- "Unemployment"  

pgsw2011$socpol <- rescale(pgsw2011$socpol, c(0,1))
pgsw2011$socpol <- set_labels(pgsw2011$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2011$socpol) <- "Social policy"  

pgsw2011$private <- rescale(pgsw2011$private, c(0,1))
pgsw2011$private <- set_labels(pgsw2011$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2011$private) <- "Privatisation"  

pgsw2011$abort <- rescale(pgsw2011$abort, c(0,1))
pgsw2011$abort <- set_labels(pgsw2011$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2011$abort) <- "Abortion"  

pgsw2015$leftrt <- rescale(pgsw2015$leftrt, c(0,1))
pgsw2015$leftrt <- set_labels(pgsw2015$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2015$leftrt) <- "Left-right self-placement"  

pgsw2015$sollib <- rescale(pgsw2015$sollib, c(0,1))
pgsw2015$sollib <- set_labels(pgsw2015$sollib, labels = c("Solidaristic" = 0, "Liberal" = 1))
var_label(pgsw2015$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2015$euinteg <- rescale(pgsw2015$euinteg, c(0,1))
pgsw2015$euinteg <- set_labels(pgsw2015$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2015$euinteg) <- "European integration"   

pgsw2015$chstdiv <- rescale(pgsw2015$chstdiv, c(0,1))
pgsw2015$chstdiv <- set_labels(pgsw2015$chstdiv, labels = c("The Church should have significant influence on the politics of the state" = 0,
                                                            "The Church should be completely separate from the state" = 1))
var_label(pgsw2015$chstdiv) <- "Church/state divide"                                                           

pgsw2015$taxreg <- rescale(pgsw2015$taxreg, c(0,1))
pgsw2015$taxreg <- set_labels(pgsw2015$taxreg, labels = c("Progressive tax regime" = 0,
                                                          "Flat tax regime" = 1))
var_label(pgsw2015$taxreg) <- "Tax regime"                                                           

pgsw2015$forpol <- rescale(pgsw2015$forpol, c(0,1))
pgsw2015$forpol <- set_labels(pgsw2015$forpol, labels = c("Foreign policy should be based on political and economic independence" = 0,
                                                          "Foreign policy should be based on close cooperation with the EU" = 1))
var_label(pgsw2015$forpol) <- "Foreign policy"  

pgsw2015$immigr <- rescale(pgsw2015$immigr, c(0,1))
pgsw2015$immigr <- set_labels(pgsw2015$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0,
                                                          "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2015$immigr) <- "Immigration"  

pgsw2015$socpol <- rescale(pgsw2015$socpol, c(0,1))
pgsw2015$socpol <- set_labels(pgsw2015$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0,
                                                          "People should take care of their own welfare" = 1))
var_label(pgsw2015$socpol) <- "Social policy"  

pgsw2015$private <- rescale(pgsw2015$private, c(0,1))
pgsw2015$private <- set_labels(pgsw2015$private, labels = c("A significant number of enterprises should remain in state hands" = 0,
                                                            "All state-owned enterprises should be privatised" = 1))
var_label(pgsw2015$private) <- "Privatisation"  

pgsw2015$abort <- rescale(pgsw2015$abort, c(0,1))
pgsw2015$abort <- set_labels(pgsw2015$abort, labels = c("There should be no right to abortion" = 0,
                                                        "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2015$abort) <- "Abortion"

pgsw2019$leftrt <- rescale(pgsw2019$leftrt, c(0,1))
pgsw2019$leftrt <- set_labels(pgsw2019$leftrt, labels = c("Left" = 0, "Right" = 1))
var_label(pgsw2019$leftrt) <- "Left-right self-placement"  

pgsw2019$sollib <- rescale(pgsw2019$sollib, c(0,1))
pgsw2019$sollib <- set_labels(pgsw2019$sollib, labels = c("Solidaristic" = 0, "Liberal" = 1))
var_label(pgsw2019$sollib) <- "Solidarism-liberalism self-placement"   

pgsw2019$euinteg <- rescale(pgsw2019$euinteg, c(0,1))
pgsw2019$euinteg <- set_labels(pgsw2019$euinteg, labels = c("Anti-integration" = 0, "Pro-integration" = 1))
var_label(pgsw2019$euinteg) <- "European integration"  

pgsw2019$climate <- rescale(pgsw2019$climate, c(0,1))
pgsw2019$climate <- set_labels(pgsw2019$climate, labels = c("Climate change and environmental degradation is not an important problem" = 0,
                                                            "Climate change and environmental degradation is the most important problem facing Poland" = 1))
var_label(pgsw2019$climate) <- "Climate change"  

pgsw2019$taxreg <- rescale(pgsw2019$taxreg, c(0,1))
pgsw2019$taxreg <- set_labels(pgsw2019$taxreg, labels = c("Progressive tax regime" = 0, "Flat tax regime" = 1))
var_label(pgsw2019$taxreg) <- "Tax regime"     

pgsw2019$forpol <- rescale(pgsw2019$forpol, c(0,1))
pgsw2019$forpol <- set_labels(pgsw2019$forpol, labels = c("Foreign policy should be based on political and economic independence" = 0, "Foreign policy should be based on close cooperation with the EU" = 1))
var_label(pgsw2019$forpol) <- "Foreign policy"  

pgsw2019$immigr <- rescale(pgsw2019$immigr, c(0,1))
pgsw2019$immigr <- set_labels(pgsw2019$immigr, labels = c("The state should work to stop immigrants from settling in Poland" = 0, "The state should encourage people from other countries to immigrate to Poland" = 1))
var_label(pgsw2019$immigr) <- "Immigration"  

pgsw2019$socpol <- rescale(pgsw2019$socpol, c(0,1))
pgsw2019$socpol <- set_labels(pgsw2019$socpol, labels = c("The state should ensure a wide range of social and welfare services" = 0, "People should take care of their own welfare" = 1))
var_label(pgsw2019$socpol) <- "Social policy"  

pgsw2019$lgbt <- rescale(pgsw2019$lgbt, c(0,1))
pgsw2019$lgbt <- set_labels(pgsw2019$lgbt, labels = c("Same-sex couples should not have the same rights as heterosexuals to publicly display their lifestyle" = 0, "Same-sex couples should have the same rights as heterosexuals to publicly display their lifestyle" = 1))
var_label(pgsw2019$lgbt) <- "LGBT rights"  

pgsw2019$abort <- rescale(pgsw2019$abort, c(0,1))
pgsw2019$abort <- set_labels(pgsw2019$abort, labels = c("There should be no right to abortion" = 0, "A woman should have a right to an abortion whatever the circumstances" = 1))
var_label(pgsw2019$abort) <- "Abortion"  

#####Merge and save files#####
pgsw_all <- bind_rows(pgsw1997, pgsw2001, pgsw2005, pgsw2007, pgsw2011, pgsw2015, pgsw2019)
write_stata(pgsw_all, path='/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW master.dta', version=14)
pgsw_all <- add_rows(pgsw1997, pgsw2001, pgsw2005, pgsw2007, pgsw2011, pgsw2015, pgsw2019)
save(pgsw_all, file = "/Users/benstanley/Google Drive/Resources/Datasets/Poland/PGSW_master.RData")

sjPlot::view_df(pgsw_all, show.id=FALSE, show.frq=TRUE, show.prc=TRUE, weight.by="weight", show.wtd.frq=TRUE, show.wtd.prc=TRUE, show.na=TRUE, use.viewer=FALSE)

save.image(file = "PGSW all.RData")
