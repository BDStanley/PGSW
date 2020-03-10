#Prepare workspace
rm(list=ls())
library(plyr); library(tidyverse); library(sjlabelled); library(labelled); library(scales); 
library(statar); library(lavaan); library(poLCA); library(sjPlot); library(googledrive); library(rio)

#Download and read data
import <- drive_download(as_id('https://drive.google.com/file/d/1Y7kjTpQkKyN_E0Ogj2yUfJu9gF4Dk98K/view?usp=sharing'), overwrite=TRUE)
1
read <- import('PGSW2019_CAPI.sav')
pgsw2019 <- tibble(1:2003)

colnames(pgsw2019) <- "n"
var_label(pgsw2019$n) <- "ID"

pgsw2019$rok <- 2019
pgsw2019$rok <- as_factor(pgsw2019$rok)
var_label(pgsw2019$rok) <- "Rok"

pgsw2019$waga <- read$waga
var_label(pgsw2019$waga) <- "Waga"

pgsw2019$Q04_5 <- read$Q04_5 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Posiadanie silnego lidera w rządzie jest dobre dla Polski, nawet jeśli lider nagina zasady, aby załatwić ważne sprawy.")

pgsw2019$Q05_1 <- read$Q05_1 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Mniejszości powinny dostosować się do zwyczajów i tradycji panujących w Polsce.")

pgsw2019$Q05_2 <- read$Q05_2 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Wola większości powinna zawsze przeważać, nawet kosztem praw mniejszości.")

pgsw2019$Q05_3 <- read$Q05_3 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Imigranci zazwyczaj stanowią korzyść dla polskiej gospodarki.")

pgsw2019$Q05_4 <- read$Q05_4 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Obecność imigrantów szkodzi polskiej kulturze.")

pgsw2019$Q05_5 <- read$Q05_5 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Imigranci przyczyniają się do wzrostu poziomu przestępczości w Polsce.")

pgsw2019$Q06_1 <- read$Q06_1 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9)

pgsw2019$Q06_2 <- read$Q06_2 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9)

pgsw2019$Q06_3 <- read$Q06_3 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9)

pgsw2019$Q06_4 <- read$Q06_4 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9)

pgsw2019$Q06_5 <- read$Q06_5 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9)

pgsw2019$Q23 <- read$Q23 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) czy też nie zgadza ze stwierdzeniem: 'W demokracji są problemy, ale jest to lepszy system rządzenia niż każdy inny'?")

pgsw2019$N8_1 <- read$N8_1 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między bogatymi a biednymi.")

pgsw2019$N8_2 <- read$N8_2 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między młodymi a starszymi.")

pgsw2019$N8_3 <- read$N8_3 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między wierzącymi a niewierzącymi.")

pgsw2019$N8_4 <- read$N8_4 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między kobietami a mężczyznami.")

pgsw2019$N8_5 <- read$N8_5 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między pracownikami a pracodawcami.")

pgsw2019$N8_6 <- read$N8_6 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między pracownikami sektora publicznego a sektora prywatnego.")

pgsw2019$N8_7 <- read$N8_7 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między wykształconymi a niewykształconymi.")

pgsw2019$N8_8 <- read$N8_8 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między klasą robotniczą a klasą średnią.")

pgsw2019$N8_9 <- read$N8_9 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między rolnikami a ludnością miejską.")

pgsw2019$N8_10 <- read$N8_10 %>%
  set_label(., "Czy istnieje, a jeśli tak, to jak silny, konflikt między następującymi grupami w Polsce? Między polskimi patriotami a ludźmi nie szanującymi Polski.")

pgsw2019$P19_1 <- read$P19_1 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) z stwierdzeniem: 'Polsce potrzebny jest ktoś, kto będzie miał dość siły, by zmienić całkowicie nasz system władzy i zaprowadzić nowy, sprawiedliwy ład i porządek'?")

pgsw2019$P19_2 <- read$P19_2 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) z stwierdzeniem: 'W Polsce nieliczni zawłaszczają władzę, przynależną zwykłym ludziom'?")

pgsw2019$P19_3 <- read$P19_3 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) z stwierdzeniem: 'Jest ostatnia chwila, by uchronić Polskę przed grożącą katastrofą'?")

pgsw2019$P19_4 <- read$P19_4 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) z stwierdzeniem: 'W Polsce nic się nie poprawi, dopóki politycy nie powrócą do starych, dobrych wartości moralnych'?")

pgsw2019$P19_5 <- read$P19_5 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) z stwierdzeniem: 'Rozwiązanie problemów naszego kraju jest bardzo prostą rzeczą, trzeba tylko dać władzę tym, którzy będą chcieli tego dokonać'?")

pgsw2019$P19_6 <- read$P19_6 %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Czy zgadza się Pan(i) z stwierdzeniem: 'Wszystko w polityce jest jednoznacznie dobre lub złe, wybór jest jasny'?")

pgsw2019$I1b <- read$I1b %>%
  na_if(., 97) %>%
  set_label(., "Każdy powinien płacić w formie podatku taki sam procent od swoich dochodów.")

pgsw2019$I1d <- read$I1d %>%
  na_if(., 97) %>%
  set_label(., "Władzę powinny przeciwdziałać osiedlaniu się w Polsce cudzoziemców.")

pgsw2019$I1f <- read$I1f %>%
  na_if(., 97) %>%
  set_label(., "Pary dwóch osób tej samej płci nie powinny mieć prawa do publicznego pokazywania swojego sposobu życia.")

pgsw2019$I1g <- read$I1g %>%
  na_if(., 97) %>%
  set_label(., "Kobieta - niezależnie od swej sytuacji społecznej czy zdrowotnej - nie powinna mieć prawa do aborcji.")

pgsw2019$D9_9 <- read$D9_9 %>%
  na_if(., 96) %>%
  set_label(., "Proszę powiedzieć, na ile, Pana(i) zdaniem, dla demokracji generalnie ważne jest żeby ostateczna interpretacja prawa należała do autorytetów religijnych.")

pgsw2019$D10_10 <- read$D10_10 %>%
  na_if(., 96) %>%
  set_label(., "Proszę powiedzieć, na ile, Pana(i) zdaniem, dla demokracji generalnie ważne jest żeby ludzie byli posłuszni władzy.")

pgsw2019$D21 <- read$D21 %>%
  set_label(., "Wolność czy równość?")

pgsw2019$D22 <- read$D22 %>%
  set_label(., "Tradycyjne wartości czy postęp społeczny i nowoczesność?")

pgsw2019$P24 <- read$D23 %>%
  set_label(., "Solidarność społeczna czy korzystne warunki dla przedsiębiorczości ludzi?")

pgsw2019$H10_1 <- read$H10_1 %>%
  na_if(., 7) %>%
  set_label(., "To nie rząd nami rządzi, ci, co naprawdę nami sterują, nie są w ogóle znani.")

pgsw2019$H10_2 <- read$H10_2 %>%
  na_if(., 7) %>%
  set_label(., "Ci, którzy twierdzą, że istnieją na świecie potężne, ukryte siły spiskujące przeciwko Polsce, mają wiele racji.")

pgsw2019$H10_3 <- read$H10_3 %>%
  na_if(., 7) %>%
  set_label(., "Najważniejszą rzeczą, której trzeba nauczyć dzieci, jest całkowite posłuszeństwo wobec rodziców.")

pgsw2019$H10_4 <- read$H10_4 %>%
  na_if(., 7) %>%
  set_label(., "Każdy dobry kierownik, aby uzyskać uznanie i posłuch, powinien być surowy i wymagający wobec ludzi, którzy mu podlegają.")

pgsw2019$H10_5 <- read$H10_5 %>%
  na_if(., 7) %>%
  set_label(., "Jak się dobrze zastanowić, to trzeba przyznać, że istnieją na świecie tylko dwa typy ludzi: silni i słabi.")

pgsw2019$H10_6 <- read$H10_6 %>%
  na_if(., 7) %>%
  set_label(., "W tym skomplikowanym świecie, jedynym sposobem na to, aby rozeznać się jak należy postępować, jest zdanie się na specjalistów i doradców.")

pgsw2019$H10_7 <- read$H10_7 %>%
  na_if(., 7) %>%
  set_label(., "Coraz trudniej dopatrzyć się jakiegoś sensu w otaczającym nas świecie.")

pgsw2019$H10_8 <- read$H10_8 %>%
  na_if(., 7) %>%
  set_label(., "Jedyne czego można być obecnie pewnym, to tego, że niczego nie można być pewnym.")

pgsw2019$H10_9 <- read$H10_9 %>%
  na_if(., 7) %>%
  set_label(., "Przy istnieniu tak wielu różnych idei, teorii i światopoglądów, często już nie wiadomo, w co wierzyć.")

pgsw2019$H10_10 <- read$H10_10 %>%
  na_if(., 7) %>%
  set_label(., "W naszym obecnym systemie społeczno-politycznym czuję się coraz bardziej obco i nieswojo.")

pgsw2019$H10_11 <- read$H10_11 %>%
  na_if(., 7) %>%
  set_label(., "Często mam poczucie, że obecna polityka władz jest moją polityką.")

pgsw2019$H10_12 <- read$H10_12 %>%
  na_if(., 7) %>%
  set_label(., "Wszyscy jesteśmy nic nie znaczącymi kółkami w machinie polityki.")

pgsw2019$K1_d <- read$K1_d %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Szkoła musi uczyć dzieci posłuszeństwa wobec autorytetów.")

pgsw2019$K1_e <- read$K1_e %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Ludzie, którzy łamią prawo powinni być karani znacznie surowszymi wyrokami niż ma to miejsce dziś.")

pgsw2019$K1_f <- read$K1_f %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Policja powinna mieć prawo do trzymania w areszcie osoby podejrzanej o planowanie ataku terrorystycznego w Polsce tak długo aż nie ma pewności że jest ona niewinna.")

pgsw2019$K1_g <- read$K1_g %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Ludzie z krajów Europy Zachodniej powinni mieć prawo do swobodnego osiedlania się i pracy w Polsce.")

pgsw2019$K1_h <- read$K1_h %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Ludzie z krajów Europy Wschodniej powinni mieć prawo do swobodnego osiedlania się i pracy w Polsce.")

pgsw2019$K1_i <- read$K1_i %>%
  na_if(., 7) %>%
  na_if(., 8) %>%
  na_if(., 9) %>%
  set_label(., "Ludzie z biednych krajów spoza Europy powinni mieć prawo do swobodnego osiedlania się i pracy w Polsce.")

save.image(file = "PGSW2019_CAPI_PL.RData")

labels <- get_label(pgsw2019)
pgsw2019 <- factorize(pgsw2019) %>%
  droplevels()
pgsw2019 <- set_label(pgsw2019, label=labels)
export(pgsw2019, "PGSW2019_CAPI_PL.dta")

