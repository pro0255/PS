#//////////////////////////////////////////////////////////////////
#///////////////// Cvičení 12 - Testování hypotéz /////////////////
#//////  R-skript k Doplňujícím příkladům k online cvičení  ///////
#/////////////////      Mgr. Adéla Vrtková        /////////////////
#//////////////////////////////////////////////////////////////////

# Tento skript obsahuje pouze R-příkazy !!!
# Veškeré korektní zápisy a doprovodné komentáře jsou k dispozici v Poznámkách.

# R-příkazy nejsou samy o sobě uznatelným řešením! 
# Slouží případně pouze jako doplněk !!!

#//////////////////////////////////////////////////////////////////
# Příprava prostředí #####

library(readxl)
library(dplyr)
library(ggplot2)
library(moments)

data = read_excel("data_hraci2.xlsx")

# V každém příkladu si data uložíme do samostatné proměnné (data1, data2, data3, atd.)
# a budeme je upravovat na míru daného zadání. Do datasetu data zasahovat nebudeme, 
# tam zůstanou původní importovaná data.

#//////////////////////////////////////////////////////////////////
# Příklad 1 #####

#* a) ########################

# POZOR! V Př. 1a byla stanovena hladina významnosti 0,01 !!!

data1a = data

# Odlehlá pozorování
boxplot(data1a$odehrane_hod_2018 ~ data1a$system)
boxplot(data1a$odehrane_hod_2018 ~ data1a$system, plot = FALSE)

data1a$odehrane_hod_2018_out = data1a$odehrane_hod_2018
data1a$odehrane_hod_2018_out[data1a$system == "WIN" & data1a$odehrane_hod_2018 <= 160.2] = NA
data1a$odehrane_hod_2018_out[data1a$system == "OSX" & data1a$odehrane_hod_2018 == 320.0] = NA

# data1a$odehrane_hod_2018_out[data1a$system == "OSX" & near(data1a$odehrane_hod_2018, 320.0)] = NA

# Analýza předpokladů - normalita
boxplot(data1a$odehrane_hod_2018_out ~ data1a$system)

# data1a$system = factor(data1a$system, 
#                        levels = c("Linux", "OSX", "WIN"),
#                        labels = c("Linux", "OS X", "Windows"))

ggplot(data1a, aes(x = odehrane_hod_2018_out))+
  geom_histogram(bins = 12)+
  facet_grid("system")

ggplot(data1a, aes(sample = odehrane_hod_2018_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data1a$odehrane_hod_2018_out, data1a$system, moments::skewness, na.rm =T)
tapply(data1a$odehrane_hod_2018_out, data1a$system, moments::kurtosis, na.rm =T)-3

tapply(data1a$odehrane_hod_2018_out, data1a$system, shapiro.test)

# Analýza předpokladů -> normalita OK -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (největší ku nejmenšímu) < 2 ?
tapply(data1a$odehrane_hod_2018_out, data1a$system, var, na.rm =T)


#největší k nejmenšímu -> shoda nebude

# Exaktně! Test o shodě rozptylů - Bartlettův test (zobecnění dvouvýběrového F-testu)
bartlett.test(data1a$odehrane_hod_2018_out~data1a$system)

# Analýza předpokladů -> normalita OK, shoda rozptylů KO -> kontrola symetrie dat (když je normalita OK, pak je symetrie OK)
# Analýza předpokladů -> normalita OK, shoda rozptylů KO, symetrie OK -> Kruskalův-Wallisův test (test o shodě mediánů)

#Shoda o mediánech!
#Volíme test o shodě mediánů.

kruskal.test(data1a$odehrane_hod_2018_out~data1a$system)

# Zamítáme nulovou hypotézu o shodě mediánů - které se ale liší? -> Post-hoc analýza Dunnové metodou

library(FSA)
dunnTest(odehrane_hod_2018_out ~ system, data = data1a, method = "bonferroni")

# výběrové mediány (vodítko k seřazení)
tapply(data1a$odehrane_hod_2018_out, data1a$system, quantile, 0.5, na.rm = T)

# případně efekty - tj. rozdíl výběrového mediánu pro danou skupinu oproti celkovému mediánu
med_sk = tapply(data1a$odehrane_hod_2018_out, data1a$system, quantile, 0.5, na.rm = T)
med_total = quantile(data1a$odehrane_hod_2018_out, 0.5, na.rm = T)
med_sk - med_total


#tapply(data1a$)

#oproti celku méně, oproti celku více


#* b) ############################

# POZOR! V Př. 1b byla stanovena hladina významnosti 0,05 !!!

data1b = data

# Odlehlá pozorování
boxplot(data1b$odehrane_hod_2019 ~ data1b$system)
boxplot(data1b$odehrane_hod_2019 ~ data1b$system, plot = FALSE)

data1b$odehrane_hod_2019_out = data1b$odehrane_hod_2019
data1b$odehrane_hod_2019_out[data1b$system == "WIN" & data1b$odehrane_hod_2019 == 100.1] = NA
# data1b$odehrane_hod_2019_out[data1b$system == "WIN" & near(data1b$odehrane_hod_2019, 100.1)] = NA

boxplot(data1b$odehrane_hod_2019_out~ data1b$system)

# Analýza předpokladů - normalita
ggplot(data1b, aes(x = odehrane_hod_2019_out))+
  geom_histogram(bins = 10)+
  facet_grid("system")

ggplot(data1b, aes(sample = odehrane_hod_2019_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data1b$odehrane_hod_2019_out, data1b$system, moments::skewness, na.rm =T)
tapply(data1b$odehrane_hod_2019_out, data1b$system, moments::kurtosis, na.rm =T)-3

tapply(data1b$odehrane_hod_2019_out, data1b$system, shapiro.test)

# Analýza předpokladů -> normalita KO -> kontrola symetrie dat testem
library(lawstat)
tapply(data1b$odehrane_hod_2019_out, data1b$system, symmetry.test, boot = FALSE)

# Analýza předpokladů -> normalita KO, symetrie OK -> Kruskalův-Wallisův test (test o shodě mediánů)

#Volíme kruskalův wallisův test na test mediánu pro 3 systémy..

# Kdybychom přeci jen potřebovali otestovat shodu rozptylů, pak v případě dat, která nepochází z normálního
# rozdělení volíme Leveneho test (a samozřejmě empirické pravidlo o poměru výběrových rozptylů).
# Empiricky - poměr výběrových rozptylů (větší ku menšímu) < 2 ?
tapply(data1b$odehrane_hod_2019_out, data1b$system, var, na.rm =T)
4341.559 / 3132.480

# Exaktně - Leveneho test o shodě rozptylů pro data, která nepochází z normálního rozdělení
library(car)
leveneTest(data1b$odehrane_hod_2019_out ~ data1b$system)

# Analýza předpokladů -> normalita KO, symetrie OK -> Kruskalův-Wallisův test (test o shodě mediánů)
kruskal.test(data1b$odehrane_hod_2019_out ~ data1b$system)

# Nulovou hypotézu o shodě mediánů NEZAMÍTÁME - tzn. mediány se statisticky významně neliší.
# Všechny tři skupiny lze považovat za homogenní.

# V případě nezamítnutí H0 post-hoc analýzu neděláme!!! Jednoduše proto, že pokud rozdíly mezi skupinami nejsou,
# pak nemáme co post-hoc analýzou hledat.


#* c) ##########################

# POZOR! V Př. 1c byla stanovena hladina významnosti 0,01 !!!

data1c = data

data1c$narust = data1c$odehrane_hod_2019 - data1c$odehrane_hod_2018

# Odlehlá pozorování
boxplot(data1c$narust ~ data1c$system)
boxplot(data1c$narust ~ data1c$system, plot = F)

data1c$narust_out = data1c$narust
data1c$narust_out[data1c$system == "WIN" & data1c$narust == -238.4] = NA
# data1c$narust_out[data1c$system == "WIN" & near(data1c$narust, -238.4)] = NA

boxplot(data1c$narust_out ~ data1c$system)

# Analýza předpokladů - normalita
ggplot(data1c, aes(x = narust_out))+
  geom_histogram(bins = 10)+
  facet_grid("system")

ggplot(data1c, aes(sample = narust_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data1c$narust_out, data1c$system, moments::skewness, na.rm =T)
tapply(data1c$narust_out, data1c$system, moments::kurtosis, na.rm =T)-3

tapply(data1c$narust_out, data1c$system, shapiro.test)

# Analýza předpokladů -> normalita OK (hladina významnosti 0,01) -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (největší ku nejmenšímu) < 2 ?
tapply(data1c$narust_out, data1c$system, var, na.rm =T)

6161.436 / 4031.805

# Exaktně! Test o shodě rozptylů - Bartlettův test
bartlett.test(data1c$narust_out ~ data1c$system)

# Analýza předpokladů -> normalita OK, shoda rozptylů OK -> ANOVA (test o shodě středních hodnot)
vysledky = aov(data1c$narust_out ~ data1c$system)
summary(vysledky)

# Zamítáme nulovou hypotézu o shodě středních hodnot - které se ale liší? -> Post-hoc analýza
TukeyHSD(vysledky) #conf level 0.99

# výběrové průměry (vodítko k seřazení)
tapply(data1c$narust_out, data1c$system, mean, na.rm = T)

# případně efekty - tj. rozdíl výběrového průměru pro danou skupinu oproti celkovému průměru
prum_sk = tapply(data1c$narust_out, data1c$system, mean, na.rm = T)
prum_total = mean(data1c$narust_out, na.rm = T)
prum_sk - prum_total

#//////////////////////////////////////////////////////////////////
# Příklad 2 #####

# Příklad 1a) vedl na Kruskalův-Wallisův test (test o shodě mediánů)
# Doplňkem budou bodové a oboustranné intervalové odhady mediánů (Wilcoxonova statistika, symetrie OK)
# hladina významnosti je 0,01, tj. spolehlivost je 0,99

tapply(data1a$odehrane_hod_2018_out, data1a$system, quantile, probs = 0.5, na.rm = T)
tapply(data1a$odehrane_hod_2018_out, data1a$system, wilcox.test, conf.level = 0.99, conf.int = T)

tapply(data1a$odehrane_hod_2018_out, data1a$system, sd, na.rm = T) # kvůli zaokrouhlení

# Příklad 1b) vedl na Kruskalův-Wallisův test (test o shodě mediánů)
# Doplňkem budou bodové a oboustranné intervalové odhady mediánů (Wilcoxonova statistika, symetrie OK)
# hladina významnosti je 0,05, tj. spolehlivost je 0,95

tapply(data1b$odehrane_hod_2019_out, data1b$system, quantile, probs = 0.5, na.rm = T)
tapply(data1b$odehrane_hod_2019_out, data1b$system, wilcox.test, conf.level = 0.95, conf.int = T)

tapply(data1b$odehrane_hod_2019_out, data1b$system, sd, na.rm = T) # kvůli zaokrouhlení

# Příklad 1c) vedl na ANOVu (test o shodě stř. hodnot)
# Doplňkem budou bodové a oboustranné intervalové odhady stř. hodnot (jednovýb. t-test, normalita OK)
# hladina významnosti je 0,01, tj. spolehlivost je 0,99

tapply(data1c$narust_out, data1c$system, mean, na.rm = T)
tapply(data1c$narust_out, data1c$system, t.test, conf.level = 0.99)

tapply(data1c$narust_out, data1c$system, sd, na.rm = T) # kvůli zaokrouhlení
