#//////////////////////////////////////////////////////////////////
#///////////////// Cvičení 10 - Testování hypotéz /////////////////
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

# Příklady jsou vymyšleny tak, aby pokryly téměř všechny dílčí problémy, se kterými je možno se setkat.
# V praxi se samozřejmě na jedné datové sadě řeší pouze některé z nich v závislosti na cíli výzkumu.

#//////////////////////////////////////////////////////////////////
# Příklad 1 #####

# Vyfiltrování těch záznamů, potřebných k tomuto příkladu - pouze záznamy o hráčích s Linux.
data1 = data[data$system == "Linux", ]

# POZOR! V Příkladu 1 byla stanovena hladina významnosti 0,1 - tuto hladinu je nutné používat u všech testů,
# které budou v kontextu Příkladu 1 použity !!!

#* a) ####
# Odlehlá pozorování a analýza předpokladů (normalita)
boxplot(data1$odehrane_hod_2018)
hist(data1$odehrane_hod_2018)

qqnorm(data1$odehrane_hod_2018)
qqline(data1$odehrane_hod_2018)

moments::skewness(data1$odehrane_hod_2018)
moments::kurtosis(data1$odehrane_hod_2018)-3

# Shapirův - Wilkův test !!! Exaktní nástroj pro posouzení normality !!!
shapiro.test(data1$odehrane_hod_2018)

# Jednovýběrový t-test (H0: mu = 290, HA: mu != 290) - normalita OK
t.test(data1$odehrane_hod_2018, mu = 290, alternative = "two.sided", conf.level = 0.9)

# Je jiný? Ano je..


# Postup v případě porušení normality:
# V případě porušení normality by nešlo použít test o střední hodnotě a přešlo by se na test o mediánu
# Zápis hypotéz by musel být opraven - jiné značení testovaného parametru - stř. hodnota -> medián!

# Dále by bylo nutné posoudit, zda data jsou výběrem ze symetrického rozdělení -> test symetrie
# library(lawstat)

# symmetry.test(data1$odehrane_hod_2018, boot = FALSE)
# Pokud by byl předpoklad symetrie dat neporušen, pokračovali bychom jednovýběrovým Wilcoxonovým testem
# wilcox.test(data1$odehrane_hod_2018, mu = 290, alternative = "two.sided", conf.level = 0.9, conf.int = TRUE)
# Pokud by byl předpoklad symetrie porušen, pokračovali bychom jednovýběrovým znaménkovým testem
# library(BSDA)
# SIGN.test(data1$odehrane_hod_2018, md = 290, alternative = "two.sided", conf.level = 0.9)

#* b) ####
# Odlehlá pozorování a analýza předpokladů - viz a)

# Jednovýběrový t-test (H0: mu = 295, HA: mu > 295) - normalita OK
t.test(data1$odehrane_hod_2018, mu = 295, alternative = "greater", conf.level = 0.9)

#* c) ####
# Odlehlá pozorování a analýza předpokladů - viz a)

# Jednovýběrový t-test (H0: mu = 305, HA: mu < 305) - normalita OK
t.test(data1$odehrane_hod_2018, mu = 305, alternative = "less", conf.level = 0.9)

 #//////////////////////////////////////////////////////////////////
# Příklad 2 #####

# Vyfiltrování těch záznamů, potřebných k tomuto příkladu
data2 = data[data$system %in% c("Linux", "WIN"),]

data2$rozdil = data2$odehrane_hod_2019 - data2$odehrane_hod_2018

# POZOR! V Příkladu 2 byla stanovena hladina významnosti 0,05 - tuto hladinu je nutné používat u všech testů,
# které budou v kontextu Příkladu 2 použity !!!

#* a) ####
boxplot(data2$rozdil ~ data2$system)

#* b) ####
# Odlehlá pozorování
boxplot(data2$rozdil ~ data2$system)
boxplot(data2$rozdil ~ data2$system, plot = F)

data2$rozdil_out = data2$rozdil
data2$rozdil_out[data2$system == "WIN" & data2$rozdil == -238.4] = NA

boxplot(data2$rozdil_out ~ data2$system)

# Analýza předpokladů (normalita pro každý OS)
ggplot(data2, aes(x = rozdil_out))+
  geom_histogram(bins = 10)+
  facet_grid("system")

ggplot(data2, aes(sample = rozdil_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data2$rozdil_out, data2$system, moments::skewness, na.rm =T)
tapply(data2$rozdil_out, data2$system, moments::kurtosis, na.rm =T)-3

tapply(data2$rozdil_out, data2$system, shapiro.test)

# Jelikož nemáme specifickou teorii, kterou bychom chtěli testovat (např. zda došlo v průměru k meziročnímu 
# nárůstu/poklesu hraní), volíme oboustrannou alternativu. Pokud bychom takovou teorii dopředu měli, je možné
# volit i odpovídající jednostranné testy (např. viz vzorový úkol 2S).

# Jednovýběrový t-test pro každý OS (H0: mu = 0, HA: mu != 0) a odpovídající int. odhady - normalita OK pro oba OS
tapply(data2$rozdil_out, data2$system, t.test, mu = 0, alternative = "two.sided", conf.level = 0.95)

# Rozsahy výběrů a sm. odchylky kvůli zaokrouhlení IO pro každý OS a bodové odhady pro doplnění int. odhadů
data2 %>% 
  group_by(system) %>% 
  summarise(rozsah = length(na.omit(rozdil_out)),
            sm_odch = sd(rozdil_out, na.rm = T),
            bod_odhad = mean(rozdil_out, na.rm = T))

# Pokud by byl předpoklad normality porušen (stačí porušení pro jednu skupinu), pak bychom s oběma skupinami 
# šli na testování mediánu (a odpovídající bodové a intervalové odhady). 
# Zápis hypotéz a IO by musel být opraven - jiné značení testovaného/odhadovaného parametru - stř. hodnota -> medián!
# Bylo by nutné ještě posoudit, zda data jsou výběrem ze symetrického rozdělení -> test symetrie
# library(lawstat)
# tapply(data2$rozdil_out, data2$system, symmetry.test, boot = FALSE)
# Pokud by byl předpoklad symetrie dat neporušen (pro obě skupiny), pokračovali bychom jednovýběrovým Wilcoxonovým testem
# a odpovídajícím intervalový odhadem.
# tapply(data2$rozdil_out, data2$system, wilcox.test, mu = 0, alternative = "two.sided", conf.level = 0.95, conf.int = T)
# Pokud by byl předpoklad symetrie porušen (alespoň u jedné skupiny), pokračovali bychom jednovýběrovým znaménkovým testem
# s odpovídajícím intervalovým odhadem.
# library(BSDA)
# tapply(data2$rozdil_out, data2$system, SIGN.test, md = 0, alternative = "two.sided", conf.level = 0.95)
# V obou výše uvedených případech bychom bodový odhad - tj. výběrový medián - získali pomocí funkce quantile.
# tapply(data2$rozdil_out, data2$system, quantile, probs = 0.5, na.rm = T)


#//////////////////////////////////////////////////////////////////
# Příklad 3 #####

data3 = data

# Definování dichotomické proměnné
data3$hodiny_dich_2018 = ifelse(data3$odehrane_hod_2018>280, "Ano", "Ne")

# Získání potřebných četností
table(data3$hodiny_dich_2018)

n = 176+163
x = 176
p = x/n
p

# Ověření předpokladů
n > 9/(p*(1-p))

# Jednovýběrový test o parametru binomického rozdělení (H0: pi = 0.5, HA: pi > 0.5)
binom.test(x, n, 0.5, alternative = "greater", conf.level = 0.95)



