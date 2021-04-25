#//////////////////////////////////////////////////////////////////
#///////////////// Cvičení 11 - Testování hypotéz /////////////////
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

# Odpovídá Příkladu 3 v prezentaci k Intervalovým odhadům (Cviceni09_skript_Int_odhady.R)

# POZOR! V Příkladu 1 byla stanovena hladina významnosti 0,05 - tuto hladinu je nutné používat u všech testů,
# které budou v kontextu Příkladu 1 použity !!!

#* a) ####
data1a = data[data$system %in% c("WIN", "Linux"),]

# Odlehlá pozorování
boxplot(data1a$odehrane_hod_2018 ~ data1a$system)
boxplot(data1a$odehrane_hod_2018 ~ data1a$system, plot = FALSE)

data1a$odehrane_hod_2018_out = data1a$odehrane_hod_2018
data1a$odehrane_hod_2018_out[data1a$system == "WIN" & data1a$odehrane_hod_2018 <= 160.2] = NA

# Analýza předpokladů - normalita
boxplot(data1a$odehrane_hod_2018_out ~ data1a$system)

ggplot(data1a, aes(x = odehrane_hod_2018_out))+
  geom_histogram(bins = 10)+
  facet_grid("system")

ggplot(data1a, aes(sample = odehrane_hod_2018_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data1a$odehrane_hod_2018_out, data1a$system, moments::skewness, na.rm =T)
tapply(data1a$odehrane_hod_2018_out, data1a$system, moments::kurtosis, na.rm =T)-3

tapply(data1a$odehrane_hod_2018_out, data1a$system, shapiro.test)

# Analýza předpokladů -> normalita OK -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (větší ku menšímu) < 2 ?
tapply(data1a$odehrane_hod_2018_out, data1a$system, var, na.rm =T)

1796.7560 / 547.7347 #vice jak trojnasobny

# Exaktně! Test o shodě rozptylů - F-test
# Touto strukturou se hůře hlídá "co k čemu" se vztahuje - pokud potřebuji i IO, pak rozhodně nedoporučuji
# Pokud potřebuji jen p-hodnotu, tak klidně
var.test(data1a$odehrane_hod_2018_out~data1a$system)

# Srovnejte!
var.test(data1a$odehrane_hod_2018_out~data1a$system)

var.test(data1a$odehrane_hod_2018_out[data1a$system == "WIN"], data1a$odehrane_hod_2018_out[data1a$system == "Linux"])

var.test(data1a$odehrane_hod_2018_out[data1a$system == "Linux"], data1a$odehrane_hod_2018_out[data1a$system == "WIN"])

# Analýza předpokladů -> normalita OK, shoda rozptylů KO -> Aspinové-Welchův test

# Aspinové-Welchův test (H0: mu_W = mu_L, HA: mu_W !=  mu_L) odpovídá (H0: mu_W - mu_L = 0, HA: mu_W - mu_L != 0)
t.test(data1a$odehrane_hod_2018_out[data1a$system=="WIN"], 
       data1a$odehrane_hod_2018_out[data1a$system=="Linux"], 
       alternative = "two.sided", 
       var.equal = FALSE,     # shoda rozptylů KO
       conf.level = 0.95)


t.test(data1a$odehrane_hod_2018_out[data1a$system=="Linux"], 
       data1a$odehrane_hod_2018_out[data1a$system=="WIN"], 
       alternative = "two.sided", 
       var.equal = FALSE,     # shoda rozptylů KO
       conf.level = 0.95)

#* b) ####
data1b = data[data$system %in% c("OSX", "Linux"),]

# Odlehlá pozorování
boxplot(data1b$odehrane_hod_2018 ~ data1b$system)
boxplot(data1b$odehrane_hod_2018 ~ data1b$system, plot = FALSE)

data1b$odehrane_hod_2018_out = data1b$odehrane_hod_2018
data1b$odehrane_hod_2018_out[data1b$system == "OSX" & data1b$odehrane_hod_2018 == 320] = NA

boxplot(data1b$odehrane_hod_2018_out~ data1b$system)

# Analýza předpokladů - normalita
ggplot(data1b, aes(x = odehrane_hod_2018_out))+
  geom_histogram(bins = 10)+
  facet_grid("system")

ggplot(data1b, aes(sample = odehrane_hod_2018_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data1b$odehrane_hod_2018_out, data1b$system, moments::skewness, na.rm =T)
tapply(data1b$odehrane_hod_2018_out, data1b$system, moments::kurtosis, na.rm =T)-3

tapply(data1b$odehrane_hod_2018_out, data1b$system, shapiro.test)

# Analýza předpokladů -> normalita OK -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (větší ku menšímu) < 2 ?
tapply(data1b$odehrane_hod_2018_out, data1b$system, var, na.rm =T)

791.3980 / 547.7347

# Exaktně! Test o shodě rozptylů - F-test
# Touto strukturou se hůře hlídá "co k čemu" se vztahuje - pokud potřebuji i IO, pak rozhodně nedoporučuji
# Pokud potřebuji jen p-hodnotu, tak klidně
var.test(data1b$odehrane_hod_2018_out~data1b$system)
# lépe - kontrolovaně
var.test(data1b$odehrane_hod_2018_out[data1b$system == "OSX"], data1b$odehrane_hod_2018_out[data1b$system == "Linux"])

# Analýza předpokladů -> normalita OK, shoda rozptylů OK -> Dvouvýběrový t-test

# Dvouvýběrový t-test (H0: mu_L = mu_O, HA: mu_L >  mu_O) odpovídá (H0: mu_L - mu_O = 0, HA: mu_L - mu_O > 0)
t.test(data1b$odehrane_hod_2018_out[data1b$system=="Linux"], 
       data1b$odehrane_hod_2018_out[data1b$system=="OSX"], 
       alternative = "greater", 
       var.equal = TRUE,     # shoda rozptylů OK
       conf.level = 0.95)


#t.test(data1b$odehrane_hod_2018_out[data1b$system=="OSX"], 
 #      data1b$odehrane_hod_2018_out[data1b$system=="Linux"], 
  #     alternative = "less", 
   #    var.equal = TRUE,     # shoda rozptylů OK
    #   conf.level = 0.95)

#* c) ####
data1c = data[data$system %in% c("OSX", "Linux"),]

# Odlehlá pozorování
boxplot(data1c$odehrane_hod_2019 ~ data1c$system)

# Analýza předpokladů - normalita
ggplot(data1c, aes(x = odehrane_hod_2019))+
  geom_histogram(bins = 10)+
  facet_grid("system")

ggplot(data1c, aes(sample = odehrane_hod_2019))+
  stat_qq()+ 
  stat_qq_line()+
  facet_grid("system")

tapply(data1c$odehrane_hod_2019, data1c$system, moments::skewness, na.rm =T)
tapply(data1c$odehrane_hod_2019, data1c$system, moments::kurtosis, na.rm =T)-3

tapply(data1c$odehrane_hod_2019, data1c$system, shapiro.test)

# Analýza předpokladů -> normalita KO -> přechod na mediány!
# Kontrola, zda výběry mají stejný tvar rozdělení (histogramy, míry tvaru)
# Nesmí být např. jedno rozdělení výrazně zešikmené doleva a druhé doprava.

# Analýza předpokladů -> normalita KO, stejný tvar OK -> Mannův-Whitneyho test

# Kvůli přehlednosti je zde medián značen jen symbolicky x (aby byl odlišitelný od stř. hodnoty mu)
# Mannův-Whitneyho test (H0: x_L = x_O, HA: x_L >  x_O) odpovídá (H0: x_L - x_O = 0, HA: x_L - x_O > 0)
wilcox.test(data1c$odehrane_hod_2019[data1c$system=="Linux"], 
            data1c$odehrane_hod_2019[data1c$system=="OSX"],
            alternative = "greater", 
            conf.level = 0.95,
            conf.int = T)

# Uvědomme si, že výše uvedené také odpovídá:
# Mannův-Whitneyho test (H0: x_O = x_L, HA: x_O <  x_L) odpovídá (H0: x_O - x_L = 0, HA: x_O - x_L < 0)
wilcox.test(data1c$odehrane_hod_2019[data1c$system=="OSX"], 
            data1c$odehrane_hod_2019[data1c$system=="Linux"], 
            alternative = "less", 
            conf.level = 0.95,
            conf.int = T)


# POZOR! V interpretaci musíme respektovat to, že jsme museli přejít na mediány!

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

# Analýza předpokladů -> normalita OK -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (větší ku menšímu) < 2 ?
tapply(data2$rozdil_out, data2$system, var, na.rm =T)

# Exaktně! Test o shodě rozptylů - F-test
# Touto strukturou se hůře hlídá "co k čemu" se vztahuje - pokud potřebuji i IO, pak rozhodně nedoporučuji
# Pokud potřebuji jen p-hodnotu, tak klidně
var.test(data2$rozdil_out~data2$system)
# lépe - kontrolovaně
var.test(data2$rozdil_out[data2$system == "WIN"], data2$rozdil_out[data2$system == "Linux"])

# Analýza předpokladů -> normalita OK, shoda rozptylů OK -> Dvouvýběrový t-test

# Jelikož nemáme specifickou teorii, kterou bychom chtěli testovat,
# volíme oboustrannou alternativu. Pokud bychom takovou teorii dopředu měli, je možné
# volit i odpovídající jednostrannou alternativu (analytik si volbu alternativy musí obhájit).

# Dvouvýběrový t-test (H0: mu_W - mu_L = 0, HA: mu_W - mu_L != 0)
t.test(data2$rozdil_out[data2$system == "WIN"], 
       data2$rozdil_out[data2$system == "Linux"],
       alternative = "two.sided", 
       var.equal = TRUE, 
       conf.level = 0.95)

# Ještě bodový odhad rozdílu středních hodnot -> rozdíl výběrových průměrů
tapply(data2$rozdil_out, data2$system, mean, na.rm =T)
mean(data2$rozdil_out[data2$system == "WIN"], na.rm = T) - mean(data2$rozdil_out[data2$system == "Linux"], na.rm = T)

# a rozsahy a výběrové sm. odchylky kvůli zaokrouhlení.
data2 %>% 
  group_by(system) %>% 
  summarise(rozsah = length(na.omit(rozdil_out)),
            sm_odch = sd(rozdil_out, na.rm = T))

# Teď už vše jen sepsat a okomentovat.


#//////////////////////////////////////////////////////////////////
# Příklad 3 #####

# Odpovídá Příkladu 4b v prezentaci k Intervalovým odhadům (Cviceni09_skript_Int_odhady.R)

data3 = data[data$system %in% c("WIN", "Linux"),]

# Definování dichotomické proměnné
data3$hodiny_dich_2018 = ifelse(data3$odehrane_hod_2018>280, "Ano", "Ne")

# Získání potřebných četností
table(data3$system, data3$hodiny_dich_2018)

# Linux
n_L = 42+16
x_L = 42
p_L = x_L/n_L

# Windows
n_W = 132+70
x_W = 132
p_W = x_W/n_W

# Ověření předpokladů
n_L > 9/(p_L*(1-p_L))
n_W > 9/(p_W*(1-p_W))

# Test shody parametrů dvou binomických rozdělení
# (H0: pi_W = pi_L, HA: pi_W > pi_L) odpovídá (H0: pi_W - pi_L = 0, HA: pi_W - pi_L > 0)
prop.test(c(x_W, x_L), c(n_W,n_L), alternative = "greater", conf.level = 0.85)

# Uvědomme si, že výše uvedené také odpovídá:
# (H0: pi_L = pi_W, HA: pi_L < pi_W) odpovídá (H0: pi_L - pi_W = 0, HA: pi_L - pi_W < 0)
prop.test(c(x_L, x_W), c(n_L,n_W), alternative = "less", conf.level = 0.85)





