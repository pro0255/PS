#//////////////////////////////////////////////////////////////////////////////////////
##############    Explorační analýza dat - Grafika s ggplot2      #####################
################       Adéla Vrtková, Martina Litschmannová    ########################
#//////////////////////////////////////////////////////////////////////////////////////

# Zobrazuje-li se vám skript s chybným kódováním, využijte příkaz File / Reopen with Encoding

# Základní skript obsahuje nezbytné minimum potřebné ke zvládnutí principů práce
# s jazykem R v kontextu Explorační analýzy dat.

# Rozšířený skript je obohacen o ukázky použití knihovny dplyr pro jednodušší práci
# s datovým souborem a knihovny ggplot2 pro hezčí grafiku.

# Lehkou nádstavbu k Základnímu skriptu tvoří skript "Grafika s ggplot2",
# kde jsou vytaženy ukázky grafiky s tímto balíčkem. Ač to tak na první pohled nepůsobí,
# ggplot2 ve výsledku dokáže ušetřit spousty práce a času při vytváření pěkných grafů
# pro domácí úkoly, závěrečné práce, diplomky apod.

#/////////////////////////////////////////////////////////////////////////////////////
## Příprava prostředí ##########

# Aktivace potřebných knihoven, případně doinstalujte pomocí install.packages()
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Import dat
dataS = as.data.frame(
        read_excel("aku.xlsx", 
                  sheet = "Standard",
                  skip = 2)
        )
colnames(dataS) = c("ID", "kap5", "kap100", "vyrobce")

#/////////////////////////////////////////////////////////////////////////////////////
## Vysvětlení principu práce s knihovnou ggplot2 ##########

# Základní fungování ggplot2 - pomocí vrstev:
# Nejprve definujeme "estetiku" (aesthetics)
#           - důležitá část, kde specifikujeme proměnnou na ose x a/nebo na ose y
#           - lze ale i určit proměnnou, která ovlivní velikost (size) nebo barvu (color) vykreslených objektů (např. bodů)
# Následuje určení "geometrie" (geometries)
#           - tato část definuje, jak se mají data znázornit
#           - jako body (geom_point), čáry (geom_line), krabicové grafy (geom_boxplot), sloupcové grafy (geom_bar),...
#           - je třeba uvážit typ dat a na základě toho, jakou chceme informaci předat, zvolit geometrii
#           - různé "geom" lze i kombinovat, má-li to smysl
# Samozřejmostí je změna popisků pomocí vrstvy "labs", grafická témata (themes), přidání textu (geom_text) apod.
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

# Ukládání grafů lze např. pomocí funkce dev.print, jpeg, pdf a dalších.
# Jednodušeji pak v okně Plots -> Export

# Pro dobrou orientaci jsou kapitoly číslovány dle ZÁKLADNÍHO skriptu.

#/////////////////////////////////////////////////////////////////////////////////////
#* 5.2 Sloupcový graf (ggplot2) ##########

# Sloupcový graf pomocí ggplot2 - ze std. datového formátu
ggplot(dataS, 
       aes(x = vyrobce))+  # estetika
  geom_bar()

# V tuto chvíli je na zvážení seřazení výrobců dle četnosti (od nejčetnějšího).
dataS$vyrobce = factor(dataS$vyrobce,
                       levels = names(sort(table(dataS$vyrobce), decreasing = TRUE))
                       )

# Další parametry je ale jednodušší nastavit, když je graf vytvořen z tabulky, která obsahuje 
# absolutní a relativní četnosti.

# Takovou tabulku lze připravit pomocí funkcí knihovny dplyr
tab_cetnosti_dplyr = 
  dataS %>%
  group_by(vyrobce) %>%
  summarise(cetnost = n()) %>%                                  
  mutate(rel.cetnost = round(100*(cetnost / sum(cetnost)), 1))    # příprava tabulky
tab_cetnosti_dplyr[1,3] = 100 - sum((tab_cetnosti_dplyr[2:4, 3])) # ohlídání zaokrouhlovací chyby
tab_cetnosti_dplyr

# Nebo také pomocí funkcí základního R
cetnosti = table(dataS$vyrobce)
rel.cetnosti = round(prop.table(cetnosti)*100, digits = 1)
rel.cetnosti[4] = 100 - sum(rel.cetnosti[1:3]) # ohlídání zaokrouhlovací chyby
tab_cetnosti = as.data.frame(cbind(cetnosti, rel.cetnosti))  # spojení tabulek a převod na data.frame
tab_cetnosti
tab_cetnosti$vyrobce = rownames(tab_cetnosti) # vytažení výrobce do samostatné proměnné
tab_cetnosti

# Vytvoření grafu
ggplot(tab_cetnosti_dplyr, 
       aes(x = vyrobce,        # estetika
           y = cetnost))+      
  geom_bar(stat = "identity")    # parametr stat nastavený kvůli vstupní tabulce

# Můžeme si vyhrát s dalšími grafickými parametry a nastavením popisků 
ggplot(tab_cetnosti_dplyr, 
       aes(x = vyrobce, 
           y = cetnost))+  # estetika
  geom_bar(stat = "identity",         # parametr stat nastavený kvůli tabulce
           fill = "darkolivegreen3",  # barva sloupců
           width = 0.7)+              # šířka sloupců
  labs(x = "Výrobce",                 # názvy os a název grafu
       y = "Četnost",
       title = "Struktura souboru dle výrobce")+
  theme_bw()+                                   # vzhled grafu (další např. theme_classic, theme_dark, theme_light,...)
  theme(plot.title = element_text(hjust = 0.5),                  # zarovnání názvu grafu
        axis.text = element_text(color = "black", size = 13),    # barva a velikost popisků os
        axis.title = element_text(size = 13))+                   # velikost názvů os
  geom_text(aes(y = cetnost, 
                label = paste0(cetnost," (", rel.cetnost," %)")),     # jaký text a kam umístit
            size = 6,                                   # velikost textu
            position = position_stack(vjust = 0.5))+  # nastavení pozice na střed sloupce
  coord_flip()  # horizontální orientace

# Mimo klasického sloupcového grafu lze sestrojit i 100% skládaný sloupcový graf, což je perfektní alternativa ke koláčovým grafům.

ggplot(tab_cetnosti_dplyr, 
       aes(x = "",
           y = rel.cetnost,
           fill = vyrobce))+  # jiné nastavení estetiky
  geom_bar(stat="identity")+
  labs(x = "",
       y = "Kumulativní relativní četnost (%)",
       fill = "Výrobce",
       title = "Struktura souboru dle výrobce")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),                  # zarovnání názvu grafu
        axis.text = element_text(color = "black", size = 13),    # barva a velikost popisků os
        axis.title = element_text(size = 13))+
  geom_text(aes(y = rel.cetnost, 
                label = paste0(cetnost," (", rel.cetnost," %)")),
            size = 5,
            position = position_stack(vjust = 0.5))        # parametr kvůli správnému vykreslení popisků

# Zkuste využít předešlého kódu a vytvořit si sloupcový graf pro proměnnou Výrobce podle sebe.

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.2 Krabicový graf (ggplot2) ##########

ggplot(dataS,
       aes(x = "",
           y = kap5))+   # estetika
  geom_boxplot()   # specifikace způsobu vykreslení - boxplot

# Chceme-li vykreslit pouze pro jednoho výrobce a nechceme si definovat další nový objekt, lze to udělat následovně
ggplot(dataS[dataS$vyrobce=="A",],
       aes(x = "",
           y = kap5))+ # estetika
  geom_boxplot()

# ... a graficky si pohrát s výstupem
ggplot(dataS[dataS$vyrobce=="A",],
       aes(x = "",
           y = kap5))+ # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "",
       y = "(mAh)",
       title = "Kapacita po 5 cyklech (mAh), výrobce A")+  # popisky
  theme_light()+
  geom_point(aes(x = "",
                 y = mean(dataS$kap5[dataS$vyrobce=="A"], na.rm=T)),  # vykreslení průměru
             color = "red",
             shape = 3)

# Vícenásobný krabicový graf v ggplot2
ggplot(dataS,
       aes(x = vyrobce,
           y = kap5))+ # estetika
  geom_boxplot()+
  labs(x = "",
       y = "(mAh)",
       title = "Kapacita po 5 cyklech (mAh) podle výrobce") # popisky

# ... s trochou grafiky
ggplot(dataS,
       aes(x = vyrobce,
           y = kap5))+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "",
       y = "(mAh)",
       title = "Kapacita po 5 cyklech (mAh) podle výrobce")+ # popisky
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

# Pro vykreslení vícenásobného krabicového grafu je na místě zvážit seřazení výrobců dle abecedy.

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.3 Histogram (ggplot2) ##########

ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(binwidth = 20)   # Lze nastavit parametr bins (počet všech sloupců) nebo binwidth (šířka jednoho sloupce)

#...s pokročilou úpravou výstupu
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(binwidth = 40,
                 color = "grey",
                 fill = "lightblue")+
  labs(x = "kapacita (mAh)",
       y = "četnost",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

#... se změnou měřítka osy y a přidáním empirické hustoty
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 binwidth = 40,
                 color = "grey",           # barva ohraničení
                 fill = "lightblue")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

# s přidáním hustoty pravděpodobnosti normálního rozdělení pomocí stat_function()
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 binwidth = 40,
                 color = "grey",           # barva ohraničení
                 fill = "lightblue")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  stat_function(fun = dnorm,
                args = list(mean = mean(dataS$kap5), sd = sd(dataS$kap5)),
                color = "red")+      # Gaussovka
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.4 QQ-graf (ggplot2) ##########

ggplot(dataS, 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()

# Výstup ggplot lze upravit analogicky předchozím ggplot výstupům.

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.5 Kombinace grafických výstupů do jednoho obrázku (ggplot2) ##########

# Kombinaci více objektů z ggplot vyžaduje jiný postup
# Jednotlivé objekty si uložit a na konci zavolat funkci ggarrange z balíčku ggpubr

hist_kap5 =
  ggplot(dataS,
         aes(x = kap5))+
  geom_histogram(binwidth = 40,
                 color = "grey",
                 fill = "lightgreen")+
  labs(x = " ",
       y = "četnost")+
  theme_test()

box_kap5 =
  ggplot(dataS,
         aes(x = "",
             y = kap5))+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   
  geom_boxplot()+
  labs(x = "\n",
       y = "kapacita (mAh)")+
  theme_test()+
  coord_flip()

ggarrange(hist_kap5, box_kap5,  # vypsání objektů k vykreslení
          ncol = 1,   # počet sloupců, řádky jsou dopočítány automaticky
          heights = c(2.5,1))
# Návrh na úpravu - k dokonalosti chybí sjednotit měřítka na osách x, případně zvětšit popisky pro lepší čitelnost hodnot

# Pokud je však cílem SROVNÁNÍ, pak není vhodné výše uvedené generovat pro každého výrobce zvlášť.
# V takovém případě je dobré sáhnout po jednom vícenásobném krabicovém grafu 
# a k němu vytvořit sadu histogramů a sadu QQ-grafů.

# Dostat ggplot do for-cyklu už vyžaduje pokročilejší znalosti jazyka R, tudíž tato možnost nebude zde už uvedena.
# Nicméně skvělé funkce facet_wrap() a facet_grid() velice příhodně pomůžou s vykreslováním sady
# histogramů, kdy automaticky pohlídají sjednocené osy...

ggplot(dataS,
       aes(x=kap5))+
  geom_histogram(binwidth = 20)+                  
  facet_grid("vyrobce")

ggplot(dataS,
       aes(x=kap5))+
  geom_histogram(binwidth = 20)+                  
  facet_wrap("vyrobce")

# Obdobně lze facety použít pro vykreslení sady QQ-grafů s tím, že zde již nepotřebujeme dodržet sjednocené osy
ggplot(dataS, 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap("vyrobce", 
             scales = "free")

ggplot(dataS, 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()+
  facet_grid("vyrobce",
             scales = "free")
