#//////////////////////////////////////////////////////////////////
#///////////////// Cvičení 13 - Kontingenční tabulky  /////////////
#//////  R-skript k Doplňujícím příkladům k online cvičení  ///////
#/////////////////      Mgr. Adéla Vrtková        /////////////////
#//////////////////////////////////////////////////////////////////

# Tento skript obsahuje pouze R-příkazy !!!
# Veškeré korektní zápisy a doprovodné komentáře jsou k dispozici v Poznámkách.

# R-příkazy nejsou samy o sobě uznatelným řešením! 
# Slouží případně pouze jako doplněk !!!

#//////////////////////////////////////////////////////////////////
# Příprava prostředí #####


path = "C:/Users/Vojta/Desktop/own/university/ing/01/ps/labs/cv9"
setwd(path)
library(readxl)
library(dplyr)

library(ggplot2)
library(ggmosaic) # vyžaduje verze ggplot2 >= 3.3.0 a plotly >= 4.5.5



data = read_excel("data_hraci2.xlsx")

# Vytvoření dichotomické proměnné
data$odehrane_dich = ifelse(data$odehrane_hod_2019 > 360, "Ano", "Ne")

# Překódování proměnných jako typ faktor, případná úprava popisků do prezentovatelné podoby
data$odehrane_dich = factor(data$odehrane_dich, 
                            levels = c("Ano", "Ne"))

data$system = factor(data$system,
                     levels = c("Linux", "OSX", "WIN"),
                     labels = c("Linux", "OS X", "Windows"))

# Základní kontingenční tabulka (řádky - "příčina", sloupce - "důsledek")
kont_tab = table(data$system, data$odehrane_dich)
kont_tab

#//////////////////////////////////////////////////////////////////
# Příklad 1 #####

#* a) ########################

## Příprava kont. tabulky ####
# Řádkové a sloupcové součty
addmargins(kont_tab)

# Získání řádkových relativních četností (zde se hodí více než sloupcové)
# 100 % je součet v řádku
100*prop.table(kont_tab, margin = 1)

# Po zaokouhlení (je potřeba zvolit vhodný počet desetinných míst - řídíme se podle toho, kolik je 1 %)
kont_tab_rel = round(100*prop.table(kont_tab, margin = 1), 1)
kont_tab_rel

# Kontrola zaokrouhlovací chyby - součet v řádku musí dávat 100 % !!!
addmargins(kont_tab_rel, margin = 2)

# Případné ruční ošetření zaokrouhlovací chyby (lze různými způsoby)
# Myšlenka - druhý sloupec vznikne jako dopočet do 100 %
kont_tab_rel[,2] = 100 - kont_tab_rel[,1] 
kont_tab_rel

# Pomocí rozšířené kontingenční tabulky s abs. četnostmi a tabulky s řádkovými relativními četnostmi
# vytvoříme vhodnou finální kontingenční tabulku (viz Poznámky).
addmargins(kont_tab)
kont_tab_rel

## Míra kontingence ####
# Vhodnou mírou kontingence je Cramerovo V
library(lsr)
cramersV(kont_tab)

## Vizualizace - mozaikový graf ####
# Vhodnou vizualizací je mozaikový graf nebo 100% skládaný sloupcový graf
# Mozaikový graf se základním R není úplně reprezentativní a je obtížný na úpravu,
# což platí i pro 100% skládaný sloupcový graf. Ukážeme si proto vizualizaci jen s ggplot2,
# která je nachystaná tak, že v podstatě stačí si pak kód jen překopírovat, vložit vlastní data
# a příp. doupravovat popisky.

# Mozaikový graf s knihovnou ggmosaic (vyžaduje verze ggplot2 >= 3.3.0 a plotly >= 4.5.5)
# Vstupem jsou data přímo "ze zdroje" + pro popisky nachystané tabulky
options(OutDec = ",")
muj_mos = 
  ggplot(data)+
    geom_mosaic(aes(x = product(odehrane_dich, system),
                    fill = odehrane_dich),
              offset = 0.02,
              colour = "black")+  
    labs(x = "", y = "",  title = "")+
    theme_bw()+
    theme(legend.position = "none",
          axis.text = element_text(color = "black", size = 13))+
    scale_fill_manual(values = c("gray68", "gray40"))

muj_mos

popis_muj_mos = matrix(paste0(kont_tab," (",kont_tab_rel, " %)"),
                       byrow = F,
                       ncol = ncol(kont_tab))
popis_muj_mos = as.vector(t(popis_muj_mos))
popis_muj_mos

data[[1]]

muj_mos + 
  geom_text(data = ggplot_build(muj_mos)$data[[1]], 
          aes(label = popis_muj_mos, 
              x = (xmin+xmax)/2, 
              y = (ymin+ymax)/2),
          colour = "black")

## Vizualizace - 100% skládaný sloupcový ####
# Ukážeme si zde tvorbu (reprezentativního) 100% skládaného sloupcového grafu s ggplot2

# Potřebujeme nachystané kont. tabulky s abs. četnostmi i řádkovými rel. četnostmi,
# které je potřeba si převést do jiného formátu
# Připravené tabulky (máme)
kont_tab
kont_tab_rel

# Úprava tabulek pro ggplot2 (obě do formátu data.frame)
kont_tab_df = as.data.frame(kont_tab)
kont_tab_df
colnames(kont_tab_df) = c("system", "hodiny", "AbsCet")

kont_tab_rel_df = as.data.frame(kont_tab_rel)
kont_tab_rel_df
colnames(kont_tab_rel_df) = c("system", "hodiny", "RelCet")

# Vykreslení 100% skládaného sloupcového grafu už se všemi potřebnými náležitostmi
options(OutDec = ",")

ggplot(kont_tab_rel_df,
       aes(x = system,
           y = RelCet,
           fill = hodiny))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(kont_tab_df$AbsCet," (",RelCet," %)")), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("gray72", "gray56"))+
  labs(x = " ",
       y = "Kumulativní relativní četnost (%)",
       fill = "Odehráno více než 360 hodin za rok 2019")+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

options(OutDec = ".")

#* b) ########################
## Chí-kvadrát test nezávislosti pro kont. tabulky ####
test = chisq.test(kont_tab) # vstupem je úplně původní kont. tabulka
test$expected # potřebujeme kvůli předpokladům (zde najdeme očekávané četnosti)

test # až poté zkoumám celý výstup a p-hodnotu

# Zvláštnost - ověření předpokladů je "uvnitř" samotné funkce, která provádí test

#//////////////////////////////////////////////////////////////////
# Příklad 2 #####

#* a) ########################

kont_tab # získání potřebných četností

# Výpočet pro Windows - pro ostatní systémy obdobně
x = 121
n = 121+81
p = x/n
p

n > 9/(p*(1-p))

binom.test(x, n, conf.level = 0.9)


#* b) ########################
# ! Riziko = Pravděpodobnost !
# Řešení bodu b) je naprosto stejné jako bodu a). 

#* c) ########################
# Bodové odhady šance vytáhneme z kont. tabulky
kont_tab
# viz Poznámky


#//////////////////////////////////////////////////////////////////
# Příklad 3 #####

# Ruční příprava tabulky - jen tak si ohlídáme potřebnou strukturu pro funkci epi.2by2
moje_tab = matrix(c(45,13,121,81), 
                  byrow = T, 
                  ncol = 2)
rownames(moje_tab) = c("Linux", "Windows")
colnames(moje_tab) = c("Ne", "Ano")
moje_tab

#* a)+b) ########################
library(epiR)
epi.2by2(moje_tab, conf.level = 0.9)


#* c) ########################
kont_tab

# Čísla pro Windows
x_W = 121
n_W = 121+81
p_W = x_W/n_W
p_W
n_W > 9/(p_W*(1-p_W))

# Čísla pro Linux
x_L = 45
n_L = 45+13
p_L = x_L/n_L
p_L
n_L > 9/(p_L*(1-p_L))

p_L-p_W

prop.test(c(x_L,x_W), c(n_L,n_W), alternative = "greater", conf.level = 0.9)





