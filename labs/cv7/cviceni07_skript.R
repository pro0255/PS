


install.packages("dplyr")
library(dplyr)

charakteristiky_dle_vyrobce = 
  dataS %>%
  group_by(vyrobce) %>% 
  summarise(rozsah = length(kap5),
            minimum = min(kap5, na.rm=T),     # preventivn� na.rm=T
            Q1 = quantile(kap5, 0.25, na.rm=T),
            prumer = mean(kap5, na.rm=T),
            median = median(kap5, na.rm=T),
            Q3 = quantile(kap5, 0.75, na.rm=T),
            maximum = max(kap5, na.rm=T),
            rozptyl = var(kap5, na.rm=T),
            smerodatna_odchylka = sd(kap5,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # varia�n� koeficient v procentech
            sikmost = (moments::skewness(kap5, na.rm=T)),       # preventivn� specifikace bal��ku moments
            spicatost = (moments::kurtosis(kap5, na.rm=T)-3))


##krasny vypis hodnot za pomoci boxoveho grafu
pom = boxplot(dataS$kap5~dataS$vyrobce, plot = F)
pom   # v pom$out jsou ulo�ena odlehl� pozorov�n� detekov�na metodou vnit�n�ch hradeb,






##neprepsat puvodni hodnoty
##udelat prosim odstraneni outlieru

dataS$kap5_out = dataS$kap5
dataS$kap5_out[dataS$vyrobce == "A" & dataS$kap5 >= 2023] = NA
dataS$kap5_out[dataS$vyrobce == "C" & dataS$kap5 == 1848.4] = NA
dataS$kap5_out[dataS$vyrobce == "D" & dataS$kap5 == 1650.3] = NA


###moznost odstraneni pomoci vnitrnich hradeb
# Pou�it� vnit�n�ch hradeb - obecn�j�� postup - uvedeno bez ohledu na v�robce!!!
IQR = quantile(dataS$kap5, 0.75, na.rm=T) - quantile(dataS$kap5, 0.25, na.rm=T)  # mezikvartilov� rozp�ti
# nebo
IQR = IQR(dataS$kap5)

dolni_mez = quantile(dataS$kap5, 0.25, na.rm=T) - 1.5*IQR  # v�po�et doln� mezi vnit�n�ch hradeb
horni_mez = quantile(dataS$kap5, 0.75, na.rm=T) + 1.5*IQR  # v�po�et horn� mezi vnit�n�ch hradeb

# Definov�n� nov�ho sloupce, ve kter�m budou odlehl� pozorov�n� odstran�na
dataS$kap5_out = dataS$kap5
dataS$kap5_out[dataS$kap5 >= horni_mez | dataS$kap5 <= dolni_mez] = NA






