
#knihovny
library(readxl)
library(moments)
library(dplyr)
library(ggplot2)
library(lawstat)
library(BSDA)
library(car)

getwd()

#load data
filename = "ukol_120.xlsx"
path = "C:/Users/Vojta/Desktop/own/university/ing/01/ps/statistics"
setwd(path)
u_data = read_excel(filename, sheet = "Vysledky mereni")


#prepare standard format
u_data = as.data.frame(u_data)

#deletion of unused column
u_data = u_data[,-1] 


amber = "Amber"
bright = "Bright"
clear = "Clear"
dim = "Dim"

#prepared name of columns
co = c(amber, bright, clear, dim)


#indicies of columns
i_22 = c(1, 3, 5, 7)
i_5 = c(2, 4, 6, 8)


#data 22
data22 = u_data[, i_22]
colnames(data22) = co

#data 5
data5 = u_data[, i_5]
colnames(data5) = co



name_c_cat = 'vyrobce'
name_c_22 = "tok_teplota_22"
name_c_5 = "tok_teplota_5"


s_data22 = stack(data22)
colnames(s_data22) = c(name_c_22, name_c_cat)

s_data5 = stack(data5)
colnames(s_data5) = c(name_c_5, name_c_cat)

s_data = cbind(s_data5, s_data22)
s_data = s_data[, -2]


#delete of na
s_data = na.omit(s_data)



#definition of extra properties
decrease_fn = function(data5, data22) {
  res =  data22 - data5 
  return (res)
}


#alespoň osmdesáti procent deklarovaného maximálního světelného toku (tj. 80 % z 1 000 lm)
true_dich_value = "ANO"
false_dich_value = "NE"


dich_fn = function(data) {
  dich_constraint = 1000*0.8
  return (ifelse(data >= dich_constraint, true_dich_value, false_dich_value))
}


#creation of extra columns
s_data$pokles = decrease_fn(s_data$tok_teplota_5, s_data$tok_teplota_22)
s_data$pozadavek_5 = dich_fn(s_data$tok_teplota_5)






#outliers

pom5 = boxplot(s_data$tok_teplota_5~s_data$vyrobce, plot = F)
pom22 = boxplot(s_data$tok_teplota_22~s_data$vyrobce, plot = F)

pom5
#659.4 652.1 686.3 938.8 675.5 725.8
#1 1 2 2 3 4
#"Amber"  "Bright" "Clear"  "Dim"   
pom22
#658.3 688.4 681.3 730.8
#1 2 3 4
#"Amber"  "Bright" "Clear"  "Dim"   



remove_outliers = function(data){
  data$tok_teplota_5_out = data$tok_teplota_5
  data$tok_teplota_22_out = data$tok_teplota_22
  
  data$tok_teplota_5_out[data$vyrobce == amber & data$tok_teplota_5 <= 659.4] = NA
  data$tok_teplota_5_out[data$vyrobce == bright & data$tok_teplota_5 == 686.3] = NA
  data$tok_teplota_5_out[data$vyrobce == bright & data$tok_teplota_5 == 938.8] = NA
  data$tok_teplota_5_out[data$vyrobce == clear & data$tok_teplota_5 == 675.5] = NA
  data$tok_teplota_5_out[data$vyrobce == dim & data$tok_teplota_5 == 725.8] = NA
  
  
  
  data$tok_teplota_22_out[data$vyrobce == amber & data$tok_teplota_22 == 658.3] = NA
  data$tok_teplota_22_out[data$vyrobce == bright & data$tok_teplota_22 == 688.4] = NA
  data$tok_teplota_22_out[data$vyrobce == clear & data$tok_teplota_22 == 681.3] = NA
  data$tok_teplota_22_out[data$vyrobce == dim & data$tok_teplota_22 == 730.8] = NA
  
  return (data)
}


#actual deletion
s_data = remove_outliers(s_data)






#analysis amber
data_amber = s_data[s_data$vyrobce == amber,]



vector_characteristics = function(data) {
  rozsah = length(data)
  minimum = min(data, na.rm=T)
  Q1 = quantile(data, 0.25, na.rm=T)
  prumer = mean(data, na.rm=T)
  median = median(data, na.rm=T)
  Q3 = quantile(data, 0.75, na.rm=T)
  maximum = max(data, na.rm=T)
  rozptyl = var(data, na.rm=T)
  smerodatna_odchylka = sd(data, na.rm=T)
  variacni_koeficient = (100*(smerodatna_odchylka/prumer))  # variační koeficient v procentech
  sikmost = moments::skewness(data, na.rm=T)     # preventivní specifikace balíčku moments
  spicatost = moments::kurtosis(data, na.rm=T) - 3

  
  emp.data <- data.frame(
    rozsah = rozsah, 
    minimum = minimum,
    Q1 = Q1,
    prumer= prumer,
    median = median,
    Q3 = Q3, 
    maximum = maximum,
    rozptyl = rozptyl,
    smerodatna_odchylka= smerodatna_odchylka,
    variacni_koeficient = variacni_koeficient,
    sikmost = sikmost,
    spicatost = spicatost,
    stringsAsFactors = FALSE
  )
  
  
  print(emp.data)
}


run_characteristics = function(data, title) {

  
  print(title)
  print("5")
  vector_characteristics(data$tok_teplota_5)
  print("22")
  vector_characteristics(data$tok_teplota_22)
}


run_characteristics(data_amber, "Amber with outliers")



data_amber_teplota_5_withoutliers = data_amber$tok_teplota_5
data_amber_teplota_22_withoutliers = data_amber$tok_teplota_22


data_amber_teplota_5_withoutoutliers = data_amber$tok_teplota_5_out[!is.na(data_amber$tok_teplota_5_out)]
data_amber_teplota_22_withoutoutliers = data_amber$tok_teplota_22_out[!is.na(data_amber$tok_teplota_22_out)]
print("Amber without outliers")
print("5")
vector_characteristics(data_amber_teplota_5_withoutoutliers)
print("22")
vector_characteristics(data_amber_teplota_22_withoutoutliers)



#hradby

calc_hradby = function(data) {
  IQR = quantile(data, 0.75, na.rm=T) - quantile(data, 0.25, na.rm=T)  # mezikvartilové rozpěti
  # nebo
  IQR = IQR(data)
  
  dolni_mez = quantile(data, 0.25, na.rm=T) - 1.5*IQR  # výpočet dolní mezi vnitřních hradeb
  horni_mez = quantile(data, 0.75, na.rm=T) + 1.5*IQR  # výpočet horní mezi vnitřních hradeb
  
  # Definování nového sloupce, ve kterém budou odlehlá pozorování odstraněna
  print(dolni_mez)
  print(horni_mez)
}

#Meze vnitřních hradeb zaokrouhlujeme na o jednu cifru vyšší přesnost, než data v datovém souboru.
print("Teplota 5 with")
calc_hradby(data_amber_teplota_5_withoutliers)
print("Teplota 22 with")
calc_hradby(data_amber_teplota_22_withoutliers)


print("Teplota 5 without")
calc_hradby(data_amber_teplota_5_withoutoutliers)
print("Teplota 22 without")
calc_hradby(data_amber_teplota_22_withoutoutliers)










#graphs



toky = data_amber[, c(name_c_5, name_c_22)]

type_tok_5 = 'Teplota 5°C'
type_tok_22 = 'Teplota 22°C'


colnames(toky) = c(type_tok_5, type_tok_22)

data_plot = stack(toky)


t_5_out = "tok_teplota_5_out"
t_22_out = "tok_teplota_22_out"
toky_withoutoutliers = data_amber[, c(t_5_out, t_22_out)]

colnames(toky_withoutoutliers) = c(type_tok_5, type_tok_22)

data_plot_withoutoutliers = stack(toky_withoutoutliers)



typ = "typ"
colnames(data_plot) = c("toky", typ)
colnames(data_plot_withoutoutliers) = c("toky", typ)


  
  


##Histogram
binwidth = 7
ggplot(data_plot_withoutoutliers,
       aes(x = toky))+
  geom_histogram(color="black", fill="gray", binwidth = binwidth)+
  stat_bin(binwidth=binwidth, geom="text", colour="white", aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(y="četnost", x="světelný tok (lm)")+
  facet_wrap(typ, nrow =  2) + theme_bw()





##QQ
ggplot(data_plot_withoutoutliers, 
       aes(sample = toky))+
  stat_qq() +
  stat_qq_line() +
  labs(y="vyběrové kvantily", x="norm. teoretické kvantily")+
  facet_wrap(typ, nrow =  2, 
             scales = "free")+
  theme_bw()



##Krabice
ggplot(data_plot,
       aes(x = typ,
           y = toky))+ # estetika
  geom_boxplot()+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +
  labs(x = "",
       y = "světelný tok (lm)") + theme_bw()









#otazky nezapomenout:


#grafy v pohodě?, grafy title?
#outliers separatně ok?
#zaokrouhleni?
#meze vnitřních hradeb zaokrouhlujeme na o jednu cifru vyšší přesnost, než data v datovém souboru?
#3s pravidlo zaokrouhlena hodnota? spíše předpokládám nezaokrouhlenou..



#782.1157
#782.1
u_5 = 782.1157


#782.7085
#782.7
u_22 = 782.7085


# 17.42455
#17.5
s_5 =  17.42455
#17.07477 
#17.1
s_22 = 17.07477
  	

calc_x = function(u,s,k) {
  
  print(u-(s*k))
  print(u+(s*k))
  
}


calc_x(u_5, s_5, 2)
calc_x(u_22, s_22, 2)




#Porovnejte pokles sveteln?ho toku po 30 sekund?ch od zapnut? pri sn?zen? okoln? teploty z 22?C na 5?C u 
#z?rivek od v?robcu Amber a Bright. Nezapomente, ze pouzit? metody mohou vyzadovat splnen? urcit?ch predpokladu. 
#Pokud tomu tak bude, okomentujte splnen?/nesplnen? techto predpokladu jak na z?klade exploracn? anal?zy 
#(napr. s odkazem na histogram apod.), tak exaktne pomoc? metod statistick? indukce.



test_vyrobci_1 = 'Amber'
test_vyrobci_2 ='Bright'



s_data_du2 = s_data


s_data_du2 = s_data_du2[s_data_du2$vyrobce == test_vyrobci_1 |  s_data_du2$vyrobce == test_vyrobci_2,] #filtrace potrebnych dat



pokles = "pokles"

filter_cols = c(name_c_cat, pokles)

s_data_du2 = s_data_du2[, filter_cols]

#a)	Graficky prezentujte srovn?n? poklesu sveteln?ho toku z?rivek v?robcu Amber a Bright 
#pri sn?zen? okoln? teploty (v?cen?sobn? krabicov? graf, histogramy, q-q grafy). 
#Srovn?n? okomentujte (vcetne informace o pr?padn? manipulaci s datov?m souborem).



krabice_title = "pokles světelného toku (lm)"

##Krabice
ggplot(s_data_du2,
       aes(x = vyrobce,
           y = pokles))+ # estetika
  geom_boxplot()+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +
  labs(x = "",
       y = krabice_title) + theme_bw() +
  theme(axis.text = element_text(size = 13),
  axis.title = element_text(size = 13))





#ziskani dat k odstraneni

outliersHERE = boxplot(s_data_du2$pokles~s_data_du2$vyrobce, plot = F)
outliersHERE

#-111.6   94.8
#1 2

#near(data1a$odehrane_hod_2018, 320.0) HELP
s_data_du2$pokles[s_data_du2$vyrobce == amber & near(s_data_du2$pokles, 111.6)] = NA
s_data_du2$pokles[s_data_du2$vyrobce == bright & near(s_data_du2$pokles, -94.8)] = NA

#actually we can plot one more time krabici WOW!

#PLS odtranit odlehla pozorovani potom histo a qq

binwidth = 0.7


ggplot(s_data_du2,
       aes(x = pokles))+
  geom_histogram(color="black", fill="gray", binwidth = binwidth)+
  stat_bin(binwidth=binwidth, geom="text", colour="white", aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(y="četnost", x="světelný tok (lm)")+
  facet_wrap(name_c_cat, nrow =  2) + theme_bw()
+  theme(axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))




##QQ
ggplot(s_data_du2, 
       aes(sample = pokles))+
  stat_qq() +
  stat_qq_line() +
  labs(y="vyběrové kvantily", x="norm. teoretické kvantily")+
  facet_wrap(name_c_cat, nrow =  2, 
             scales = "free")+
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))





#b)	Na hladine v?znamnosti 5 % rozhodnete, zda jsou stredn? poklesy (popr. medi?ny poklesu) sveteln?ho toku z?rivek v?robcu Amber a Bright 
#statisticky v?znamn?. K resen? vyuzijte bodov? a intervalov? odhady i testov?n? hypot?z. V?sledky okomentujte.


tapply(s_data_du2$pokles, s_data_du2$vyrobce, moments::skewness, na.rm =T) #šikmost
tapply(s_data_du2$pokles, s_data_du2$vyrobce, moments::kurtosis, na.rm =T)-3 #špičatost

tapply(s_data_du2$pokles, s_data_du2$vyrobce, shapiro.test)


vyznamnost = 0.05


0.1032 < vyznamnost

0.01195 < vyznamnost

tapply(s_data_du2$pokles, s_data_du2$vyrobce, symmetry.test, boot = FALSE)


tapply(s_data_du2$pokles, s_data_du2$vyrobce, SIGN.test, md = 0, alternative = "greater", conf.level = 0.95)
# V obou výše uvedených případech bychom bodový odhad - tj. výběrový medián - získali pomocí funkce quantile.
tapply(s_data_du2$pokles, s_data_du2$vyrobce, quantile, probs = 0.5, na.rm = T)




#c)	Na hladine v?znamnosti 5 % rozhodnete, zda je rozd?l stredn?ch hodnot (medi?nu) poklesu sveteln?ch toku z?rivek 
#v?robcu Amber a Bright (pri sn?zen? okoln? teploty) statisticky v?znamn?. K resen? vyuzijte bodov? a intervalov? odhad 
#i cist? test v?znamnosti. V?sledky okomentujte.


# Analýza předpokladů -> normalita KO -> přechod na mediány!
# Kontrola, zda výběry mají stejný tvar rozdělení (histogramy, míry tvaru)
# Nesmí být např. jedno rozdělení výrazně zešikmené doleva a druhé doprava.

# Analýza předpokladů -> normalita KO, stejný tvar OK -> Mannův-Whitneyho test

# Kvůli přehlednosti je zde medián značen jen symbolicky x (aby byl odlišitelný od stř. hodnoty mu)
# Mannův-Whitneyho test (H0: x_L = x_O, HA: x_L >  x_O) odpovídá (H0: x_L - x_O = 0, HA: x_L - x_O > 0)


wilcox.test(s_data_du2$pokles[s_data_du2$vyrobce =="Bright"], 
            s_data_du2$pokles[s_data_du2$vyrobce =="Amber"],
            alternative = "two.sided", 
            conf.level = 0.95,
            conf.int = T)


tapply(s_data_du2$pokles, s_data_du2$vyrobce, quantile, probs = 0.5, na.rm = T)

quantile(s_data_du2$pokles[s_data_du2$vyrobce == "Bright"], probs = 0.5, na.rm = T) - quantile(s_data_du2$pokles[s_data_du2$vyrobce == "Amber"], probs = 0.5, na.rm = T)




# 1.918464
vector_characteristics(s_data_du2$pokles[s_data_du2$vyrobce == "Bright"])


# 3.140029 
vector_characteristics(s_data_du2$pokles[s_data_du2$vyrobce == "Amber"])

#### konec 2


#### zacatek 3


#a

#create data structure
selector_du3 = c("tok_teplota_5", "tok_teplota_5_out", "vyrobce")
data_du3 = s_data[, selector_du3]

#make data vizualization

##Krabice

#with outliers
ggplot(data_du3,
       aes(x = vyrobce,
           y = tok_teplota_5))+ # estetika
  geom_boxplot()+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +
  labs(x = "",
       y = krabice_title) + theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))



#without outliers
ggplot(data_du3,
       aes(x = vyrobce,
           y = tok_teplota_5_out))+ # estetika
  geom_boxplot()+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +
  labs(x = "",
       y = krabice_title) + theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))






binwidth = 8
ggplot(data_du3,
       aes(x = tok_teplota_5_out))+
  geom_histogram(color="black", fill="gray", binwidth = binwidth)+
  stat_bin(binwidth=binwidth, geom="text", colour="white", aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(y="četnost", x="světelný tok (lm)")+
  facet_wrap(name_c_cat, nrow =  2) + theme_bw()+
 theme(axis.text = element_text(size = 13),axis.title = element_text(size = 13))




##QQ
ggplot(data_du3, 
       aes(sample = tok_teplota_5_out))+
  stat_qq() +
  stat_qq_line() +
  labs(y="vyběrové kvantily", x="norm. teoretické kvantily")+
  facet_wrap(name_c_cat, nrow =  2, 
             scales = "free")+
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))


#b
tapply(data_du3$tok_teplota_5_out, data_du3$vyrobce, moments::skewness, na.rm =T) #šikmost

#        Amber        Bright         Clear           Dim 
#-0.0876206068 -0.0005222603 -0.0849482178 -0.1690739422 

tapply(data_du3$tok_teplota_5_out, data_du3$vyrobce, moments::kurtosis, na.rm =T)-3 #špičatost


#     Amber     Bright      Clear        Dim 
#-1.0720822 -0.9846829 -0.8359846 -0.6902268 

tapply(data_du3$tok_teplota_5_out, data_du3$vyrobce, shapiro.test)

vyznamnost = 0.05

0.03169 < vyznamnost
0.04621 < vyznamnost
0.1211 < vyznamnost
0.0232 < vyznamnost

tapply(data_du3$tok_teplota_5_out, data_du3$vyrobce, symmetry.test, boot = FALSE)



0.6765 < vyznamnost
0.9854 < vyznamnost
0.4133 < vyznamnost
0.01651 < vyznamnost




#c
# Empiricky - poměr výběrových rozptylů (největší ku nejmenšímu) < 2 ?
tapply(data_du3$tok_teplota_5_out, data_du3$vyrobce, var, na.rm =T)


var_res = 717.1908 / 303.6150
var_res < 2


#Exaktně
#bartlett.test(data_du3$tok_teplota_5_out~data_du3$vyrobce)

# Exaktně - Leveneho test o shodě rozptylů pro data, která nepochází z normálního rozdělení

leveneTest(data_du3$tok_teplota_5_out ~ data_du3$vyrobce)






#d



vector_characteristics(data_du3$tok_teplota_5_out[data_du3$vyrobce == amber])
vector_characteristics(data_du3$tok_teplota_5_out[data_du3$vyrobce == bright])
vector_characteristics(data_du3$tok_teplota_5_out[data_du3$vyrobce == clear])
vector_characteristics(data_du3$tok_teplota_5_out[data_du3$vyrobce == dim])




