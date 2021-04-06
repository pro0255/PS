
#knihovny
library(readxl)
library(moments)
library(dplyr)
library(ggplot2)


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
  res =  data5 - data22
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

typ = "typ"
colnames(data_plot) = c("toky", typ)


  
  


##Histogram
binwidth = 10
ggplot(data_plot,
       aes(x = toky))+
  geom_histogram(color="black", fill="gray", binwidth = binwidth)+
  stat_bin(binwidth=binwidth, geom="text", colour="white", aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(y="četnost", x="světelný tok (lm)")+
  facet_wrap(typ, nrow =  2) + theme_bw()





##QQ
ggplot(data_plot, 
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









#otazky:


#grafy v poho?, grafy title?
#outliers separatně ok?
#zaokrouhleni?
#meze vnitřních hradeb zaokrouhlujeme na o jednu cifru vyšší přesnost, než data v datovém souboru.?




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













