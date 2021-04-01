
#knihovny
library(readxl)
library(moments)


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





















