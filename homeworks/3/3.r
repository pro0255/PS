

##1
#a
pbinom(2, 50, 0.1)
1 - pbinom(2, 50, 0.1)
#b
(1 - pbinom(2, 50, 0.1)) / (1 - pbinom(0, 50, 0.1)) 

#d
1 - phyper(2, 20, 180, 50)




##2
#a
1 - ppois(50, 48)

ppois(50, 48, lower.tail = FALSE)
#b
ppois(55, 48) - ppois(44, 48)
#c
(1 - ppois(54, 48)) / (1 - ppois(39, 48)) 
#d

1 - pnbinom(14, 3, 0.2)


##3
#a


x = seq(0, 5, 0.05)
y = dexp(x, 1)
plot(x, y, type = "l")


#hustota pravdepodobnosti
x = seq(0, 100, 0.05)
y = dexp(x, 1/12)
plot(x, y, type = "l")



#distribucni funkce
x = seq(0, 50)
y = pexp(x, 1/12)
plot(x, y, type = "l")


#b
1 - pexp(13, 1/12)

#c
(1 - pexp(379, 1/365)) / (1 - pexp(365, 1/365))


pexp(379, 1/365) - pexp(365, 1/365)  

#d
qexp(0.9, 1/12)

##4
#a

#hustota pravdepodobnosti
k = 3 # 1 = 0.68, 2 = 0.95, 3 = 0.99
x = seq(112-(10*k), 112+(10*k)) #kolik hodnot v intervalu
y = dnorm(x, 112, 10)
plot(x, y, type="l") ##moznost vykreslit caru..
##

#distribucni funkce
k = 3 # 1 = 0.68, 2 = 0.95, 3 = 0.99
x = seq(112-(10*k), 112+(10*k)) #kolik hodnot v intervalu
y = pnorm(x, 112, 10)
plot(x, y, type="l") ##moznost vykreslit caru..


##vypsat si jednotlive body a zakreslit
#b
(1 - pnorm(119, 112, 10)) + pnorm(90, 112, 10)

v = pnorm(120, 112, 10) - pnorm(90, 112, 10)
v
1 - v
#c
pnorm(105, 112, 10)
#d
qnorm(0.82, 112, 10)




##test
qnorm(0.2, 35, 2)



