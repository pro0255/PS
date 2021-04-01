

#vyberove charakteristiky
#mame populaci
#chova se stale stejne


#vim neco o populaci a modeluju jak se bude chovat vyber


###1

##a
#X...delka zivota mouchy
#E(x) = 28 dni
#sigma(x) = 2 dny
#P(X > 30) = neni rozdeleni tudiz nelze spocist



##b
# n = 40
#X40 = 1/40suma(i:40(Xi))

#vime:
# n = 40, u = 28, s = 2^2
# N(u=28,s^2/n=4/40)
#
pnorm(31, 28, sqrt(4/40)) - pnorm(29, 28, sqrt(4/40))


##c




###2

##b
1 - pt(-3.162, 39)


1- pnorm(880000, 60*15000, sqrt(60*(2000^2)))


x = (49*(50^2))/(45^2)
1 - pchisq(x, 49)
