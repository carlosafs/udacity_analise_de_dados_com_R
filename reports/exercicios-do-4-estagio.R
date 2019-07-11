

getwd()
setwd('~/CURSOS_DATA_SCIENCE/udacity')

library(ggplot2)
data(diamonds)

help("diamonds")
force(diamonds)


# Create a histogram of the price of
# all the diamonds in the diamond data set.

# TYPE YOUR CODE BELOW THE LINE
# =======================================

names(diamonds)

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(color = 'black', fill = 'white')


### how many diamonds cost less than $500

ggplot(aes(x = price), data = diamonds, binwidth = 1) + 
  geom_histogram(color = 'black', fill = 'white') +
  facet_wrap(~cut, ncol = 1) +
  scale_x_continuous(limits = c(18800, 19000), breaks = seq(18800, 19000, 10)) +
  scale_y_continuous(breaks = seq(0, 500, 5))


summary(diamonds$price)

sum(diamonds$price<20000)

sum(diamonds$price<500)
sum(diamonds$price<250)
sum(diamonds$price>14999)
sum(diamonds$price>=15000)


by(diamonds$price, diamonds$cut, summary)

qplot(x = price, data = diamonds) + facet_wrap(~cut, "free_y")



?facet_wrap

#histograma preço por quilate
#facetado por cut

qplot(x = price, y = carat, data = diamonds) + facet_wrap(~cut)

ggplot(aes(x = price/carat), data = diamonds) + 
  geom_histogram(color = 'blue', fill = 'yellow') +
  facet_wrap(~cut, ncol = 1) +
  scale_x_log10()


# usando boxplot
# Investigue o preço dos diamantes usando gráficos de caixa, resumos numéricos e 
# uma das seguintes variáveis categóricas: corte, clareza ou cor.

#resposta: color e price

qplot(x = color, y = price, 
      data = diamonds, 
      geom = 'boxplot') +
  coord_cartesian(ylim = c(750, 8000))

by(diamonds$price, diamonds$color, summary)

by(diamonds$price, diamonds$color, IQR)

#Investigue o preço por quilate de diamantes nas 
#diferentes cores de diamantes usando boxplots.

qplot(x = color, y = price/carat, 
      data = diamonds, 
      geom = 'boxplot') +
  coord_cartesian(ylim = c(2000, 6000))

# Investigar o peso dos diamantes (quilate) usando um polígono de freqüência. 
# Use larguras binárias diferentes para ver como o polígono de freqüência muda. 
# Qual tamanho de quilate tem contagem maior que 2000?

qplot(x = carat, data = diamonds,
      xlab = 'quilates',
      geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.1))
