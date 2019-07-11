
getwd()

library(ggplot2)
data(diamonds)


names(diamonds)
View(diamonds)

#histograma
ggplot(aes(x = price), data = diamonds) +
  geom_histogram(aes(fill = cut)) +
  facet_wrap( ~color) +
  scale_x_log10()


#grafico de dispersao
ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(color = cut)) +
  scale_color_brewer(type = 'qual')


#sabendo os limite da maiorias dos valores cut
ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(color = cut)) +
  scale_color_brewer(type = 'qual') +
  scale_x_continuous(limits = c(50, 80), breaks = seq(50, 80, 2))

# saber os valores precos pelo corte
by(diamonds$price, diamonds$cut, summary)

# saber os valores de table pelo corte 
by(diamonds$table, diamonds$cut, summary)

#grafico de dispersao preço por volume

diamonds$volume <- (diamonds$x * diamonds$y * diamonds$z)

ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point(aes(color = clarity), alpha = 0.99) +
  scale_color_brewer(type = 'div') +
  coord_trans(y = "log10") +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, 100))

  
####################################################################

#utilizando os dados do facebook

read.delim('pseudo_facebook.tsv') 
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

names(pf)

#criando nova variavel proporcao de amizades iniciadas pelo total de amigos

pf$prop_initiated <- (pf$friendships_initiated / pf$friend_count)

#########passo a passo para fazer um novo grafico

library(dplyr)
library(gridExtra)

## adicionando a variável ano de aderencia

pf$year_joined <- floor(2014 - pf$tenure/365)

summary(pf$year_joined)
table(pf$year_joined)

## agrupamento de anos 2004-2009, 2009-2011, 2011-2012, 2012-2014

pf$year_joined.bucket <- cut(pf$year_joined,
                             c(2004, 2009, 2011, 2012, 2014))
View(pf)
table(pf$year_joined.bucket)

# grafico
ggplot(aes(x = tenure, y = prop_initiated),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = median)


## suavizando o gráfico
ggplot(aes(x = tenure, y = prop_initiated),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_smooth(aes(color = year_joined.bucket))

## saber a media de prop_initiated no periodo 2012-2014
by(pf$prop_initiated, pf$year_joined.bucket, summary)

##########################################################

#voltando aos diamantes

ggplot(aes(x = cut, y = price/carat), data = diamonds) +
  geom_jitter(aes(color = color), alpha = 0.5) +
  facet_wrap(~clarity) +
  scale_color_brewer(type = 'div')




