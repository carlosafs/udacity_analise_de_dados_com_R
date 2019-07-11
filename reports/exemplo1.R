
## construindo histograma

#saber onde fica o repositorio
getwd()

#mudar repositorio
setwd('~/CURSOS_DATA_SCIENCE/udacity')

#carregar dados
read.delim('pseudo_facebook.tsv')

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')



## Histograma do dia de aniversario dos usuarios

library(ggplot2)

#histograma
names(pf)
qplot(x = dob_day, data = pf) +
  scale_x_continuous(breaks = 1:31)

install.packages('ggthemes', dependencies = TRUE) 
library(ggthemes) 

#histograma com detalhes diferentes
ggplot(aes(x = dob_day), data = pf) + 
  geom_histogram(bins = 31, fill='black', color ='white') +
  scale_x_continuous(breaks = 1:31)

##histograma facetado

ggplot(aes(x = dob_day), data = pf) + 
  geom_histogram(bins = 31, fill='black', color ='white') +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month, ncol = 3)

##contagem de amigos

```{r}
ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000))
```
qplot(x = friend_count, data = pf, binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))


##contagem de amigos

```{r}
ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000))
```

## facetando por genero

#qplot
qplot(x = friend_count, data = pf, binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender, ncol = 1)
#ggplot
ggplot(aes(x = friend_count), data = pf, binwidth = 25) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender, ncol = 1)

## corrigindo o valores N/A da variavel genero - tirando esses valores - funcao !is.na
#qplot
qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender, ncol = 1)

#ggplot
ggplot(aes(x = friend_count), data = subset(pf, !is.na(gender))) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender, ncol = 1)

## corrigindo o valores N/A da variavel genero - tirando esses valores - funcao na.omit
##cuidado pois pode ignorar alguns valores
#qplot
qplot(x = friend_count, data = na.omit(pf), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender, ncol = 1)

## Quantidade por genero
```{r}
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

```
## variavel tenure - mandato

#qplot em dias
qplot(x = tenure, data = pf, binwidth = 30, 
      color = I('black'), fill = I('#099DD9'))

#qplot em anos
qplot(x = tenure/365, data = pf, binwidth = 0.25, 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(1, 7, 1))



## variavel age - idade
#completo
qplot(x = age, data = pf, binwidth = 1, 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(10, 115, 5))

#ate 50 anos
qplot(x = age, data = pf, binwidth = 1, 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 115, 5))

#mais de 50 anos
qplot(x = age, data = pf, binwidth = 1, 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(50, 115), breaks = seq(0, 115, 5))

#usando o summary

summary(pf$age)

#assim, vamos obter um grafico geral:
qplot(x = age, data = pf, binwidth = 1, 
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(13, 113), breaks = seq(13, 113, 5))


#### transformando/manipulando os dados

install.packages('gridExtra') 
library(gridExtra)

# qplot
```{r}
p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(friend_count + 1), data = pf)
p3 <- qplot(x = sqrt(friend_count), data = pf)

grid.arrange(p1, p2, p3, ncol = 1)
```
# ggplot
```{r}
p1 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram() 
p2 <- p1 + scale_x_log10() 
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3, ncol = 1)
```

#### gráfico poligono de frequencia

qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 10,
      geom = 'freqpoly', color = gender ) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))


# Organizando
qplot(x = friend_count, y = ..count../sum(..count..), 
      data = subset(pf, !is.na(gender)),
      xlab = 'Quantidade de Amigos - Friend Count',
      ylab = 'Proporção do usuário com essa contagem de amigos - proportion of user with that friend count',
      binwidth = 10, geom = 'freqpoly', color = gender ) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

####################
## questao: qual gênero dá mais curtidas na world wide web (www_likes).

summary(pf$www_likes)

#geral
qplot(x = www_likes,
      data = subset(pf, !is.na(gender)),
      xlab = 'curtidas no www',
      binwidth = 10, geom = 'freqpoly', color = gender ) +
  scale_x_continuous(limits = c(0, 15000), breaks = seq(0, 15000, 500))

#melhorando
qplot(x = www_likes,
      data = subset(pf, !is.na(gender)),
      xlab = 'curtidas no www',
      binwidth = 10, geom = 'freqpoly', color = gender ) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 15000, 50))

qplot(x = www_likes,
      data = subset(pf, !is.na(gender)),
      xlab = 'curtidas no www',
      binwidth = 1, geom = 'freqpoly', color = gender ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 15000, 5))

qplot(x = www_likes,
      data = subset(pf, !is.na(gender)),
      xlab = 'curtidas no www',
      binwidth = 1, geom = 'freqpoly', color = gender ) +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15000, 1))

by(pf$www_likes, pf$gender, summary)

qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      xlab = 'curtidas no www',
      geom = 'freqpoly', color = gender ) +
  scale_x_continuous() +
  scale_x_log10()

by(pf$www_likes, pf$gender, sum)

### grafico boxplot- caixa

qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)), geom = 'boxplot')


###########
## Questao: Ajuste o código dos gráficos de caixa para focar nos usuários 
## de nossa amostra que têm contagens de amigos entre 0 e 1.000

#para tirar os valores a cima - cuidado, muda o grafico
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot' , ylim = c(0, 1000))

#usando todos os dados, mas observando apenas a parte de baixo
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 1000))

by(pf$friend_count, pf$gender, summary)

qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 250))

############
## questão: Em média, quem fez mais pedidos de amizade? (friendships_initiated)

# usando by e o summary
by(pf$friendships_initiated, pf$gender, summary)

#usando o boxplot
qplot(x = gender, y = friendships_initiated, 
      data = subset(pf, !is.na(gender)), 
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 250))

##########
## questão: Qual o percentual de check in usando dispositivos móveis?
  
#criando uma variavel para uso binario 1 ou 0. 
pf$mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)




