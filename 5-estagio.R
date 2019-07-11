

getwd()
read.delim('pseudo_facebook.tsv') 
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

library(ggplot2)

ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20) +
  xlim(13, 90)

ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 0.05, 
             position = position_jitter(h = 0),
             color = 'orange') +
  xlim(13, 90) +
  coord_trans(y = "sqrt")

ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/20,  position = position_jitter(h = 0)) +
  xlim(13, 90) +
  coord_trans(y = "sqrt")


## organizando os dados: agrupando obs com valores iguais. exemplo: agrupando a
## quantidade de amigos por idades, ou seja, as contas de 13 anos são 484 e tem uma
## mediana de 74 amigos.



## biblioteca dplyr
install.packages("dplyr")
library(dplyr)


pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)
head(pf.fc_by_age)




# graficos

ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 0.05, 
             position = position_jitter(h = 0),
             color = 'orange') +
  coord_cartesian(xlim = c(13, 90), ylim = c(0, 1000)) +
  coord_trans(y = "sqrt") +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1),
            linetype = 2, color = "blue") +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5),
            color = "blue") +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9),
            linetype = 2, color = "blue") +
  labs( x = "IDADE", 
        y = "QUANTIDADE DE AMIGOS", 
        title = "Quantidade de amigos por idade ")


## nivel de relação entre duas variáveis pelo método de Pearson

cor.test(pf$age, pf$friend_count, method = "pearson")
# ate 70 anos
with(subset(pf, age < 70), cor.test(pf$age, pf$friend_count, method = "pearson"))

#grafico de correlação entre curtidas pelo pc com curtidas totais.

names(pf)


ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
geom_point()

#correlação
cor.test(pf$www_likes_received, pf$likes_received , method = "pearson")

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()

head(pf.fc_by_age, 10)
pf.fc_by_age[17:19, ]

names(pf)
View(pf)

pf$age_with_months <- pf$age + (1 - pf$dob_month / 12) 

View(pf)

## agrupando os dados

pf.fc_by_age_with_months <- pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age_with_months)
head(pf.fc_by_age_with_months)

#graficos

g1 <- ggplot(aes(x = age_with_months, y = friend_count_mean), 
             data = subset(pf.fc_by_age_with_months, age_with_months < 71)) + 
  geom_line() +
  geom_smooth()

g2 <- ggplot(aes(x = age, y = friend_count_mean), 
             data = subset(pf.fc_by_age, age < 71)) +
  geom_line() +
  geom_smooth()

g3 <- ggplot(aes(x = round(age/5)*5, y = friend_count), 
             data = subset(pf, age < 71)) +
  geom_line(stat = "summary", fun.y = mean)

library(gridExtra)

grid.arrange(g1, g2, g3, ncol = 1)







