
getwd()
read.delim('pseudo_facebook.tsv') 
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

library(ggplot2)
library(dplyr)
library(gridExtra)

## Terceira variável qualitativa

```{r}
ggplot(aes(x = gender, y = age),
       data = subset(pf, !is.na(gender))) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 4)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = "summary", fun.y = median)
```

## criando data frame
# pf por idade e genero -- 3 variáveis

pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>%
  ungroup() %>%
  arrange(age)

head(pf.fc_by_age_gender)

#mesmo grafico utilizando o novo data frame

ggplot(aes(x = age, y = median_friend_count),
       data = pf.fc_by_age_gender) +
  geom_line(aes(color = gender))

###########
## transformando dados longo para amplo
# transfomando o genero em duas variáves male e female com valores de medianas neles.

install.packages("reshape2")
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = "median_friend_count")
head(pf.fc_by_age_gender.wide)

## grafico de proposrção de female por male

ggplot(aes(x = age, y = female/male), data = pf.fc_by_age_gender.wide) +
  geom_line(color = "black") +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

## adicionando a variável ano de aderencia

pf$year_joined <- floor(2014 - pf$tenure/365)

summary(pf$year_joined)
table(pf$year_joined)

## agrupamento de anos 2004-2009, 2009-2011, 2011-2012, 2012-2014

pf$year_joined.bucket <- cut(pf$year_joined,
                             c(2004, 2009, 2011, 2012, 2014))
View(pf)
table(pf$year_joined.bucket)

##grafico mediana dos agrupamentos de anos

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = median)

##grafico da media com a media geral tracejada

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = mean, linetype = 2)

## analise de relação de amigos por quantidade de dias de conta ativa

with(subset(pf, tenure >=1), summary(friend_count / tenure))

## grafico

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_smooth(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_smooth(aes(color = year_joined.bucket))

# instalando novo biblioteca
install.packages('GGally')
library("GGally")

# matriz de graficos de dispersão OBSSSSS: demora mais de uma hora para
# gerar os gráficos, depois de dá o comando.

set.seed(1836)

pf_subset <- pf[, c(2:15)]
names(pf_subset)

ggpairs(pf_subset [sample.int(nrow(pf_subset), 1000), ])





