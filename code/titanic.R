require(tidyverse)
require(GGally)
require(DescTools)
require(kableExtra)


df <- read_csv('data/train.csv')

summary(df)

df$Survived <- factor(df$Survived)
df$Pclass <- factor(df$Pclass)
df$Sex <- factor(df$Sex)
df$SibSp <- factor(df$SibSp)
df$Parch <- factor(df$Parch)
df$Embarked <- factor(df$Embarked)

df <- df %>%
  separate(Name, c('first_name', 'rest_name'), sep=', ', remove=F) %>%
  separate(rest_name, c('title_name', 'rest_name'), sep='\\.') %>%
  select(-rest_name)

df$title_name[df$title_name %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Rev', 'Sir')] <- 'Mr'
df$title_name[df$title_name %in% c('Lady', 'Mme','the Countess')] <- 'Mrs'
df$title_name[df$title_name %in% c('Mlle', 'Ms')] <- 'Miss'
df$title_name <- factor(df$title_name)



summary(df)


df <- left_join(df, df %>%
                  group_by(Ticket) %>%
                  summarize(n_ticket=n())) %>%
  mutate(n_ticket=factor(n_ticket))

df <- df %>%
  mutate(Child=Age<=12)

df %>%
  select(where(is.factor)) %>%
  na.omit() %>%
  pivot_longer(-Survived) %>%
  group_by(name, value, Survived) %>%
  summarize(n=n()) %>%
  mutate(prop=prop.table(n)) %>%
  pivot_longer(c(n,prop), names_to='tipo', values_to='val') %>%
  ggplot(aes(x=interaction(name,value, lex.order = T), y=val, fill=Survived)) + 
  geom_bar(stat='identity', position='stack') +
  facet_grid(tipo ~ ., scale='free_y') +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

df %>% 
  select(Survived, where(is.numeric), -PassengerId) %>%
  ggpairs(aes(color=Survived))


df <- df %>%
  mutate(Child=if_else(SibSp==0 & Parch==0 & is.na(Child) & n_ticket==1, FALSE, as.logical(Child))) %>%
  mutate(Child=if_else(title_name=='Master' & is.na(Child), TRUE, as.logical(Child))) %>%
  mutate(Child=if_else(title_name=='Mrs' & is.na(Child), FALSE, as.logical(Child))) %>%
  mutate(Child=if_else(SibSp > '0' & Parch > '0' & is.na(Child), TRUE, as.logical(Child))) %>%
  mutate(Child=if_else(is.na(Child), FALSE, as.logical(Child))) %>%
  mutate(Child=factor(Child))



df %>%
  select(Survived, where(is.factor)) %>%
  pivot_longer(-Survived) %>%
  group_by(name) %>%
  summarize(phi=Phi(x=Survived, y=factor(value)), 
            chisq.pvalue=chisq.test(Survived, factor(value))$p.value) %>%
  mutate(`signif 95%`=chisq.pvalue < 0.05) %>%
  arrange(chisq.pvalue) %>%
  {.->>tmp} %>%
  kable(format='html', digits=4, caption='<b>Phi y chisq.test</b>') %>%
  kable_styling(full_width = F)

df %>%
  select(Survived, where(is.numeric), -PassengerId) %>%
  pivot_longer(-Survived) %>%
  group_by(name, Survived) %>%
  summarize(value_list = list(value)) %>%
  pivot_wider(names_from=Survived, values_from=value_list) %>%
  mutate(vartest.pval=var.test(unlist(`0`), unlist(`1`))$p.val) %>%
  mutate(ttest.pval=t.test(unlist(`0`), unlist(`1`), var.equal=vartest.pval>0.05)$p.val) %>%
  mutate(`signif 95%`=ttest.pval < 0.05) %>%
  select(-`0`, -`1`)  %>%
  kable(format='html', digits=4, caption='<b>t.test</b>') %>%
  kable_styling(full_width = F)


df %>%
  mutate(Crew=if_else(Fare==0, TRUE, FALSE))

df %>%
  filter(Fare==0) %>%
  group_by(Survived) %>%
  summarize(n=n())

df %>%
  mutate(Fare_=if_else(Fare<10, round(Fare*2)/2, round(Fare))) %>%
  group_by(Fare_, Survived) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from=Survived, values_from=n) %>%
  arrange(Fare_) -> tmp

df %>%
  filter(Fare>0) %>%
  mutate(q=ntile(Fare,5)) %>%
  group_by(Pclass, q, Survived) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from=Survived, values_from=n) %>%
  mutate(rate=`1`/(`0`+`1`))
  