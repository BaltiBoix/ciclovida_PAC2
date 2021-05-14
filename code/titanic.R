require(tidyverse)
require(GGally)

df <- read_csv('data/train.csv')

summary(df)

df$Survived <- factor(df$Survived)
df$Pclass <- factor(df$Pclass, ordered=T)
df$Sex <- factor(df$Sex)
df$SibSp <- factor(df$SibSp, ordered=T)
df$Parch <- factor(df$Parch, ordered=T)
df$Embarked <- factor(df$Embarked)

df <- df %>%
  separate(Name, c('first_name', 'rest_name'), sep=', ', remove=F) %>%
  separate(rest_name, c('title_name', 'rest_name'), sep='\\.') %>%
  mutate(title_name=factor(title_name))
  select(-rest_name)

summary(df)


df %>%
  group_by(Ticket) %>%
  summarize(n_ticket=n()) %>%
  filter(n_ticket==1) %>%
  select(Ticket) -> tmp

df <- df %>%
  mutate(Child=factor(Age<=12))

left_join(tmp, df) %>%
  filter(SibSp==0, Parch==0, is.na(Age)) %>%
  select(PassengerId) -> tmp

df$Child[df$PassengerId %in% tmp$PassengerId] <- FALSE


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

df %>%
  filter(is.na(Child)) %>%
  filter(grepl('Mrs', Name)) %>%
  select(PassengerId) -> tmp

df$Child[df$PassengerId %in% tmp$PassengerId] <- FALSE

df$Child[df$title_name=='Master'] <- TRUE

df %>%
  filter(is.na(Child)) %>%
  filter(SibSp > '0', Parch > '0') %>%
  select(PassengerId) -> tmp

df$Child[df$PassengerId %in% tmp$PassengerId] <- TRUE

df$Child[is.na(df$Child)] <- FALSE

df %>%
  filter(is.na(Child)) %>%
  filter(Parch > '0')

df[df$first_name=='Bourke',]
df$Child[594]