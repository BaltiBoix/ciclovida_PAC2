require(tidyverse)

df <- read_csv('data/train.csv')

summary(df)

df$Survived <- factor(df$Survived)
df$Pclass <- factor(df$Pclass)
df$Sex <- factor(df$Sex)
df$SibSp <- factor(df$SibSp)
df$Parch <- factor(df$Parch)
df$Embarked <- factor(df$Embarked)

summary(df)

df %>%
  select(-PassengerId, -Survived) %>%
  filter(is.na(Age)) %>%
  group_by(Ticket, SibSp, Parch) %>%
  summarize(n=n(), nf=sum(Sex=='female'), nm=n-nf) %>%
  mutate(Parch_n=as.numeric(Parch)-1) %>%
  filter(Parch_n>=1) %>%
  arrange(Ticket) -> tmp

df %>%
  filter(is.na(Age)) %>%
  group_by(Ticket) %>%
  summarize(n_ticket=n()) %>%
  filter(n_ticket==1)->tmp

df <- df %>%
  mutate(Child=Age<=16)

df$Ticket %in% tmp$Ticket


  