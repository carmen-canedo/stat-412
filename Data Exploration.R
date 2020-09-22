library(tidyverse)
library(reshape2)
df = read.csv("Sports Data - MajorSports.csv")

df$Boys = as.numeric(df$Boys)
df$Girls = as.numeric(df$Girls)
df$Year = as.character(df$Year)

df.year = df %>% 
  group_by(Year) %>% 
  summarise(TotalBoys = sum(Boys),TotalGirls = sum(Girls)) %>%
  ungroup()
df.year.melt = melt(df.year)

df.year.melt = df.year.melt %>%
  group_by(variable) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate(pct_change = (value/lag(value) - 1) * 100)

df.year.melt = df.year.melt %>%
  na.omit()
df.year.melt$Year = as.Date(as.character(df.year.melt$Year),"%Y")
ggplot(df.year.melt) + 
  geom_line(aes(x=Year,y=pct_change,color = variable))

df.sport = melt(df)
df.sport = df.sport %>%
  group_by(Sport,variable) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate(pct_change = (value/lag(value) - 1) * 100)

df.sport$Year = as.numeric(df.sport$Year)
df.sport = df.sport %>%
  na.omit()

df.sport = df.sport %>%
  filter(variable != "Girls" | Sport != "Baseball") %>%
  filter(variable != "Girls" | Sport != "Football")
df.sport$Year = as.Date(as.character(df.sport$Year),"%Y")


ggplot(df.sport) +
  geom_line(aes(x=Year,y = pct_change,color = Sport)) +
  facet_wrap(~variable)


df.SB = read.csv("Sports Data - Football Viewership.csv")
df.SB$Date = as.Date(df.SB$Date,"%B %d,%Y")
df.SB$Year = lubridate::year(df.SB$Date)


df.SB$Viewers = gsub(",","",df.SB$Viewers)
df.SB$Viewers = as.numeric(df.SB$Viewers)
df.SB = df.SB %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(pct_change = (Viewers/lag(Viewers) - 1) * 100) %>%
  filter(Year %in% df$Year)

ggplot(df.SB) +
  geom_line(aes(x=Year,y = pct_change)) 
