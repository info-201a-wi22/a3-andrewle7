jail_jurisdiction <- read.csv("~/Documents/info201assignments/a3-andrewle7/incarceration-trends/incarceration_trends_jail_jurisdiction.csv")
incar_trends <-read.csv("~/Documents/info201assignments/a3-andrewle7/incarceration-trends/incarceration_trends.csv")
library(dplyr)
library(tidyr)
library(ggplot2)

blackpercpop <- incar_trends %>%
  summarize(totalpop = sum(total_pop, na.rm = TRUE ), 
    whitepop = sum(white_pop_15to64, na.rm = T),
    blackpop = sum(black_pop_15to64, na.rm = T),
    whiteperc = whitepop / totalpop,
    blackperc = blackpop / totalpop) %>%
    pull(blackperc) %>%
    round(4) * 100
    

whitepercpop <- incar_trends %>%
  summarize(totalpop = sum(total_pop, na.rm = TRUE ), 
            whitepop = sum(white_pop_15to64, na.rm = T),
            blackpop = sum(black_pop_15to64, na.rm = T),
            whiteperc = whitepop / totalpop,
            blackperc = blackpop / totalpop) %>%
            pull(whiteperc) %>%
            round(4) * 100

blackpercincar <- incar_trends %>%
  summarize(blackjail = sum(black_jail_pop, na.rm = T),
            totaljpop = sum(total_jail_pop, na.rm = T),
            blackjperc = blackjail/totaljpop) %>%
            pull(blackjperc) %>%
            round(4) * 100

whitepercincar <- incar_trends %>%
  summarize(whitejail = sum(white_jail_pop, na.rm = T),
            totaljpop = sum(total_jail_pop, na.rm = T),
            whitejperc = whitejail/totaljpop) %>%
            pull(whitejperc) %>%
            round(4) * 100


southjblackperc <- incar_trends %>%
  group_by(region) %>%
  summarize(blackjail = sum(black_jail_pop, na.rm = T),
            totaljpop = sum(total_jail_pop, na.rm = T),
            blackjperc = blackjail/totaljpop)%>%
            filter(blackjperc == max(blackjperc, na.rm = T)) %>%
            pull(blackjperc) %>%
            round(4) * 100
southtotalblackpop <- incar_trends %>%
  group_by(region) %>%
  summarize(totalpop = sum(total_pop, na.rm = TRUE),
            whitepop = sum(white_pop_15to64, na.rm = T),
            blackpop = sum(black_pop_15to64, na.rm = T),
            blackjail = sum(black_jail_pop, na.rm = T),
            totaljpop = sum(total_jail_pop, na.rm = T),
            blackperc = blackpop / totalpop)%>%
  filter(region == "South" )%>%
  pull(blackperc) %>%
  round(4) * 100
           
westjblackperc <- incar_trends %>%
  group_by(region) %>%
  summarize(whitepop = sum(white_pop_15to64, na.rm = T),
            blackpop = sum(black_pop_15to64, na.rm = T),
            blackjail = sum(black_jail_pop, na.rm = T),
            totaljpop = sum(total_jail_pop, na.rm = T),
            blackjperc = blackjail/totaljpop)%>%
            filter(region == "West" )%>%
            pull(blackjperc) %>%
  round(4) * 100

westtotalblackpop <- incar_trends %>%
  group_by(region) %>%
  summarize(totalpop = sum(total_pop, na.rm = TRUE),
            whitepop = sum(white_pop_15to64, na.rm = T),
            blackpop = sum(black_pop_15to64, na.rm = T),
            blackjail = sum(black_jail_pop, na.rm = T),
            totaljpop = sum(total_jail_pop, na.rm = T),
            blackperc = blackpop / totalpop)%>%
  filter(region == "West" )%>%
  pull(blackperc) %>%
  round(4) * 100
  

overtimedata <- incar_trends %>%
  group_by(year) %>%
  summarize (blackjail = sum(black_jail_pop, na.rm = T),
             totalpop = sum(total_pop, na.rm = TRUE),
             totaljpop = sum(total_jail_pop, na.rm = T),
             blackjperc = blackjail/totaljpop,
             whitepop = sum(white_pop_15to64, na.rm = T),
             blackpop = sum(black_pop_15to64, na.rm = T),
             whitejail = sum(white_jail_pop, na.rm = T),
             totaljpop = sum(total_jail_pop, na.rm = T),
             whiteperc = whitepop / totalpop,
             blackperc = blackpop / totalpop,
             whitejperc = whitejail/totaljpop)%>%
  filter(year >= 1990)


#legend
colors <- c("Jailed Black Population" = "darkred", "Jailed White Population" = "steelblue")

overtimechart <- ggplot(data = overtimedata, aes(x=year)) +
  geom_line(aes(y=blackjperc, color = "Jailed Black Population")) +
  geom_line(aes(y=whitejperc, color = "Jailed White Population")) +
  labs(
    x = "Year",
    y = "Percentage of Population (Jailed)",
    title = "Population Figures by Race in the US",
    subtitle = "Data from ggplot2() incar_trends data frame.",
    caption = "Assignment 3 Trends Over Time Chart",
    color = "Legend"
  ) +
  scale_color_manual(values = colors)

variabledata <- incar_trends %>%
  select(black_jail_pop, total_jail_pop, black_pop_15to64, total_pop_15to64)%>%
  summarize(blackjperc = black_jail_pop / total_jail_pop,
         blackperc1564 = black_pop_15to64 / total_pop_15to64)%>%
  filter(blackjperc < 1) %>%
  round(4) * 100

variableplot <- ggplot(data = variabledata) +
  geom_point(mapping = aes(x=blackjperc, y=blackperc1564)) +
  geom_smooth(mapping = aes(x=blackjperc, y=blackperc1564)) +
  labs(
    x = "Percent of Jail Population",
    y = "Percent of Total Population (15-64)",
    title = "Percent of Total and Jailed Population (Black)",
    subtitle = "Data from ggplot2() incar_trends data frame.",
    caption = "Assignment 3 Continuous Variables Chart",
  )
  
