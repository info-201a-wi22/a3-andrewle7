jail_jurisdiction <- read.csv("~/Documents/info201assignments/a3-andrewle7/incarceration-trends/incarceration_trends_jail_jurisdiction.csv")
incar_trends <-read.csv("~/Documents/info201assignments/a3-andrewle7/incarceration-trends/incarceration_trends.csv", stringsAsFactors =  FALSE)
library(dplyr)
library(tidyr)
library(ggplot2)

# 5 Values, percentages of population trends
blackpercpop <- incar_trends %>%
  summarize(totalpop = mean(total_pop, na.rm = TRUE ), 
    blackpop = mean(black_pop_15to64, na.rm = TRUE),
    blackperc = blackpop / totalpop) %>%
    pull(blackperc) %>%
    round(4) * 100
    

whitepercpop <- incar_trends %>%
  summarize(totalpop = mean(total_pop, na.rm = TRUE ), 
            whitepop = mean(white_pop_15to64, na.rm = TRUE),
            whiteperc = whitepop / totalpop) %>%
            pull(whiteperc) %>%
            round(4) * 100

blackpercincar <- incar_trends %>%
  summarize(blackjail = mean(black_jail_pop, na.rm = TRUE),
            totaljpop = mean(total_jail_pop, na.rm = TRUE),
            blackjperc = blackjail/totaljpop) %>%
            pull(blackjperc) %>%
            round(4) * 100

whitepercincar <- incar_trends %>%
  summarize(whitejail = mean(white_jail_pop, na.rm = TRUE),
            totaljpop = mean(total_jail_pop, na.rm = TRUE),
            whitejperc = whitejail/totaljpop) %>%
            pull(whitejperc) %>%
            round(4) * 100


highestjblackperc <- incar_trends %>%
  group_by(region) %>%
  summarize(blackjail = mean(black_jail_pop, na.rm = TRUE),
            totaljpop = mean(total_jail_pop, na.rm = TRUE),
            blackjperc = blackjail/totaljpop)%>%
            filter(blackjperc == max(blackjperc, na.rm = TRUE)) %>%
            pull(blackjperc) %>%
            round(4) * 100

southtotalblackpop <- incar_trends %>%
  group_by(region) %>%
  summarize(blackpop = mean(black_pop_15to64, na.rm = TRUE),
            totalpop = mean(total_pop_15to64, na.rm = TRUE),
            blackperc = blackpop / totalpop)%>%
  filter(region == "South")%>%
  pull(blackperc) %>%
  round(4) * 100
           
westjblackperc <- incar_trends %>%
  group_by(region) %>%
  summarize(blackjail = sum(black_jail_pop, na.rm = TRUE),
            totaljpop = sum(total_jail_pop, na.rm = TRUE),
            blackjperc = blackjail/totaljpop)%>%
            filter(region == "West" )%>%
            pull(blackjperc) %>%
  round(4) * 100

westtotalblackpop <- incar_trends %>%
  group_by(region) %>%
  summarize(totalpop = mean(total_pop, na.rm = TRUE),
            blackpop = mean(black_pop_15to64, na.rm = TRUE),
            blackperc = blackpop / totalpop)%>%
  filter(region == "West" )%>%
  pull(blackperc) %>%
  round(4) * 100
  
# Dataset for TOT Chart
overtimedata <- incar_trends %>%
  group_by(year) %>%
  summarize (blackjail = mean(black_jail_pop, na.rm = TRUE),
             totaljpop = mean(total_jail_pop, na.rm = TRUE),
             blackjperc = blackjail/totaljpop,
             whitejail = mean(white_jail_pop, na.rm = TRUE),
             totaljpop = mean(total_jail_pop, na.rm = TRUE),
             whitejperc = whitejail/totaljpop)%>%
  filter(year >= 1990)


#legend
colors <- c("Jailed Black Population" = "darkred", "Jailed White Population" = "steelblue")

#Trends over Time Chart

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

#Continuous Variable Dataset
variabledata <- incar_trends %>%
  select(black_jail_pop, total_jail_pop, black_pop_15to64, total_pop_15to64)%>%
  summarize(blackjperc = black_jail_pop / total_jail_pop,
         blackperc1564 = black_pop_15to64 / total_pop_15to64)%>%
  filter(blackjperc < 1) %>%
  round(4) * 100

#Variable lot
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

# MAP DATASET
mapdf <- incar_trends %>%
  group_by(fips)%>%
  summarize(blackjperc = round(mean(black_jail_pop, na.rm=T) / mean(total_jail_pop,na.rm = T), 4)) %>%
  filter(blackjperc< 1) %>%
  mutate(blackjpercc = blackjperc * 100)
  
# FIPS STATE DATASET
mapcountyfips <- readRDS('~/Documents/info201assignments/a3-andrewle7/source/county_map_fips.RDS', refhook = NULL)
         
map_countyj <- mapcountyfips %>%
  left_join(mapdf, by = "fips")

# Minimalistic Theme         
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),       
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()
  )

#map labels
maplabels <- labs(
  title = "Average Percentage of Black People in Total Jailed Population in US",
  subtitle = "Data taken by Vera, from 1970 - 2018",
)

# Map Visual                     
jpercmap <-   ggplot(data = map_countyj, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = blackjpercc)) +
    scale_fill_gradient(low = "blue", high = "red") +
    coord_quickmap() +
  maplabels +
    blank_theme

jpercmap <- jpercmap + guides(fill=guide_legend(title="Average Percent of Pop."))
