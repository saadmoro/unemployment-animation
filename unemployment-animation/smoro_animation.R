library(XML)
library(methods)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(maps)
library(colorspace)
library(mapproj)
library(RColorBrewer)
library(gganimate)
library(gifski)
library(transformr)
library(png)

claims <- xmlParse(file = "r539cy.xml")
frameclaims <- xmlToDataFrame("r539cy.xml") #this dataset is MASSIVE, don't touch this

popcensusdata <- read.csv(file = 'popcensusdata.csv', header = TRUE)


dfClaims <- frameclaims %>%
  select(stateName, weekEnded, InitialClaims, ContinuedClaims)

dfClaims$weekEnded <- as.Date(dfClaims$weekEnded, "%m/%d/%Y")
dfClaims$InitialClaims <- as.numeric(gsub(",","",as.character(dfClaims$InitialClaims)))
dfClaims$ContinuedClaims <- as.numeric(gsub(",","",as.character(dfClaims$ContinuedClaims)))

claimtibble <- as_tibble(dfClaims)

allclaims <- claimtibble %>% 
  pivot_wider(names_from=stateName, values_from = c(InitialClaims, ContinuedClaims))

states <- as.data.frame(state.x77)
states$region <- tolower(rownames(states))
map <- map_data("state")

statejoin <- left_join(map, states, by="region")

popData <- popcensusdata

start_of_2020 <- as.Date("2020-01-01")


claims2020 <- dfClaims %>%
  filter(weekEnded >= start_of_2020)

claims2020$region <- tolower(claims2020$stateName)

claimjoin <- left_join(map, claims2020, by="region")

popData$region <- tolower(popData$X)

claimcapitajoin <- left_join(claimjoin, popData, by="region")

claimcapitajoin$X2019 <- as.numeric(gsub(",","",as.character(claimcapitajoin$X2019)))

first_weekFeb <- as.Date("2020-02-22")
claimExample <- claimjoin %>%
  filter(weekEnded >= first_weekFeb)

claimExample <- as_tibble(claimExample)

lastYear <- as.Date("2020-01-21")
claimCapita <- claimcapitajoin %>%
  filter(weekEnded >= first_weekFeb)

claimCapita <- as_tibble(claimCapita)


laborMap <- ggplot(claimCapita, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = ContinuedClaims / X2019), color = "black", size = 1) +
  scale_fill_distiller(type = "seq", palette = "Reds", trans = "reverse",
                       name = "Per Capita Continued \nUnemployment Claims") +
  coord_map() +
  theme_void(base_size = 20) +
  labs(title = "Continued Unemployment Benefits Claims per Capita on {frame_time}",
       subtitle = "Source: US Department of Labor Statistics")+
  transition_time(weekEnded)

printmap <- animate(laborMap, fps = 4, height = 475, width = 950)
anim_save("claims.gif", printmap)
