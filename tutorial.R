# Impress your coding mates with these riddikulusly magical data visualizations
# according to barbora8


# 1. Packages ----
# install.packages("waffle")
# install.packages("treemapify")
# install.packages("ggparliament")
library(waffle)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(treemapify)
library(ggparliament)

hp_characters <- read.csv("data/Characters.csv", sep = ";", na.strings = c("","NA"))


# 2. Exploring the dataset ----
View(hp_characters)

# How many characters are in the dataset
length(unique(hp_characters$Name))

unique(hp_characters$House)
# We can see that some characters do not have a house
length(unique(hp_characters$House == "Ravenclaw"))

# to see
length(unique(hp_characters$Blood.status))
unique(hp_characters$Blood.status)

length(unique(hp_characters$Species))
unique(hp_characters$Species)

length(unique(hp_characters$Eye.colour))

hp_characters <- hp_characters %>% 
  mutate(House = replace_na(House, "No House"))  # Replacing NAs with No House

# 3. WAFFLE ----
unique(hp_characters$House)

hp_characters <- hp_characters %>% 
  mutate(House = replace_na(House, "No House"))  # Replacing NAs with No House

chr <- hp_characters %>% 
  group_by(House) %>% 
  tally() %>% 
  ungroup()

house_waffle <- waffle(c(Gryffindor = 38, Hufflepuff = 13, Slytherin = 28, Ravenclaw = 18, NoHouse = 39, Durmstrang  = 1, Beauxbatons = 3), rows = 10)


iron(house_waffle)


# 4. TREE MAP ----
ggplot(chr, aes(fill = House, area = n )) +
  geom_treemap()

# 5. PARLIAMENT ----

data <- parliament_data()
# Create the data frame to be used
semicircle <- parliament_data(election_data = chr,
                              type = "semicircle", # Parliament type
                              parl_rows = 6,      # Number of rows of the parliament
                              party_seats = chr$n) # Seats per party

ggplot(semicircle, aes(x = x, y = y, colour = House)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Hogwarts Parliament") 

# 6. PIECHART ----
ggplot(chr, aes(x= "", y= n, fill=House)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_map() +
  theme(legend.position = "right")

# 7. WORD CLOUD ----

# library
# install.packages("wordcloud2")
library(wordcloud2) 

# have a look to the example dataset
head(demoFreq)

hair <- hp_characters %>%
  group_by(Hair.colour) %>%
  tally() %>%
  ungroup()

# Basic plot
wordcloud2(data=hair, size=1.6, shape = 'triangle')


# 8. Radial barchart ----

ggplot(chr, aes(x = House, y = n, fill = House)) + 
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_bw()


---

# Lets give Horace a gender
hp_characters <- hp_characters %>% 
  mutate(Gender = replace_na(Gender, "Male"))  # Replacing NAs with Male

gender <- hp_characters %>% 
  group_by(House, Gender) %>% 
  tally()



# 9. DATA ART ----
# USING RSTUDIO FOR VIZUALIZtion of ACTUALL SPELLS

par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(1,1,1,1))
plot(0,0,type="n", xlim=c(-2,32), ylim=c(3,27),
     xaxs="i", yaxs="i", axes=FALSE, xlab=NA, ylab=NA,
     asp=1)

for (j in 0:35) {
  for (i in 0:35) {
    
    R <- 8
    alpha <- j*10
    X <- 15+R*cos(alpha/180*pi)
    Y <- 15+R*sin(alpha/180*pi)
    
    r <- 3
    beta <- i*10
    x <- 15+r*cos(beta/180*pi)
    y <- 15+r*sin(beta/180*pi)
    
    d1 <- sqrt((X-x)^2+(Y-y)^2)
    xc <- x
    yc <- y
    
    n <- 180-atan((Y-y)/(X-x))/pi*180
    
    alpha2 <- -(0:n)
    theta <- alpha2/180*pi
    
    b <- d1/(n/180*pi)
    r <- b*theta
    
    x1 <- xc+r*cos(theta)
    y1 <- yc+r*sin(theta)
    
    lines(x1,y1, col="blue")
    
  }
}



