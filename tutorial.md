
# Impress your coding mates with these _riddikulusly_ magical data visualizations

Created by barbora8

## Tutorial aims:
1. Welcome to Hogwarts; Get familiar with less known plots
2. Hogwarts house student parliament; parliament chart
3. Dinner at the Great Hall;
-  Waffle chart
-  Pie chart
4. The whomping willow; Tree map
5. Wingardium leviosa; Word cloud
6. Give your bar-chart a little magic; Radial bar-chart
7. BONUS; Visualisation of a spell aka Data art



Data visualisation is a graphical representation of the data and is an important part of every project. There many different ways how we can visualise our results, some more common than others.

This tutorial is aimed to show and teach all the muggles and wizards who are interested in coding some _siriusly_ cool and less known types of plots you can generate to visualise your results using Rstudio, which despite looking pretty impressive are relatively easy to make and require only basic knowledge of R.

In this tutorial we will be using dataset on Harry Potter characters, which is publicly available from this repository.

In Rstudio we will import our data and load some general libraries (additional packages needed for specific graphs will be downloaded later in the tutorial). You can of course do a little exploring of your data to get familiar with it.
```
# Load the basic libraries
library(tidyverse)     
library(ggplot2)
library(ggthemes)

# Import our data
hp_characters <- read.csv("data/Characters.csv", sep = ";", na.strings = c("","NA"))
```
**Ready to _visualise_ some magic? Let's begin then!**

### 2. Hogwarts house student parliament; parliament chart

First on our list is a parliament chart. Parliament charts are used to visualise seats belonging to different groups we have in our data, usually used for a presentation of election results. In our tutorial we will use Hogwarts House column to see the distribution of the Harry Potter characters if Hogwarts had a parliament.

 We need a ggparliament extension in the ggplot package. This allows us to plot the parliament in different layouts, such as semicircle, horseshoe, circle, opposing bench or a classroom. Here we will use the classical semicircle parliament.

```
# Load a needed extension, if needed install first
library(ggparliament)

# Create the data frame to be used
data <- parliament_data()
semicircle <- parliament_data(election_data = chr,
                              type = "semicircle", # Parliament type
                              parl_rows = 6,      # Number of rows of the parliament
                              party_seats = chr$n) # Seats per party

# Plot our chart
ggplot(semicircle, aes(x = x, y = y, colour = House)) +
  geom_parliament_seats() +
  theme_ggparliament() +
  labs(title = "Hogwarts Parliament")
```
After running the code, the chart will look like this:

### 3. Dinner at the Great Hall;

### Waffle chart

```
library(waffle)

unique(hp_characters$House)

hp_characters <- hp_characters %>%
  mutate(House = replace_na(House, "No House"))  # Replacing NAs with No House

chr <- hp_characters %>%
  group_by(House) %>%
  tally() %>%
  ungroup()

house_waffle <- waffle(c(G = 38, H = 13, NH = 39, S = 28, R = 18, D = 1, B = 3), rows = 10)


iron(house_waffle)
```

### Pie chart

```
ggplot(chr, aes(x= "", y= n, fill=House)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_map() +
  theme(legend.position = "right")
```

### 4. The whomping willow; Tree map

```
library(treemapify)

ggplot(chr, aes(fill = House, area = n )) +
  geom_treemap()
```

### 5. Wingardium leviosa; Word cloud

```
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
wordcloud2(data=hair, size=1.6)
```

### 6. Give your bar-chart a little magic; Radial bar-chart


```
ggplot(chr, aes(x = House, y = n, fill = House)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_bw()
```

### 7. BONUS; Visualisation of a spell aka Data art

```
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
```
