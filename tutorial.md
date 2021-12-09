
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

This tutorial is aimed to show and teach all the muggles and wizards who are interested in coding some _siriusly_ cool and less known types of plots you can generate to visualise your results using Rstudio, which despite looking pretty impressive are relatively easy to make and require only basic knowledge of R. If you are a beginner without previous experience with this language, I would recommend looking at___ first.

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

<p style="text-align:center;"><img src="outputs/parliament.png" alt="parliament" width="500"/>

### 3. Dinner at the Great Hall

### Waffle chart

Did you know that waffles are great not only for breakfast, but also for visualising your data?

A waffle chart illustrates the data of one or multiple categories. It can be used to compare them, or in a case of one, to show a progress towards our target.

Now we are going to create a simple waffle chart using the houses of our characters from the hp_characters dataset. We start with loading a waffle package
```
# Load the library, install if needed
library(waffle)

# Replacing NAs in our data with "No House"
unique(hp_characters$House)

hp_characters <- hp_characters %>%
  mutate(House = replace_na(House, "No House"))  

# Creating a subset dataframe of the column of our interest- House, let's call it chr
chr <- hp_characters %>%
  group_by(House) %>%
  tally() %>%
  ungroup()

# Generating our waffle chart
house_waffle <- waffle(c(G = 38, H = 13, NH = 39, S = 28, R = 18, D = 1, B = 3), rows = 10)
iron(house_waffle)
```
This is our output:
<p style="text-align:center;"><img src="outputs/waffle.png" alt="waffle" width="500"/>

### Pie chart

Now let's move to our second course, a pie! Pie charts might be a simple method of presenting your results, but you know what they say: there is a beauty in simplicity! And often it is a great chouce, as it shows the data in a _siriusly_ nice and clear way.


```
# Creating a pie chart using our chr subset dataframe
ggplot(chr, aes(x= "", y= n, fill=House)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_map() +
  scale_fill_manual(values= c("Beauxbatons Academy of Magic"= "orange","NoHouse"="grey","Durmstrang Institute"="black", "Gryffindor"="darkred", "Slytherin"="darkgreen", "Hufflepuff"="yellow", "Ravenclaw"="darkblue"))+
  theme(legend.position = "right")
```
You can play a bit with your proffered colours, but this is the graph we get with this code:

<p style="text-align:center;"><img src="outputs/pie.png" alt="pie" width="500"/>

### 4. The whomping willow; Tree map

Treemap is an alternative method of visualising the hierarchical structure of our data. Using a rectangles assigned to each category it also illustrates its quantity.

Let's make a nice treemap showing the distribution of each of our houses.
```
# Creating a treemap using our chr subset dataframe
ggplot(chr, aes(fill = House, area = n )) +
  geom_treemap() +
  scale_fill_manual(values= c("Beauxbatons Academy of Magic"= "orange","NoHouse"="grey","Durmstrang Institute"="black", "Gryffindor"="darkred", "Slytherin"="darkgreen", "Hufflepuff"="yellow", "Ravenclaw"="darkblue"))
```
Here we can see the output:

<p style="text-align:center;"><img src="outputs/tree.png" alt="tree" width="500"/>

### 5. Wingardium leviosa; Word cloud

Now let's give the words some wings an see them fly! A word cloud is a visualisation method that shows how frequent are the words in our data. This is done by the size of each word being proportional to its frequency. We will start by loading the extension wordcloud2
```
# load the needed library, install if required
library(wordcloud2)

# have a look to the example dataset
head(demoFreq)

hair <- hp_characters %>%
  group_by(Hair.colour) %>%
  tally() %>%
  ungroup()

# Creating the basic plot
wordcloud2(data=hair, size=1.6)
```
This is our output:


<p style="text-align:center;"><img src="outputs/wordcloud.png" alt="wordcloud" width="500"/>

You can get more creative with the plot by choosing the shape, colours or orientation of the text:

```
wordcloud2(data=hair, size=1.6, shape='star', color='red', backgroundColor="black")
```


### 6. Add a little magic to your bar-chart; Radial bar-chart

Bar-charts can get a bit boring. In wizards' world we add them a little pinch of magic to make them look more fascinating. Here we will turn an usual boring bar-chart into a radial/circular one.

```
# Creating a radial bar-chart
ggplot(chr, aes(x = House, y = n, fill = House)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values= c("Beauxbatons Academy of Magic"= "orange","NoHouse"="grey","Durmstrang Institute"="black", "Gryffindor"="darkred", "Slytherin"="darkgreen", "Hufflepuff"="yellow", "Ravenclaw"="darkblue"))
```
This is the chart we get:

<p style="text-align:center;"><img src="outputs/radial.png" alt="radial" width="500"/>

### 7. BONUS; Visualisation of an actual spell aka Data art

Did you know that even muggles can see spells? Thanks to R, we can now visualise even more abstract things from the wizards' world. This bonus graph is a tricky one, well beyond the basic knowledge of coding, so I won't go into any deep details, but the output is remarkably impressive, so feel free to copy the code, maybe play a bit with the colours and enjoy!

```

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

And this is how magical our output looks like:
<p style="text-align:center;"><img src="outputs/spell.png" alt="spell" width="500"/>

I hope you enjoyed the tutorial! If you have any questions, please contact me at:
barbora.ebringerova@gmail.com
