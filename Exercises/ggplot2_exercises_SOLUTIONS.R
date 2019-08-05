############
###Exercises for ggplot2 AMAAS worshop
###########

#load required packages

require(knitr)
require(tidyverse)


#we will be using the msleep dataset that is included with ggplot. 
#This  file  contains  sleep habits of different animal species. It is a dataframe with 83 rows and 11 variables.
#original source: V. M. Savage and G. B. West. A quantitative, theoretical framework for understanding mammalian sleep. 
#Proceedings of the National Academy of Sciences, 104 (3):1051-1056, 2007.

#first explore the msleep dataset:

?msleep    #for variable definitions
View(msleep)   #full dataframe   83 obs x 11 variables
str(msleep)  #summary of each column

#The general form of a command will look like this:  MyGraph <- ggplot(MyData, aes(variable for x axis, variable for y axis))+geom()



#Say you want to look at the relationship between numbers of hours slept per day (sleep_total) and the weight of the animals brain (brainwt).

scatterplot<- ggplot(data=msleep, aes(x=brainwt, y=sleep_total))+geom_point()
scatterplot

#you can add a different colour for each trophic level, and change the size of the points
scatterplot<- ggplot(data=msleep, aes(x=brainwt, y=sleep_total, colour=vore, shape=vore))+geom_point(size=5)
scatterplot


#looks like there are a few orders of magnitude spread in brain weight in these animals. 
#Perhaps a log transform will make the data linear
scatterplot<- ggplot(data=msleep, aes(x=log(brainwt), y=sleep_total, colour=vore, shape=vore))+geom_point(size=5)
scatterplot

#some cosmetic changes to make the plot look more professional:

scatterplot<- ggplot(data=msleep, aes(x=log(brainwt), y=sleep_total, colour=vore, shape=vore))+geom_point(size=5)
scatterplot <- scatterplot + xlab("Log Brain Weight")+ylab("Total Hours Sleep")+ggtitle("Mammal sleep time versus brain weight") #axis titles
scatterplot <- scatterplot + scale_x_continuous(breaks=seq(-10, 4, 2))  #set x-axis breaks from -10 to 4 in steps of 2
scatterplot <- scatterplot + scale_colour_brewer(palette="Set1")   #you can set your own colour scheme or use premade "sets"
scatterplot <- scatterplot + theme_classic(base_size=13)  #I think the grey doesn't look very nice, theme_classic() or theme_bw() are popular
                                                          #there are many preset themes to choose from and customize things such as font or size
scatterplot


#Now lets try plotting a categorical variable on the x-axis
#We want to look at the average amount of sleep (sleep_total) per trophic level (vore).
sleepyvore <- ggplot(msleep, aes(x=vore, y=sleep_total, col=vore)) + geom_point() 
sleepyvore

#to prevent overlapping datapoints obscuring eachother, add some random noise using geom_jitter()
sleepyvore <- ggplot(msleep, aes(x=vore, y=sleep_total, col=vore)) + geom_jitter(position = position_jitter(width=0.2), size=5) #width determins how wide the jitter is
sleepyvore

#we can even overlay a boxplot if we want. Since we are overlaying the boxplots we need to make them transparant by specifying alpha between 0 and 1
sleepyvore <- sleepyvore + geom_boxplot(alpha=.2, colour="black")
sleepyvore

###########
##EXERCISES
##########

#1.A you want to know the distribution of body mass in your dataset. Visualise this using a geom_histogram()
#inside the geom, set an appropriate binwidth, colour to black, and fill to blue
ggplot(msleep, aes(x=bodywt)) + geom_histogram(colour="black", fill="blue")

#1.B. Everyone loves normal distributions, and bodywt is definitely not normal. 
#when plotting things over several orders of magnitude a log-transform works wonders.
#Generate the same plot but this time log-transformed bodywt
ggplot(msleep, aes(x=log(bodywt))) + geom_histogram(colour="black", fill="blue")

##2. You want to see if there are differences in sleep time between primates and non-primate mammals. Assess using a boxplot
#hint: First we have to create a new column for presence/absence of primate order using dplyr
msleep <- msleep %>%
  mutate(Primate = ifelse(order == "Primates", "Primate", "Non-primate"))

ggplot(msleep, aes(x=Primate, y=sleep_total, fill=Primate)) + geom_boxplot()

#3A Scatter plot log(brainwt) to sleep_total in the same scatter plot for primates and non primates. add a linear model using geom_smooth(method="lm")
bw_sleep <- ggplot(msleep, aes(x=log(brainwt), y=sleep_total, col=Primate)) + geom_point() + geom_smooth(method="lm")
bw_sleep

#3B use facet_grid(Primate~vore) to visualize the same relationship for trophic levels 

bw_sleep <- bw_sleep + facet_grid(Primate ~ vore)
bw_sleep

#4 Violin/bean plots are a variation on the theme of the boxplot. 
#They have the same structure but data density is mirrored by the shape of the body.
#make a violinplot (geom_violin) of log(body weight) for each trophic level
violinplot <- ggplot(msleep, aes(x=vore, y=log(bodywt)))+geom_violin()
violinplot

#4B plenty of room for improvement here. As you can see the data on the edges are trimmed by default.
#turn off trim inside the geom using trim=FALSE and fill the geom a nice colour using full= "xx" and a black edge using colour="xx"
violinplot <- ggplot(msleep, aes(x=vore, y=log(bodywt)))+geom_violin(trim=FALSE, fill="green", colour="black")
violinplot

#4C lets divide plots between primates and non-primates again. 
#This time, fill the violin plots with a different fill based on trophic level. 
violinplot <- ggplot(msleep, aes(x=vore, y=log(bodywt), fill=vore))+geom_violin(trim=FALSE) + facet_grid(~Primate)
violinplot

#5 plot the log of body weight on the x axis to brain weight on the y axis using a scatter plot. 
#In geom_smooth, Use different colours based on the taxonomic order and shape based on whether is is a primate or not
ggplot(msleep, aes(x=log(bodywt), y=log(brainwt))) + geom_point(size=3, aes(colour=order, shape=Primate))


#5B in the same plot, add a regression line for Primates and non-primates using geom_Smooth
ggplot(msleep, aes(x=log(bodywt), y=log(brainwt))) + geom_point(size=3, aes(colour=order, shape=Primate)) + geom_smooth(method="lm", aes(group=Primate))

#5C Finally, let's make the plot a bit prettier. Add appropriate labels to the axes and give the plot a title. 
#Have a look at different themes and select one you like
ggplot(msleep, aes(x=log(bodywt), y=log(brainwt))) + 
  geom_point(size=3, aes(colour=order, shape=Primate)) + 
  geom_smooth(method="lm", aes(group=Primate)) +
  xlab("Log body weight") +
  ylab("Log brain weight") +
  ggtitle("body size predicts brain size in primates and non-primate mammals") +
  theme_classic(base_size = 15)


#########
#Saving plots:
#########

#You can save plots using the export button above your plots in RStudio. However, you will have much more control over the plot properties
#if you use ggsave() which lets you set resolution, image name, type of image, etc.
#it takes the following arguments: ggsave(filename, plot = last_plot(), device = NULL, path = NULL,scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),dpi = 300, limitsize = TRUE, ...)
#example:
ggsave("bodybrain.png", width = 10, height=7, units = "in", dpi=300)
