#AMAAS: dplyr workshop answers
#Jaap Saers (jpps2@cam.ac.uk) & Benjamin Utting (bju23@cam.ac.uk)

#Doing a bit of data tidying======================================================
#Create a new dataset called "separated_location" with the "separate" function
separated_location <- goldman %>%
  separate(Location, into = c("Subregion", "Region", "Country"), sep = ",", fill = "left")
str(separated_location)

#This function returned "character" class data, so we must transform it into "factor"
#data in order to explore it
separated_location$Subregion <- as.factor(separated_location$Subregion)
separated_location$Region <- as.factor(separated_location$Region)
separated_location$Country <- as.factor(separated_location$Country)

#Explore data using "summary()" function
summary(separated_location$Subregion)
summary(separated_location$Region)
summary(separated_location$Country)

#Adding a "Continent" column======================================================

#Defining levels for each continent
EU <- c(" United Kingdom", "Austria", "Belgium", "France", "Germany", "Greenland",
        "Italy")
NAm <- c(" United States")
SAm <- c("Argentina", "Chile", "Ecuador", "Peru")
AFR <- c("Canary Islands", "Democratic Republic of the Congo", "Egypt", "Madagascar",
         "South Africa", "Sudan", "Tanzania")
ASIA <- c("Andaman Islands", "China", "Indonesia", "Japan", "Malaysia",
          "Philippine Islands", "Russia")
OCE <- c("Australia", "Papua New Guinea", "Solomon Islands", "Tasmania")
#Note the space before "United States" and "United Kingdom" - including this is 
#necessary in order to properly complete this task.

#Using "mutate()" to create a new column and define conditions for each new variable:
separated_location <- separated_location %>% 
  mutate(Continent = factor(case_when(Country %in% EU ~ "EU",
                                      Country %in% NAm ~ "NAm",
                                      Country %in% SAm ~ "SAm",
                                      Country %in% AFR ~ "AFR",
                                      Country %in% ASIA ~ "ASIA",
                                      Country %in% OCE ~ "OCE")))

#Take a look at what we have for each continent:
summary(separated_location$Continent)

#Finally, we can rearrange and define our new dataset.
separated_location <- separated_location %>% 
  select(Inst, ID, Sex, Age, NOTE, Continent, Country, Region, Subregion, everything())

View(separated_location)

#Questions======================================================

#1. Create a new dataframe using select() containing country, sex, and age
newdata <- separated_location %>%
  select(Sex, Age, Country) 

head(newdata)

#2. Create a new dataframe using select() containing all variables except NOTE and Inst
newdata <- separated_location %>%
  select(-NOTE, -Inst)

head(newdata)

#3. Create a new dataframe using select() containing all columns between LHUM and OSCX
newdata <- separated_location %>%
  select(LHUM:OSCX)

head(newdata)

#4. Create a new dataframe using select() containing all columns with femoral head measurements (hint: all columns contain the string "FHD")
newdata <- separated_location %>%
  select(contains("FHD"))

head(newdata)

#5. filter the dataset so that it only contains individuals from the subregion Alaska with both humeri present
newdata <- separated_location %>%
  filter(Region=="Alaska", 
         LHUM == 0, 
         RHUM ==0)

head(newdata)

#6. create a new dataset with only males that are between 50 and 70 kilograms (AVG.FHD for weight calculated from femoral head diameter)
newdata <- separated_location %>%
  filter(Sex=="M", 
         AVG.FHD >= 50, 
         AVG.FHD <=70)

head(newdata)

#7. filter out rows that are not from Egypt or the United states using the %in% operator
newdata <- separated_location %>%
  filter(Country %in% c(" United States", "Egypt"))

head(newdata)

#8. arrange separated_location dataset by continent, then by country show the first couple of rows using the head function

newdata <- separated_location %>%
  select(Continent, Country) %>%
  arrange(Continent, Country) 

head(newdata)

summary(newdata)

#9. Same as above, except include AVG.FHD and lastly arrange the rows in the AVG.FHD column in a descending order. 
#For this, use the function desc()

newdata <- separated_location %>%
  select(Continent, Country, AVG.FHD) %>%
  arrange(Continent, Country, desc(AVG.FHD)) 

head(newdata)

summary(newdata)

#10. We want to estimate body mass from the average of right and left humeral head diameters using a regression equation
#create a new column of estimated body mass using the following formula: BM = -17.555 + 1.803* humeral head diameter
#Remember we want the new column to be the average of left and right humeral head diameter

separated_location %>%
  select(RHHD, LHHD) %>%
  mutate(BM.HHD = 0.5*((-17.555+1.803*RHHD)+(-17.555 + 1.803*LHHD))) %>%
  head

#11. The summarise() function will create summary statistics for a given column in the data frame such as finding the mean. 
#For example, to compute the average tibia length, create a new column using mutate that averages the right and left tibia max length
#Then, use summarise() and apply the mean() function to the new column and call the summary value AVG.TML.
#hint: there are some NA values in the new column as not all right or left tibiae are present. therefore, when calling mean() it returns NA
#use mean(, na.rm=TRUE) to discount NA values
#an alternative way would be to use filter() to filter out individuals without a tibia length before summarising

separated_location %>%
  mutate(AVG.TML = 0.5*(LTML + RTML)) %>%
  summarise(mean_tml = mean(AVG.TML, na.rm=TRUE))

#12. you can summarise many things in one table. Using the same mean tibia length created in the previous exercise:
#provide the mean, SD, min, max, median, mode, and total n of average tibia lengths 

separated_location %>%
  mutate(AVG.TML = 0.5*(LTML + RTML)) %>%
  summarise(mean_tml = mean(AVG.TML, na.rm=TRUE),
            sd_tml = sd(AVG.TML, na.rm=TRUE),
            min_tml = min(AVG.TML, na.rm=TRUE),
            max_tml = max(AVG.TML, na.rm=TRUE),
            median_tml = median(AVG.TML, na.rm=TRUE),
            total_tml = n()
  )

#The group_by() verb is an important function in dplyr, it's related to concept of "split-apply-combine". 
#We want to split the data frame by some variable (e.g. continent), apply a function to the individual data frames and then combine the output.

#13. Let's do that: split the separated_location data frame by continent, 
#then ask for the same summary statistics as above. We expect a set of summary statistics for each Country.

separated_location %>%
  group_by(Continent) %>%
  mutate(AVG.TML = 0.5*(LTML + RTML)) %>%
  summarise(mean_tml = mean(AVG.TML, na.rm=TRUE),
            sd_tml = sd(AVG.TML, na.rm=TRUE),
            min_tml = min(AVG.TML, na.rm=TRUE),
            max_tml = max(AVG.TML, na.rm=TRUE),
            median_tml = median(AVG.TML, na.rm=TRUE),
            total_tml = n() 
  )


#14. Among recent humans brachial and crural indices are positively correlated with mean annual temperature, 
#High indices are found in tropical groups. Let's see if this is the case in our dataset
#Group the dataset by country, then region, then subregion, and summarise mean brachial and crural index of the LEFT HAND SIDE
#You'll have to calculate the anthropometric indices first:
#brachial index: max radius length*100 / max humerus length
#crural indices: max tibia length*100 / max femur length

newdata <- separated_location %>%
  group_by(Continent) %>%
  mutate(BI.L = (LRML*100)/LHML,
         CI.L = (LTML*100)/LFML
  ) %>%
  summarise(BI.L.avg = mean(BI.L, na.rm = TRUE),
            CI.L.avg = mean(CI.L, na.rm= TRUE),
            total = n())

View(newdata)
