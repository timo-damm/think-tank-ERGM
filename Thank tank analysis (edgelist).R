##################################
# Think tanks network Statistics # 
##################################
# Based on the code from the websites below
# Bipartite/Two-Mode Networks in statnet # https://rpubs.com/pjmurphy/542335
# Intro to networks in R # https://rstudio-pubs-static.s3.amazonaws.com/470420_91f1d47c90c54b879aa08ce93fa66730.html
# Centrality in statnet - for the most part # https://rpubs.com/pjmurphy/546652
# I departed quite a bit from this if any questions come up, do feel free to 
# send an Email to timo.damm.bs@icloud.com ~Timo

#----general preparations----
library(statnet)
library(mice)
library(ggplot2)
library(GGally)
library(MASS)
library(readxl)
library(tidyverse)
library(fastnet)
library(scales)

#-----data import----
# Entering the edgelists (note that the filepath is specific to my machine, so you
# will need to modify it in order to make it work)
TTboard1998 <- read_excel("/home/td/random_coding/think tank work/data/1998TTboard.xls",col_names = FALSE,na = "")
TTboard1999 <- read_excel("/home/td/random_coding/think tank work/data/1999TTboard.xls",col_names = FALSE,na = "")
TTboard2000 <- read_excel("/home/td/random_coding/think tank work/data/2000TTboard.xls",col_names = FALSE,na = "")
TTboard2001 <- read_excel("/home/td/random_coding/think tank work/data/2001TTboard.xls",col_names = FALSE,na = "")
TTboard2002 <- read_excel("/home/td/random_coding/think tank work/data/2002TTboard.xls",col_names = FALSE,na = "")
TTboard2003 <- read_excel("/home/td/random_coding/think tank work/data/2003TTboard.xls",col_names = FALSE,na = "")
TTboard2004 <- read_excel("/home/td/random_coding/think tank work/data/2004TTboard.xls",col_names = FALSE,na = "")
TTboard2005 <- read_excel("/home/td/random_coding/think tank work/data/2005TTboard.xls",col_names = FALSE,na = "")
TTboard2006 <- read_excel("/home/td/random_coding/think tank work/data/2006TTboard.xls",col_names = FALSE,na = "")
TTboard2007 <- read_excel("/home/td/random_coding/think tank work/data/2007TTboard.xls",col_names = FALSE,na = "")
TTboard2008 <- read_excel("/home/td/random_coding/think tank work/data/2008TTboard.xls",col_names = FALSE,na = "")
TTboard2009 <- read_excel("/home/td/random_coding/think tank work/data/2009TTboard.xls",col_names = FALSE,na = "")
TTboard2010 <- read_excel("/home/td/random_coding/think tank work/data/2010TTboard.xls",col_names = FALSE,na = "")
TTboard2011 <- read_excel("/home/td/random_coding/think tank work/data/2011TTboard.xls",col_names = FALSE,na = "")

#importing the think tank attributes: 
TTattributes1998 <- read_excel("/home/td/random_coding/think tank work/data/1998 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes1999 <-read_excel("/home/td/random_coding/think tank work/data/1999 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2000 <-read_excel("/home/td/random_coding/think tank work/data/2000 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2001 <-read_excel("/home/td/random_coding/think tank work/data/2001 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2002 <-read_excel("/home/td/random_coding/think tank work/data/2002 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2003 <-read_excel("/home/td/random_coding/think tank work/data/2003 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2004 <-read_excel("/home/td/random_coding/think tank work/data/2004 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2005 <-read_excel("/home/td/random_coding/think tank work/data/2005 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2006 <-read_excel("/home/td/random_coding/think tank work/data/2006 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2007 <-read_excel("/home/td/random_coding/think tank work/data/2007 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2008 <-read_excel("/home/td/random_coding/think tank work/data/2008 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2009 <-read_excel("/home/td/random_coding/think tank work/data/2009 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2010 <-read_excel("/home/td/random_coding/think tank work/data/2010 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2011 <-read_excel("/home/td/random_coding/think tank work/data/2011 think tank attributes.xls",col_names = TRUE,na = "")

#importing the list of "problematic think tanks"  
#TTproblematic <- read_excel("/home/td/random_coding/think tank work/data/Think tanks with more than double increase in board size (1998-2011).xlsx",col_names = TRUE,na = "")

#----data preparation and cleaning----
# Missing data
sum(is.na(TTboard1999)) 
sum(is.na(TTboard2000)) 
sum(is.na(TTboard2001)) 
sum(is.na(TTboard2002)) 
sum(is.na(TTboard2003)) 
sum(is.na(TTboard2004)) 
sum(is.na(TTboard2005)) 
sum(is.na(TTboard2006)) 
sum(is.na(TTboard2007)) 
sum(is.na(TTboard2008))
sum(is.na(TTboard2009))
sum(is.na(TTboard2010))
sum(is.na(TTboard2011)) #no missing data was found 

#extracting the attributes of only the think tanks present in the data
uniqueTT <- data.frame(unique(TTboard1998[ ,2]))
colnames(uniqueTT) <- c("ID")
TTattributes1998 <- merge(TTattributes1998, uniqueTT, by = "ID")
TTattributes1999 <- merge(TTattributes1999, uniqueTT, by = "ID")
TTattributes2000 <- merge(TTattributes2000, uniqueTT, by = "ID")
TTattributes2001 <- merge(TTattributes2001, uniqueTT, by = "ID")
TTattributes2002 <- merge(TTattributes2002, uniqueTT, by = "ID")
TTattributes2003 <- merge(TTattributes2003, uniqueTT, by = "ID")
TTattributes2004 <- merge(TTattributes2004, uniqueTT, by = "ID")
TTattributes2005 <- merge(TTattributes2005, uniqueTT, by = "ID")
TTattributes2006 <- merge(TTattributes2006, uniqueTT, by = "ID")
TTattributes2007 <- merge(TTattributes2007, uniqueTT, by = "ID")
TTattributes2008 <- merge(TTattributes2008, uniqueTT, by = "ID")
TTattributes2009 <- merge(TTattributes2009, uniqueTT, by = "ID")
TTattributes2010 <- merge(TTattributes2010, uniqueTT, by = "ID")
TTattributes2011 <- merge(TTattributes2011, uniqueTT, by = "ID")

#preparation (this was done because the as.network() function works with two 
# columns of the same type)
TTboard1998$...2 <- as.character(TTboard1998$...2)
TTboard1999$...2 <- as.character(TTboard1999$...2)
TTboard2000$...2 <- as.character(TTboard2000$...2)
TTboard2001$...2 <- as.character(TTboard2001$...2)
TTboard2002$...2 <- as.character(TTboard2002$...2)
TTboard2003$...2 <- as.character(TTboard2003$...2)
TTboard2004$...2 <- as.character(TTboard2004$...2)
TTboard2005$...2 <- as.character(TTboard2005$...2)
TTboard2006$...2 <- as.character(TTboard2006$...2)
TTboard2007$...2 <- as.character(TTboard2007$...2)
TTboard2008$...2 <- as.character(TTboard2008$...2)
TTboard2009$...2 <- as.character(TTboard2009$...2)
TTboard2010$...2 <- as.character(TTboard2010$...2)
TTboard2011$...2 <- as.character(TTboard2011$...2)

# Conversion from Edgelist to Network in statnet
netTTboard1998 <- as.network(TTboard1998, bipartite = TRUE, directed=FALSE)
netTTboard1999 <- as.network(TTboard1999, bipartite=T, directed=FALSE)
netTTboard2000 <- as.network(TTboard2000, bipartite=T, directed=FALSE)
netTTboard2001 <- as.network(TTboard2001, bipartite=T, directed=FALSE)
netTTboard2002 <- as.network(TTboard2002, bipartite=T, directed=FALSE)
netTTboard2003 <- as.network(TTboard2003, bipartite=T, directed=FALSE)
netTTboard2004 <- as.network(TTboard2004, bipartite=T, directed=FALSE)
netTTboard2005 <- as.network(TTboard2005, bipartite=T, directed=FALSE)
netTTboard2006 <- as.network(TTboard2006, bipartite=T, directed=FALSE)
netTTboard2007 <- as.network(TTboard2007, bipartite=T, directed=FALSE)
netTTboard2008 <- as.network(TTboard2008, bipartite=T, directed=FALSE)
netTTboard2009 <- as.network(TTboard2009, bipartite=T, directed=FALSE)
netTTboard2010 <- as.network(TTboard2010, bipartite=T, directed=FALSE)
netTTboard2011 <- as.network(TTboard2011, bipartite=T, directed=FALSE)

#creating one big file for descriptives across all years
TTboard_full <- rbind(TTboard1998, TTboard1999, TTboard2000, TTboard2001, TTboard2002, 
                      TTboard2003, TTboard2004, TTboard2005, TTboard2006, TTboard2007, 
                      TTboard2008, TTboard2009, TTboard2010, TTboard2011)
netTTboard_full <- as.network(TTboard_full, bipartite=T, directed=F, multiple = T) #to allow for multiplex edges

#----finding "jumps" in the numbers of board members----
change98 <-  TTboard1998 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change99 <- TTboard1999 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change00 <-TTboard2000 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change01 <- TTboard2001 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change02 <- TTboard2002 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change03 <- TTboard2003 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change04 <- TTboard2004 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change05 <- TTboard2005 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change06 <- TTboard2006 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change07 <- TTboard2007 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change08 <- TTboard2008 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change09 <- TTboard2009 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change10 <- TTboard2010 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())
change11 <- TTboard2011 %>% filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n())

change_full <- merge(change98, change99, by = "...2")
change_full <- merge(change_full, change00, by = "...2")
change_full <- merge(change_full, change01, by = "...2")
change_full <- merge(change_full, change02, by = "...2")
change_full <- merge(change_full, change03, by = "...2")
change_full <- merge(change_full, change04, by = "...2")
change_full <- merge(change_full, change05, by = "...2")
change_full <- merge(change_full, change06, by = "...2")
change_full <- merge(change_full, change07, by = "...2")
change_full <- merge(change_full, change08, by = "...2")
change_full <- merge(change_full, change09, by = "...2")
change_full <- merge(change_full, change10, by = "...2")
change_full <- merge(change_full, change11, by = "...2")
change_full <- as.data.frame(t(change_full))
colnames <- change_full[1,]
change_full <- change_full[-1,] 
change_full <- as.data.frame(lapply(change_full, as.numeric))
colnames(change_full) <- colnames #this table contains
#changes in board members for all think tanks for all years

sd <- change_full %>% 
  summarise_if(is.numeric,sd) 
colnames(sd) <- colnames
sd <- as.data.frame(t(sd))
sd <- sort(sd, decreasing =  T)
sd1 <- slice(sd, 1:20)
#from this we can see the different SDs
barplot(height = sd$V1, names.arg = rownames(sd), las = 2)
barplot(height = sd1$V1, names.arg = rownames(sd1), las = 2)


#----Descriptive statistics for the two mode network----
# number of board members for each year
mode98 <- count(unique(TTboard1998[,1]))
mode99 <- count(unique(TTboard1999[,1])) 
mode00 <- count(unique(TTboard2000[,1]))
mode01 <- count(unique(TTboard2001[,1]))
mode02 <- count(unique(TTboard2002[,1]))
mode03 <- count(unique(TTboard2003[,1]))
mode04 <- count(unique(TTboard2004[,1]))
mode05 <- count(unique(TTboard2005[,1]))
mode06 <- count(unique(TTboard2006[,1]))
mode07 <- count(unique(TTboard2007[,1]))
mode08 <- count(unique(TTboard2008[,1]))
mode09 <- count(unique(TTboard2009[,1]))
mode10 <- count(unique(TTboard2010[,1]))
mode11 <- count(unique(TTboard2011[,1]))

#total number of unique board members
count(unique(TTboard_full[,1]))

#plotting the total number of board members over the 14 years: 
total_dev <- c(mode98[[1]], mode99[[1]], mode00[[1]], mode01[[1]] , mode02[[1]], mode03[[1]],
              mode04[[1]], mode05[[1]], mode06[[1]], mode07[[1]], mode08[[1]], mode09[[1]], 
              mode10[[1]], mode11[[1]])
total_dev <- as.data.frame(total_dev)
total_dev$year <- c("1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005",
                    "2006", "2007", "2008", "2009", "2010", "2011")


barplot(height = total_dev$total_dev, names = total_dev$year, xlab = "year", ylab = "total board members")
#average number of total board members
sum(mode98[[1]], mode99[[1]], mode00[[1]], mode01[[1]] , mode02[[1]], mode03[[1]],
    mode04[[1]], mode05[[1]], mode06[[1]], mode07[[1]], mode08[[1]], mode09[[1]], 
    mode10[[1]], mode11[[1]]) / 14 
#average number of members per board per year:
mode98[[1]]/374
mode99[[1]]/374
mode00[[1]]/374
mode01[[1]]/374
mode02[[1]]/374
mode03[[1]]/374
mode04[[1]]/374
mode05[[1]]/374
mode06[[1]]/374
mode07[[1]]/374
mode08[[1]]/374
mode09[[1]]/374
mode10[[1]]/374
mode11[[1]]/374

#----descriptive statistics for the think tank attributes-----
#affiliations
sum(TTattributes2011$Corporate_affiliation)
sum(TTattributes2011$Government_affiliation)
sum(TTattributes2011$Non_profit_affiliation)
sum(TTattributes2011$University_affiliation)
sum(TTattributes2011$Thinktank_affiliation)

sd(TTattributes2011$Corporate_affiliation)
sd(TTattributes2011$Government_affiliation)
sd(TTattributes2011$Non_profit_affiliation)
sd(TTattributes2011$University_affiliation)
sd(TTattributes2011$Thinktank_affiliation)

#lifespan
sum(!is.na(TTattributes2011$Founded))
mean(TTattributes2011$Founded, na.rm = TRUE)
min(TTattributes2011$Founded, na.rm = TRUE)
max(TTattributes2011$Founded, na.rm = TRUE)
sd(TTattributes2011$Founded, na.rm = TRUE)
TTattributes2011$Death <- as.numeric(TTattributes2011$Death)
sum(!is.na(TTattributes2011$Death))
mean(TTattributes2011$Death, na.rm = TRUE)
min(TTattributes2011$Death, na.rm = TRUE)
max(TTattributes2011$Death, na.rm = TRUE)
sd(TTattributes2011$Death, na.rm = TRUE)

TTattributes2011$Founded <- as.numeric(TTattributes2011$Founded)
unique(TTattributes2011$Founded)
barplot(height = unique(TTattributes2011$Founded))

#budget (compared to 1998)
sum(!is.na(TTattributes1998$budget))
mean(TTattributes1998$budget, na.rm = TRUE)
min(TTattributes1998$budget, na.rm = TRUE)
max(TTattributes1998$budget, na.rm = TRUE)
sd(TTattributes1998$budget, na.rm = TRUE)

sum(!is.na(TTattributes2011$budget))
mean(TTattributes2011$budget, na.rm = TRUE)
min(TTattributes2011$budget, na.rm = TRUE)
max(TTattributes2011$budget, na.rm = TRUE)
sd(TTattributes2011$budget, na.rm = TRUE)

#funding
sum(TTattributes2011$Corpfnd, na.rm = T)
mean(TTattributes2011$Corpfnd, na.rm = TRUE)
min(TTattributes2011$Corpfnd, na.rm = TRUE)
max(TTattributes2011$Corpfnd, na.rm = TRUE)
sd(TTattributes2011$Corpfnd, na.rm = TRUE)

sum(TTattributes2011$Govfnd, na.rm = T)
mean(TTattributes2011$Govfnd, na.rm = TRUE)
min(TTattributes2011$Govfnd, na.rm = TRUE)
max(TTattributes2011$Govfnd, na.rm = TRUE)
sd(TTattributes2011$Govfnd, na.rm = TRUE)

sum(TTattributes2011$Indfnd, na.rm = T)
mean(TTattributes2011$Indfnd, na.rm = TRUE)
min(TTattributes2011$Indfnd, na.rm = TRUE)
max(TTattributes2011$Indfnd, na.rm = TRUE)
sd(TTattributes2011$Indfnd, na.rm = TRUE)

sum(TTattributes2011$NGOfnd, na.rm = T)
mean(TTattributes2011$NGOfnd, na.rm = TRUE)
min(TTattributes2011$NGOfnd, na.rm = TRUE)
max(TTattributes2011$NGOfnd, na.rm = TRUE)
sd(TTattributes2011$NGOfnd, na.rm = TRUE)

#templates
sum(TTattributes2011$brookings_dummy, na.rm = T)
mean(TTattributes2011$brookings_dummy, na.rm = TRUE)
min(TTattributes2011$brookings_dummy, na.rm = TRUE)
max(TTattributes2011$brookings_dummy, na.rm = TRUE)
sd(TTattributes2011$brookings_dummy, na.rm = TRUE)

sum(TTattributes2011$rand_dummy, na.rm = T)
mean(TTattributes2011$rand_dummy, na.rm = TRUE)
min(TTattributes2011$rand_dummy, na.rm = TRUE)
max(TTattributes2011$rand_dummy, na.rm = TRUE)
sd(TTattributes2011$rand_dummy, na.rm = TRUE)

sum(TTattributes2011$heritage_dummy, na.rm = T)
mean(TTattributes2011$heritage_dummy, na.rm = TRUE)
min(TTattributes2011$heritage_dummy, na.rm = TRUE)
max(TTattributes2011$heritage_dummy, na.rm = TRUE)
sd(TTattributes2011$heritage_dummy, na.rm = TRUE)

#----1. biggest and smallest board across the years and in total----
TTboard_full %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard_full %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard1998 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard1998 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard1999 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard1999 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2000 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2000 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2001 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2001 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2002%>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2002 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2003 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2003 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2004 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2004 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2005 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2005 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2006 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2006 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2007 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2007 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2008 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2008 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2009 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2009 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2010 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2010 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)
TTboard2011 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)
TTboard2011 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  slice(1)


# standard deviations
sd98 <- TTboard1998 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd98$n)
sd99 <- TTboard1999 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd99$n)
sd00 <- TTboard2000 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd00$n)
sd01 <- TTboard2001 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd01$n)
sd02 <- TTboard2002 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd02$n)
sd03 <- TTboard2003 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd03$n)
sd04 <- TTboard2004 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd04$n)
sd05 <- TTboard2005 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd05$n)
sd06 <- TTboard2006 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd06$n)
sd07 <- TTboard2007 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd07$n)
sd08 <- TTboard2008 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd08$n)
sd09 <- TTboard2009 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd09$n)
sd10 <- TTboard2010 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd10$n)
sd11 <- TTboard2011 %>%
  filter(!is.na(...2)) %>%
  group_by(...2) %>%
  summarize(n = n()) 
sd(sd11$n)

# -----2. think tanks founded each year-----
TTattributes2011$Founded <- as.numeric(TTattributes2011$Founded)
founded <- TTattributes2011 %>%
  filter(!is.na(Founded)) %>%
  group_by(Founded) %>%
  summarize(n = n())
ggplot(data = TTattributes2011, aes(Founded), colour = 'gray') +
geom_histogram(binwidth = 0.5) +
  ggtitle('Think Tanks Founded', '1900-2011') +
  xlab('Year') + ylab('n') +
  scale_x_continuous(limits=c(1900, 2011), breaks=c(1900, 1920, 1940, 1950, 1960,
                                                    1965,1970,1975,1980,1985,1990,
                                                    1995,2000, 2005, 2010)) +
  theme(text=element_text(size=16,family = 'Times New Roman')) +
  theme_minimal()

#think tanks that died 
TTattributes2011$Death <- as.numeric(TTattributes2011$Death)
ggplot(data = TTattributes2011, aes(Death), colour = 'gray') +
  geom_histogram(binwidth = 0.5) +
  ggtitle('Think Tank Deaths', '2000-2016') +
  xlab('Year') + ylab('n') +
  scale_x_continuous(limits=c(2000, 2016), breaks=c(2000, 2002, 2004, 2006, 2008,
                                                    2010, 2012, 2014, 2016)) +
  theme(text=element_text(size=16,family = 'Times New Roman')) +
  theme_minimal()

# ----3. ideology across the years----
#make three ideology columns into one column
i1998 <- c(sum(TTattributes1998$left, na.rm = T),sum(TTattributes1998$centrist, na.rm = T),
           sum(TTattributes1998$right, na.rm = T))
i1999 <-  c(sum(TTattributes1999$left, na.rm = T),sum(TTattributes1999$centrist, na.rm = T),
            sum(TTattributes1999$right, na.rm = T))
i2000 <-  c(sum(TTattributes2000$left, na.rm = T),sum(TTattributes2000$centrist, na.rm = T),
            sum(TTattributes2000$right, na.rm = T))
i2001 <-  c(sum(TTattributes2001$left, na.rm = T),sum(TTattributes2001$centrist, na.rm = T),
            sum(TTattributes2001$right, na.rm = T))
i2002 <-  c(sum(TTattributes2002$left, na.rm = T),sum(TTattributes2002$centrist, na.rm = T),
            sum(TTattributes2002$right, na.rm = T))
i2003 <-  c(sum(TTattributes2003$left, na.rm = T),sum(TTattributes2003$centrist, na.rm = T),
            sum(TTattributes2003$right, na.rm = T))
i2004 <-  c(sum(TTattributes2004$left, na.rm = T),sum(TTattributes2004$centrist, na.rm = T),
            sum(TTattributes2004$right, na.rm = T))
i2005 <-  c(sum(TTattributes2005$left, na.rm = T),sum(TTattributes2005$centrist, na.rm = T),
            sum(TTattributes2005$right, na.rm = T))
i2006 <-  c(sum(TTattributes2006$left, na.rm = T),sum(TTattributes2006$centrist, na.rm = T),
            sum(TTattributes2006$right, na.rm = T))
i2007 <-  c(sum(TTattributes2007$left, na.rm = T),sum(TTattributes2007$centrist, na.rm = T),
            sum(TTattributes2007$right, na.rm = T))
i2008 <-  c(sum(TTattributes2008$left, na.rm = T),sum(TTattributes2008$centrist, na.rm = T),
            sum(TTattributes2008$right, na.rm = T))
i2009 <-  c(sum(TTattributes2009$left, na.rm = T),sum(TTattributes2009$centrist, na.rm = T),
            sum(TTattributes2009$right, na.rm = T))
i2010 <-  c(sum(TTattributes2010$left, na.rm = T),sum(TTattributes2010$centrist, na.rm = T),
            sum(TTattributes2010$right, na.rm = T))
i2011 <-  c(sum(TTattributes2011$left, na.rm = T),sum(TTattributes2011$centrist, na.rm = T),
            sum(TTattributes2011$right, na.rm = T))


ideology <- data.frame(i1998, i1999, i2000, i2001, i2002, i2003, i2004,
                       i2005, i2006, i2007, i2008, i2009, i2010, i2011) 
ideology <- as.data.frame(t(ideology))
colnames(ideology) <- c('left', 'centrist', 'right')
write.table(ideology, file = "ideology.txt", sep = ",", quote = FALSE, row.names = F)

#plotting the number of politically left think tanks
ideology$left <- as.numeric(ideology$left)
ideology$year <- c(1998:2011)
ggplot(data = ideology, colour = 'gray') +
  geom_col(aes(year,left), width = 0.75) +
  ggtitle('Politically Left Think Tanks', '1998-2011') +
  xlab('Year') + ylab('n') +
  theme(text=element_text(size=16,family = 'Times New Roman')) +
  theme_minimal()


# ----Plotting a bipartite Network----
gplot(netTTboard_full)

gplot(netTTboard1998, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard1999, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2000, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2001, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2002, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2003, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2004, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2005, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2006, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2007, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2009, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2010, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")
gplot(netTTboard2011, gmode="twomode", usearrows = FALSE, displaylabels = FALSE, edge.col="gray")

############################################
# Converting Two-Mode to One-Mode Networks #
############################################

# converting the nets to rectangular matrices to count overlap
bipartite_matrix_1998 <- as.matrix(netTTboard1998)
bipartite_matrix_1999 <- as.matrix(netTTboard1999)
bipartite_matrix_2000 <- as.matrix(netTTboard2000)
bipartite_matrix_2001 <- as.matrix(netTTboard2001)
bipartite_matrix_2002 <- as.matrix(netTTboard2002)
bipartite_matrix_2003 <- as.matrix(netTTboard2003)
bipartite_matrix_2004 <- as.matrix(netTTboard2004)
bipartite_matrix_2005 <- as.matrix(netTTboard2005)
bipartite_matrix_2006 <- as.matrix(netTTboard2006)
bipartite_matrix_2007 <- as.matrix(netTTboard2007)
bipartite_matrix_2008 <- as.matrix(netTTboard2008)
bipartite_matrix_2009 <- as.matrix(netTTboard2009)
bipartite_matrix_2010 <- as.matrix(netTTboard2010)
bipartite_matrix_2011 <- as.matrix(netTTboard2011)

###############
# Think tanks #
###############
# Convert 2-mode matrix to a 1-mode matrix
# Make sure the diagonal elements are zero since those are simply self-referential ties and in this case it's the size of think tank board
#member_matrix_prod_1998 <- bipartite_matrix_1998 %*% t(bipartite_matrix_1998) #right now the board member network and not the think tank network
#diag(member_matrix_prod_1998) <- 0
org_matrix_prod_1998 <-  t(bipartite_matrix_1998) %*% bipartite_matrix_1998 #this is correct for the think tank matrix
diag(org_matrix_prod_1998) <- 0

org_matrix_prod_1999 <-  t(bipartite_matrix_1999) %*% bipartite_matrix_1999 
diag(org_matrix_prod_1999) <- 0

org_matrix_prod_2000 <-  t(bipartite_matrix_2000) %*% bipartite_matrix_2000
diag(org_matrix_prod_2000) <- 0

org_matrix_prod_2001 <-  t(bipartite_matrix_2001) %*% bipartite_matrix_2001
diag(org_matrix_prod_2001) <- 0

org_matrix_prod_2002 <-  t(bipartite_matrix_2002) %*% bipartite_matrix_2002
diag(org_matrix_prod_2002) <- 0

org_matrix_prod_2003 <-  t(bipartite_matrix_2003) %*% bipartite_matrix_2003
diag(org_matrix_prod_2003) <- 0

org_matrix_prod_2004 <-  t(bipartite_matrix_2004) %*% bipartite_matrix_2004
diag(org_matrix_prod_2004) <- 0

org_matrix_prod_2005 <-  t(bipartite_matrix_2005) %*% bipartite_matrix_2005
diag(org_matrix_prod_2005) <- 0

org_matrix_prod_2006 <-  t(bipartite_matrix_2006) %*% bipartite_matrix_2006
diag(org_matrix_prod_2006) <- 0

org_matrix_prod_2007 <-  t(bipartite_matrix_2007) %*% bipartite_matrix_2007
diag(org_matrix_prod_2007) <- 0

org_matrix_prod_2008 <-  t(bipartite_matrix_2008) %*% bipartite_matrix_2008
diag(org_matrix_prod_2008) <- 0

org_matrix_prod_2009 <-  t(bipartite_matrix_2009) %*% bipartite_matrix_2009
diag(org_matrix_prod_2009) <- 0

org_matrix_prod_2010 <-  t(bipartite_matrix_2010) %*% bipartite_matrix_2010
diag(org_matrix_prod_2010) <- 0

org_matrix_prod_2011 <-  t(bipartite_matrix_2011) %*% bipartite_matrix_2011
diag(org_matrix_prod_2011) <- 0


# Convert the 1-mode matrix into a matrix object in R
# we are not binarizing, since we are interested in valued ties 
#(i.e. multiple members sitting on the boards of the same two think tanks)
thinktank_overlap_1998 <- as.matrix(org_matrix_prod_1998, mode = "undirected", weighted = TRUE)
#write.csv(thinktank_overlap_1998, file="thinktank_overlap_1998.csv") #saving the one mode matrices for further analyses

thinktank_overlap_1999 <- as.matrix(org_matrix_prod_1999, mode = "undirected", weighted = TRUE)

thinktank_overlap_2000 <- as.matrix(org_matrix_prod_2000, mode = "undirected", weighted = TRUE)

thinktank_overlap_2001 <- as.matrix(org_matrix_prod_2001, mode = "undirected", weighted = TRUE)

thinktank_overlap_2002 <- as.matrix(org_matrix_prod_2002, mode = "undirected", weighted = TRUE)

thinktank_overlap_2003 <- as.matrix(org_matrix_prod_2003, mode = "undirected", weighted = TRUE)

thinktank_overlap_2004 <- as.matrix(org_matrix_prod_2004, mode = "undirected", weighted = TRUE)

thinktank_overlap_2005 <- as.matrix(org_matrix_prod_2005, mode = "undirected", weighted = TRUE)

thinktank_overlap_2006 <- as.matrix(org_matrix_prod_2006, mode = "undirected", weighted = TRUE)

thinktank_overlap_2007 <- as.matrix(org_matrix_prod_2007, mode = "undirected", weighted = TRUE)
#write.csv(thinktank_overlap_2007, file="thinktank_overlap_2007.csv")

thinktank_overlap_2008 <- as.matrix(org_matrix_prod_2008, mode = "undirected", weighted = TRUE)

thinktank_overlap_2009 <- as.matrix(org_matrix_prod_2009, mode = "undirected", weighted = TRUE)

thinktank_overlap_2010 <- as.matrix(org_matrix_prod_2010, mode = "undirected", weighted = TRUE)

thinktank_overlap_2011 <- as.matrix(org_matrix_prod_2011, mode = "undirected", weighted = TRUE)
#write.csv(thinktank_overlap_2011, file="thinktank_overlap_2011.csv")

# Convert 1-mode matrix into a statnet network object 
# here I did different visualizations, you can just choose what you like most.
thinktank_overlap_1998 <- as.network(thinktank_overlap_1998, directed=FALSE)
sum(thinktank_overlap_1998)
sna::degree(thinktank_overlap_1998)
gplot(thinktank_overlap_1998, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "aquamarine4", edge.col = "burlywood4")

thinktank_overlap_1999 <- as.network(thinktank_overlap_1999, directed=FALSE)
sum(thinktank_overlap_1999)
sna::degree(thinktank_overlap_1999)
gplot(thinktank_overlap_1999, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "aquamarine4", edge.col = "burlywood4") +
  title(main = "Think Tank Network (1999)")

thinktank_overlap_2000 <- as.network(thinktank_overlap_2000, directed=FALSE)
sum(thinktank_overlap_2000)
sna::degree(thinktank_overlap_2000)
gplot(thinktank_overlap_2000, usearrows = FALSE, displaylabels = FALSE, 
        displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2000)")

thinktank_overlap_2001 <- as.network(thinktank_overlap_2001, directed=FALSE)
sum(thinktank_overlap_2001)
sna::degree(thinktank_overlap_2001)
gplot(thinktank_overlap_2001, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2001)")

thinktank_overlap_2002 <- as.network(thinktank_overlap_2002, directed=FALSE)
sum(thinktank_overlap_2002)
sna::degree(thinktank_overlap_2002)
gplot(thinktank_overlap_2002, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2002)")

thinktank_overlap_2003 <- as.network(thinktank_overlap_2003, directed=FALSE)
sum(thinktank_overlap_2003)
sna::degree(thinktank_overlap_2003)
gplot(thinktank_overlap_2003, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2003)")

thinktank_overlap_2004 <- as.network(thinktank_overlap_2004, directed=FALSE)
sum(thinktank_overlap_2004)
sna::degree(thinktank_overlap_2004)
gplot(thinktank_overlap_2004, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2004)")

thinktank_overlap_2005 <- as.network(thinktank_overlap_2005, directed=FALSE)
sum(thinktank_overlap_2005)
sna::degree(thinktank_overlap_2005)
gplot(thinktank_overlap_2005, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2005)")

thinktank_overlap_2006 <- as.network(thinktank_overlap_2006, directed=FALSE)
sum(thinktank_overlap_2006)
sna::degree(thinktank_overlap_2006)
gplot(thinktank_overlap_2006, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2006)")

thinktank_overlap_2007 <- as.network(thinktank_overlap_2007, directed=FALSE)
sum(thinktank_overlap_2007)
sna::degree(thinktank_overlap_2007)
gplot(thinktank_overlap_2007, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2007)")

thinktank_overlap_2008 <- as.network(thinktank_overlap_2008, directed=FALSE)
sum(thinktank_overlap_2008)
sna::degree(thinktank_overlap_2008)
gplot(thinktank_overlap_2008, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2008)")

thinktank_overlap_2009 <- as.network(thinktank_overlap_2009, directed=FALSE)
sum(thinktank_overlap_2009)
sna::degree(thinktank_overlap_2009)
gplot(thinktank_overlap_2009, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2009)")

thinktank_overlap_2010 <- as.network(thinktank_overlap_2010, directed=FALSE)
sum(thinktank_overlap_2010)
sna::degree(thinktank_overlap_2010)
gplot(thinktank_overlap_2010, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2010)")


thinktank_overlap_2011 <- as.network(thinktank_overlap_2011, directed=FALSE)
sum(thinktank_overlap_2011)
sna::degree(thinktank_overlap_2011)
#setting vertex attributes: 
#thinktank_overlap_2011 #work in progress here
gplot(thinktank_overlap_2011, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Think Tank Network (2011)")



#################
# Board members #
#################
# I did not work a lot on this part, since the focus was on the think tanks
# Not everything in this part is tested (May 10th 2022)
#creating the one mode matrix
person_matrix_prod_1998 <- bipartite_matrix_1998 %*% t(bipartite_matrix_1998) 
diag(person_matrix_prod_1998) <- 0

person_matrix_prod_1999 <- bipartite_matrix_1999 %*% t(bipartite_matrix_1999) 
diag(person_matrix_prod_1999) <- 0

person_matrix_prod_2000 <- bipartite_matrix_2000 %*% t(bipartite_matrix_2000) 
diag(person_matrix_prod_2000) <- 0

person_matrix_prod_2001 <- bipartite_matrix_2001 %*% t(bipartite_matrix_2001) 
diag(person_matrix_prod_2001) <- 0

person_matrix_prod_2002 <- bipartite_matrix_2002 %*% t(bipartite_matrix_2002) 
diag(person_matrix_prod_2002) <- 0

person_matrix_prod_2003 <- bipartite_matrix_2003 %*% t(bipartite_matrix_2003) 
diag(person_matrix_prod_2003) <- 0

person_matrix_prod_2004 <- bipartite_matrix_2004 %*% t(bipartite_matrix_2004) 
diag(person_matrix_prod_2004) <- 0

person_matrix_prod_2005 <- bipartite_matrix_2005 %*% t(bipartite_matrix_2005) 
diag(person_matrix_prod_2005) <- 0

person_matrix_prod_2006 <- bipartite_matrix_2006 %*% t(bipartite_matrix_2006) 
diag(person_matrix_prod_2006) <- 0

person_matrix_prod_2007 <- bipartite_matrix_2007 %*% t(bipartite_matrix_2007) 
diag(person_matrix_prod_2007) <- 0

person_matrix_prod_2008 <- bipartite_matrix_2008 %*% t(bipartite_matrix_2008) 
diag(person_matrix_prod_2008) <- 0

person_matrix_prod_2009 <- bipartite_matrix_2009 %*% t(bipartite_matrix_2009) 
diag(person_matrix_prod_2009) <- 0

person_matrix_prod_2010 <- bipartite_matrix_2010 %*% t(bipartite_matrix_2010) 
diag(person_matrix_prod_2010) <- 0

person_matrix_prod_2011 <- bipartite_matrix_2011 %*% t(bipartite_matrix_2011) 
diag(person_matrix_prod_2011) <- 0

#also here binarizing is not necessary, since we are interested in valued ties.
person_overlap_1998 <- as.matrix(person_matrix_prod_1998, mode = "undirected", weighted = TRUE)

person_overlap_1999 <- as.matrix(person_matrix_prod_1999, mode = "undirected", weighted = TRUE) 

person_overlap_2000 <- as.matrix(person_matrix_prod_2000, mode = "undirected", weighted = TRUE) 

person_overlap_2001 <- as.matrix(person_matrix_prod_2001, mode = "undirected", weighted = TRUE) 

person_overlap_2002 <- as.matrix(person_matrix_prod_2002, mode = "undirected", weighted = TRUE) 

person_overlap_2003 <- as.matrix(person_matrix_prod_2003, mode = "undirected", weighted = TRUE) 

person_overlap_2004 <- as.matrix(person_matrix_prod_2004, mode = "undirected", weighted = TRUE) 

person_overlap_2005 <- as.matrix(person_matrix_prod_2005, mode = "undirected", weighted = TRUE) 

person_overlap_2006 <- as.matrix(person_matrix_prod_2006, mode = "undirected", weighted = TRUE) 

person_overlap_2007 <- as.matrix(person_matrix_prod_2007, mode = "undirected", weighted = TRUE) 

person_overlap_2008 <- as.matrix(person_matrix_prod_2008, mode = "undirected", weighted = TRUE) 

person_overlap_2009 <- as.matrix(person_matrix_prod_2009, mode = "undirected", weighted = TRUE) 

person_overlap_2010 <- as.matrix(person_matrix_prod_2010, mode = "undirected", weighted = TRUE) 

person_overlap_2011 <- as.matrix(person_matrix_prod_2011, mode = "undirected", weighted = TRUE) 
#person_overlap_2011 <- ifelse(person_overlap_2011 >=1, 1, 0)  # Binarize
#person_overlap_2011 <- as.matrix(person_overlap_2011, mode = "undirected", weighted = FALSE)
#left these in so you have an indication on how to binarize should you want to do so in the future.

# Create a statnet network
person_overlap_1998 <- as.network(person_overlap_1998, directed=FALSE)
gplot(person_overlap_1998, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (1998)")

person_overlap_1999 <- as.network(person_overlap_1999, directed=FALSE)
gplot(person_overlap_1999, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (1999)")

person_overlap_2000 <- as.network(person_overlap_2000, directed=FALSE)
gplot(person_overlap_2000, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2000)")

person_overlap_2001 <- as.network(person_overlap_2001, directed=FALSE)
gplot(person_overlap_2001, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2001)")

person_overlap_2002 <- as.network(person_overlap_2002, directed=FALSE)
gplot(person_overlap_2002, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2002)")

person_overlap_2003 <- as.network(person_overlap_2003, directed=FALSE)
gplot(person_overlap_2003, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2003)")

person_overlap_2004 <- as.network(person_overlap_2004, directed=FALSE)
gplot(person_overlap_2004, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2004)")

person_overlap_2005 <- as.network(person_overlap_2005, directed=FALSE)
gplot(person_overlap_2005, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2005)")

person_overlap_2006 <- as.network(person_overlap_2006, directed=FALSE)
gplot(person_overlap_2006, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2006)")

person_overlap_2007 <- as.network(person_overlap_2007, directed=FALSE)
gplot(person_overlap_2007, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2007)")

person_overlap_2008 <- as.network(person_overlap_2008, directed=FALSE)
gplot(person_overlap_2008, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2008)")

person_overlap_2009 <- as.network(person_overlap_2009, directed=FALSE)
gplot(person_overlap_2009, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2009)")

person_overlap_2010 <- as.network(person_overlap_2010, directed=FALSE)
gplot(person_overlap_2010, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2010)")

person_overlap_2011 <- as.network(person_overlap_2011, directed=FALSE)
gplot(person_overlap_2011, usearrows = FALSE, displaylabels = FALSE, 
      displayisolates = F, vertex.col = "darkgray", vertex.cex = 1.25, edge.col = "black") +
  title(main = "Board Member Network (2011)")

#----Gerber et. al. statistics member networks-----
network.density(person_overlap_1998)
person_deg_1998        <- sna::degree(person_overlap_1998, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
person_clus_1998       <- sna::gtrans(person_overlap_1998) #transitivity/clustering
person_geo_1998        <- sna::geodist(person_overlap_1998, inf.replace = NA,
                                          count.paths = T) #geodesic distance/path length
#the path length between two unconnected nodes is set to infinite, so it was 
#replaced with NA to calculate a meaningful average.
person_comp_1998       <- sna::component.dist(person_overlap_1998, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to:
set.seed(666)
gs <- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(7008, 0.0075824, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_1998 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_1998) #path length
random_mean_clus_1998 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_1998) #clustering

stats_1998 <- c(374, network.density(person_overlap_1998),mean(person_deg_1998),
                max(person_deg_1998), mean(person_clus_1998), mean(person_geo_1998$gdist, na.rm = T), thinktank_comp_1998$csize[1],
                person_comp_1998$csize[1]/374,
                mean(random_mean_paths_1998), mean(random_mean_clus_1998))


#########################
# Think tank attributes #
#########################
# Entering the edgelists
TTattributes1998 <- read_excel("/home/td/random_coding/think tank work/data/1998 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes1998 <- data.frame(TTattributes1998)
#impute missing values for numeric variables
TTattributes1998_imp <- cbind(TTattributes1998$ID, 
             TTattributes1998$ID, 
             TTattributes1998$Corporate_affiliation, 
             TTattributes1998$ID, 
             TTattributes1998$ID,
             TTattributes1998$ID,
             TTattributes1998$ID,
             TTattributes1998$ID)

TTattributes1998_imp <- mice(TTattributes1998_imp)
TTattributes1998_imp <- complete(TTattributes1998_imp)

TTattributes1999 <- read_excel("1999 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes1999 <- data.frame(TTattributes1999)
TTattributes2000 <- read_excel("2000 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2000 <- data.frame(TTattributes2000)
TTattributes2001 <- read_excel("2001 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2001 <- data.frame(TTattributes2001)
TTattributes2002 <- read_excel("2002 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2002 <- data.frame(TTattributes2002)
TTattributes2003 <- read_excel("2003 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2003 <- data.frame(TTattributes2003)
TTattributes2004 <- read_excel("2004 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2004 <- data.frame(TTattributes2004)
TTattributes2005 <- read_excel("2005 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2005 <- data.frame(TTattributes2005)
TTattributes2006 <- read_excel("2006 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2006 <- data.frame(TTattributes2006)
TTattributes2007 <- read_excel("2007 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2007 <- data.frame(TTattributes2007)
TTattributes2008 <- read_excel("2008 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2008 <- data.frame(TTattributes2008)
TTattributes2009 <- read_excel("2009 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2009 <- data.frame(TTattributes2009)
TTattributes2010 <- read_excel("2010 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2010 <- data.frame(TTattributes2010)
TTattributes2011 <- read_excel("2011 think tank attributes.xls",col_names = TRUE,na = "")
TTattributes2011 <- data.frame(TTattributes2011)

####################
# One-Mode Metrics # # Check https://rpubs.com/pjmurphy/546652
####################
# -----Approach 1-----
ID <- thinktank_overlap_1998%v%"vertex.names"
#for 1998 the statistics are full, (i.e. including those of the reporting
#framework of Gerber et. al. (2013), whereas for the other years there are only the
# default statistics)
network.density(thinktank_overlap_1998)
thinktank_deg_1998        <- sna::degree(thinktank_overlap_1998, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_1998  <- sna::degree(thinktank_overlap_1998, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_1998        <- sna::betweenness(thinktank_overlap_1998) # Betweeeness centrality
thinktank_clos_1998       <- sna::closeness(thinktank_overlap_1998, cmode="suminvdir") # Closeness centrality
thinktank_eig_1998        <- sna::evcent(thinktank_overlap_1998) # Eigenvector
thinktank_clus_1998       <- sna::gtrans(thinktank_overlap_1998) #transitivity/clustering
thinktank_geo_1998        <- sna::geodist(thinktank_overlap_1998, inf.replace = NA,
                                          count.paths = T) #geodesic distance/path length
#the path length between two unconnected nodes is set to infinite, so it was 
#replaced with NA to calculate a meaningful average.
thinktank_comp_1998       <- sna::component.dist(thinktank_overlap_1998, 
                                                 connected = "strong") #components
network.density(thinktank_overlap_1998) #density
mean(thinktank_deg_1998)
max(thinktank_deg_1998)
mean(thinktank_degscaled_1998)
mean(thinktank_bet_1998)
mean(thinktank_clos_1998)
mean(thinktank_eig_1998)
mean(thinktank_clus_1998)
mean(thinktank_geo_1998$gdist, na.rm = T)
thinktank_comp_1998$csize[1] #examines component size
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs <- list()
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.0084, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths <- sapply(gs, igraph::mean_distance, 1:100) #path length
random_mean_paths <- mean(random_mean_paths) #path length
random_mean_clus <- sapply(gs, igraph::transitivity, type = "global", 1:100)
random_mean_clus <- mean(random_mean_clus) #clustering

thinktank_deg_1999        <- sna::degree(thinktank_overlap_1999, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_1999  <- sna::degree(thinktank_overlap_1999, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_1999        <- sna::betweenness(thinktank_overlap_1999) # Betweeeness centrality
thinktank_clos_1999       <- sna::closeness(thinktank_overlap_1999, cmode="suminvdir") # Closeness centrality
thinktank_eig_1999       <- sna::evcent(thinktank_overlap_1999) # Eigenvector
mean(thinktank_deg_1999)
mean(thinktank_degscaled_1999)
mean(thinktank_bet_1999)
mean(thinktank_clos_1999)
mean(thinktank_eig_1999)

thinktank_deg_2000       <- sna::degree(thinktank_overlap_2000, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2000  <- sna::degree(thinktank_overlap_2000, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2000        <- sna::betweenness(thinktank_overlap_2000) # Betweeeness centrality
thinktank_clos_2000       <- sna::closeness(thinktank_overlap_2000, cmode="suminvdir") # Closeness centrality
thinktank_eig_2000       <- sna::evcent(thinktank_overlap_2000) # Eigenvector
mean(thinktank_deg_2000)
mean(thinktank_degscaled_2000)
mean(thinktank_bet_2000)
mean(thinktank_clos_2000)
mean(thinktank_eig_2000)

thinktank_deg_2001       <- sna::degree(thinktank_overlap_2001, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2001  <- sna::degree(thinktank_overlap_2001, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2001        <- sna::betweenness(thinktank_overlap_2001) # Betweeeness centrality
thinktank_clos_2001       <- sna::closeness(thinktank_overlap_2001, cmode="suminvdir") # Closeness centrality
thinktank_eig_2001      <- sna::evcent(thinktank_overlap_2001) # Eigenvector
mean(thinktank_deg_2001)
mean(thinktank_degscaled_2001)
mean(thinktank_bet_2001)
mean(thinktank_clos_2001)
mean(thinktank_eig_2001)

thinktank_deg_2002       <- sna::degree(thinktank_overlap_2002, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2002  <- sna::degree(thinktank_overlap_2002, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2002        <- sna::betweenness(thinktank_overlap_2002) # Betweeeness centrality
thinktank_clos_2002       <- sna::closeness(thinktank_overlap_2002, cmode="suminvdir") # Closeness centrality
thinktank_eig_2002       <- sna::evcent(thinktank_overlap_2002) # Eigenvector
mean(thinktank_deg_2002)
mean(thinktank_degscaled_2002)
mean(thinktank_bet_2002)
mean(thinktank_clos_2002)
mean(thinktank_eig_2002)

thinktank_deg_2003       <- sna::degree(thinktank_overlap_2003, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2003  <- sna::degree(thinktank_overlap_2003, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2003        <- sna::betweenness(thinktank_overlap_2003) # Betweeeness centrality
thinktank_clos_2003       <- sna::closeness(thinktank_overlap_2003, cmode="suminvdir") # Closeness centrality
thinktank_eig_2003       <- sna::evcent(thinktank_overlap_2003) # Eigenvector
mean(thinktank_deg_2003)
mean(thinktank_degscaled_2003)
mean(thinktank_bet_2003)
mean(thinktank_clos_2003)
mean(thinktank_eig_2003)

thinktank_deg_2004       <- sna::degree(thinktank_overlap_2004, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2004  <- sna::degree(thinktank_overlap_2004, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2004        <- sna::betweenness(thinktank_overlap_2004) # Betweeeness centrality
thinktank_clos_2004       <- sna::closeness(thinktank_overlap_2004, cmode="suminvdir") # Closeness centrality
thinktank_eig_2004       <- sna::evcent(thinktank_overlap_2004) # Eigenvector
mean(thinktank_deg_2004)
mean(thinktank_degscaled_2004)
mean(thinktank_bet_2004)
mean(thinktank_clos_2004)
mean(thinktank_eig_2004)

thinktank_deg_2005      <- sna::degree(thinktank_overlap_2005, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2005  <- sna::degree(thinktank_overlap_2005, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2005        <- sna::betweenness(thinktank_overlap_2005) # Betweeeness centrality
thinktank_clos_2005       <- sna::closeness(thinktank_overlap_2005, cmode="suminvdir") # Closeness centrality
thinktank_eig_2005       <- sna::evcent(thinktank_overlap_2005) # Eigenvector
mean(thinktank_deg_2005)
mean(thinktank_degscaled_2005)
mean(thinktank_bet_2005)
mean(thinktank_clos_2005)
mean(thinktank_eig_2000)

thinktank_deg_2006       <- sna::degree(thinktank_overlap_2006, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2006  <- sna::degree(thinktank_overlap_2006, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2006        <- sna::betweenness(thinktank_overlap_2006) # Betweeeness centrality
thinktank_clos_2006       <- sna::closeness(thinktank_overlap_2006, cmode="suminvdir") # Closeness centrality
thinktank_eig_2006       <- sna::evcent(thinktank_overlap_2006) # Eigenvector
mean(thinktank_deg_2006)
mean(thinktank_degscaled_2006)
mean(thinktank_bet_2006)
mean(thinktank_clos_2006)
mean(thinktank_eig_2006)

thinktank_deg_2007       <- sna::degree(thinktank_overlap_2007, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2007  <- sna::degree(thinktank_overlap_2007, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2007        <- sna::betweenness(thinktank_overlap_2007) # Betweeeness centrality
thinktank_clos_2007       <- sna::closeness(thinktank_overlap_2007, cmode="suminvdir") # Closeness centrality
thinktank_eig_2007       <- sna::evcent(thinktank_overlap_2007) # Eigenvector
mean(thinktank_deg_2007)
mean(thinktank_degscaled_2007)
mean(thinktank_bet_2007)
mean(thinktank_clos_2007)
mean(thinktank_eig_2007)

thinktank_deg_2008       <- sna::degree(thinktank_overlap_2008, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2008  <- sna::degree(thinktank_overlap_2008, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2008        <- sna::betweenness(thinktank_overlap_2008) # Betweeeness centrality
thinktank_clos_2008       <- sna::closeness(thinktank_overlap_2008, cmode="suminvdir") # Closeness centrality
thinktank_eig_2008       <- sna::evcent(thinktank_overlap_2008) # Eigenvector
mean(thinktank_deg_2008)
mean(thinktank_degscaled_2008)
mean(thinktank_bet_2008)
mean(thinktank_clos_2008)
mean(thinktank_eig_2008)

thinktank_deg_2009       <- sna::degree(thinktank_overlap_2009, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2009  <- sna::degree(thinktank_overlap_2009, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2009        <- sna::betweenness(thinktank_overlap_2009) # Betweeeness centrality
thinktank_clos_2009       <- sna::closeness(thinktank_overlap_2009, cmode="suminvdir") # Closeness centrality
thinktank_eig_2009       <- sna::evcent(thinktank_overlap_2009) # Eigenvector
mean(thinktank_deg_2009)
mean(thinktank_degscaled_2009)
mean(thinktank_bet_2009)
mean(thinktank_clos_2009)
mean(thinktank_eig_2009)

thinktank_deg_2010       <- sna::degree(thinktank_overlap_2010, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2010  <- sna::degree(thinktank_overlap_2010, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2010        <- sna::betweenness(thinktank_overlap_2010) # Betweeeness centrality
thinktank_clos_2010       <- sna::closeness(thinktank_overlap_2010, cmode="suminvdir") # Closeness centrality
thinktank_eig_2010       <- sna::evcent(thinktank_overlap_2010) # Eigenvector
mean(thinktank_deg_2010)
mean(thinktank_degscaled_2010)
mean(thinktank_bet_2010)
mean(thinktank_clos_2010)
mean(thinktank_eig_2010)

thinktank_deg_2011       <- sna::degree(thinktank_overlap_2011, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_2011  <- sna::degree(thinktank_overlap_2011, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_2011        <- sna::betweenness(thinktank_overlap_2011) # Betweeeness centrality
thinktank_clos_2011       <- sna::closeness(thinktank_overlap_2011, cmode="suminvdir") # Closeness centrality
thinktank_eig_2011       <- sna::evcent(thinktank_overlap_2011) # Eigenvector
mean(thinktank_deg_2011)
mean(thinktank_degscaled_2011)
mean(thinktank_bet_2011)
mean(thinktank_clos_2011)
mean(thinktank_eig_2011)

# thinktank_bona_1998  <- sna::bonpow(thinktank_overlap_1998) # Bonacich power
network.density(thinktank_overlap_1998)
thinktank_deg_1998        <- sna::degree(thinktank_overlap_1998, cmode="freeman") 
thinktank_clus_1998       <- sna::gtrans(thinktank_overlap_1998) #transitivity/clustering
thinktank_geo_1998        <- sna::geodist(thinktank_overlap_1998, inf.replace = NA,
                                          count.paths = T)


# Creating a data frame
thinktank_cent_1998 <- round(cbind(thinktank_deg_1998, thinktank_degscaled_1998, thinktank_bet_1998, thinktank_clos_1998, thinktank_eig_1998),2)
thinktank_cent_df_1998 <- data.frame(ID, thinktank_deg_1998, thinktank_degscaled_1998, thinktank_bet_1998, thinktank_clos_1998, thinktank_eig_1998)
# Save it to your computer as a spreadsheet
write.csv(thinktank_cent_df_1998, file="centralities_1998.csv")

#----Gerber et. al. statistics think tank networks----
#1998
network.density(thinktank_overlap_1998)
thinktank_deg_1998        <- sna::degree(thinktank_overlap_1998, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_1998  <- sna::degree(thinktank_overlap_1998, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_1998        <- sna::betweenness(thinktank_overlap_1998) # Betweeeness centrality
thinktank_clos_1998       <- sna::closeness(thinktank_overlap_1998, cmode="suminvdir") # Closeness centrality
thinktank_eig_1998        <- sna::evcent(thinktank_overlap_1998) # Eigenvector
thinktank_clus_1998       <- sna::gtrans(thinktank_overlap_1998) #transitivity/clustering
thinktank_geo_1998        <- sna::geodist(thinktank_overlap_1998, inf.replace = NA,
                                          count.paths = T) #geodesic distance/path length
#the path length between two unconnected nodes is set to infinite, so it was 
#replaced with NA to calculate a meaningful average.
thinktank_comp_1998       <- sna::component.dist(thinktank_overlap_1998, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs <- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.0084, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_1998 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_1998) #path length
random_mean_clus_1998 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_1998) #clustering

stats_1998 <- c(374, network.density(thinktank_overlap_1998),mean(thinktank_deg_1998),
                max(thinktank_deg_1998), mean(thinktank_clus_1998), mean(thinktank_geo_1998$gdist, na.rm = T), thinktank_comp_1998$csize[1],
                thinktank_comp_1998$csize[1]/374,
                mean(random_mean_paths_1998), mean(random_mean_clus_1998))

#1999 (note that the input for the erdos reyi random models needs to be modified for
# each year, based on the density of the network)
network.density(thinktank_overlap_1999)
thinktank_deg_1999        <- sna::degree(thinktank_overlap_1999, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_degscaled_1999  <- sna::degree(thinktank_overlap_1999, cmode="freeman", rescale=TRUE) # Freeman degree centrality with scaling 
thinktank_bet_1999        <- sna::betweenness(thinktank_overlap_1999) # Betweeeness centrality
thinktank_clos_1999       <- sna::closeness(thinktank_overlap_1999, cmode="suminvdir") # Closeness centrality
thinktank_eig_1999        <- sna::evcent(thinktank_overlap_1999) # Eigenvector
thinktank_clus_1999       <- sna::gtrans(thinktank_overlap_1999) #transitivity/clustering
thinktank_geo_1999        <- sna::geodist(thinktank_overlap_1999, inf.replace = NA,
                                          count.paths = T) #geodesic distance/path length
#the path length between two unconnected nodes is set to infinite, so it was 
#replaced with NA to calculate a meaningful average.
thinktank_comp_1999       <- sna::component.dist(thinktank_overlap_1999, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.008989, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_1999 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_1999) #path length
random_mean_clus_1999 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_1999) #clustering

stats_1999 <- c(374, network.density(thinktank_overlap_1999),mean(thinktank_deg_1999),
                max(thinktank_deg_1999), mean(thinktank_clus_1999), mean(thinktank_geo_1999$gdist, na.rm = T), 
                thinktank_comp_1999$csize[1],thinktank_comp_1999$csize[1]/374,
                mean(random_mean_paths_1999), mean(random_mean_clus_1999))

#2000
network.density(thinktank_overlap_2000)
thinktank_deg_2000        <- sna::degree(thinktank_overlap_2000, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2000       <- sna::gtrans(thinktank_overlap_2000) #transitivity/clustering
thinktank_geo_2000      <- sna::geodist(thinktank_overlap_2000, inf.replace = NA,
                                          count.paths = T) #geodesic distance/path length
#the path length between two unconnected nodes is set to infinite, so it was 
#replaced with NA to calculate a meaningful average.
thinktank_comp_2000       <- sna::component.dist(thinktank_overlap_2000, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.00971, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2000 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2000) #path length
random_mean_clus_2000 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2000) #clustering

stats_2000 <- c(374, network.density(thinktank_overlap_2000),mean(thinktank_deg_2000),
                max(thinktank_deg_2000), mean(thinktank_clus_2000), mean(thinktank_geo_2000$gdist, na.rm = T), 
                thinktank_comp_2000$csize[1],thinktank_comp_2000$csize[1]/374,
                mean(random_mean_paths_2000), mean(random_mean_clus_2000))

#2001
network.density(thinktank_overlap_2001)
thinktank_deg_2001        <- sna::degree(thinktank_overlap_2001, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2001       <- sna::gtrans(thinktank_overlap_2001) #transitivity/clustering
thinktank_geo_2001      <- sna::geodist(thinktank_overlap_2001, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2001       <- sna::component.dist(thinktank_overlap_2001, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.008831, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2001 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2001) #path length
random_mean_clus_2001 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2001) #clustering

stats_2001 <- c(374, network.density(thinktank_overlap_2001),mean(thinktank_deg_2001),
                max(thinktank_deg_2001), mean(thinktank_clus_2001), mean(thinktank_geo_2001$gdist, na.rm = T), 
                thinktank_comp_2001$csize[1],thinktank_comp_2001$csize[1]/374,
                mean(random_mean_paths_2001), mean(random_mean_clus_2001))

#2002
network.density(thinktank_overlap_2002)
thinktank_deg_2002        <- sna::degree(thinktank_overlap_2002, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2002       <- sna::gtrans(thinktank_overlap_2002) #transitivity/clustering
thinktank_geo_2002      <- sna::geodist(thinktank_overlap_2002, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2002       <- sna::component.dist(thinktank_overlap_2002, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.01022, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2002 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2002) #path length
random_mean_clus_2002 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2002) #clustering

stats_2002 <- c(374, network.density(thinktank_overlap_2002),mean(thinktank_deg_2002),
                 max(thinktank_deg_2002), mean(thinktank_clus_2002), mean(thinktank_geo_2002$gdist, na.rm = T), 
                 thinktank_comp_2002$csize[1],thinktank_comp_2002$csize[1]/374,
                 mean(random_mean_paths_2002), mean(random_mean_clus_2002))

#2003
network.density(thinktank_overlap_2003)
thinktank_deg_2003        <- sna::degree(thinktank_overlap_2003, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2003       <- sna::gtrans(thinktank_overlap_2003) #transitivity/clustering
thinktank_geo_2003      <- sna::geodist(thinktank_overlap_2003, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2003       <- sna::component.dist(thinktank_overlap_2003, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.01035, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2003 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2003) #path length
random_mean_clus_2003 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2003) #clustering

stats_2003 <- c(374, network.density(thinktank_overlap_2003),mean(thinktank_deg_2003),
                max(thinktank_deg_2003), mean(thinktank_clus_2003), mean(thinktank_geo_2003$gdist, na.rm = T), 
                thinktank_comp_2003$csize[1],thinktank_comp_2003$csize[1]/374,
                mean(random_mean_paths_2003), mean(random_mean_clus_2003))

#2004
network.density(thinktank_overlap_2004)
thinktank_deg_2004        <- sna::degree(thinktank_overlap_2004, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2004       <- sna::gtrans(thinktank_overlap_2004) #transitivity/clustering
thinktank_geo_2004      <- sna::geodist(thinktank_overlap_2004, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2004       <- sna::component.dist(thinktank_overlap_2004, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.0085733, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2004 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2004) #path length
random_mean_clus_2004 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2004) #clustering

stats_2004 <- c(374, network.density(thinktank_overlap_2004),mean(thinktank_deg_2004),
                max(thinktank_deg_2004), mean(thinktank_clus_2004), mean(thinktank_geo_2004$gdist, na.rm = T), 
                thinktank_comp_2004$csize[1],thinktank_comp_2004$csize[1]/374,
                mean(random_mean_paths_2004), mean(random_mean_clus_2004))

#2005
network.density(thinktank_overlap_2005)
thinktank_deg_2005       <- sna::degree(thinktank_overlap_2005, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2005       <- sna::gtrans(thinktank_overlap_2005) #transitivity/clustering
thinktank_geo_2005      <- sna::geodist(thinktank_overlap_2005, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2005       <- sna::component.dist(thinktank_overlap_2005, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.008917, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2005 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2005) #path length
random_mean_clus_2005 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2005) #clustering

stats_2005 <- c(374, network.density(thinktank_overlap_2005),mean(thinktank_deg_2005),
                max(thinktank_deg_2005), mean(thinktank_clus_2005), mean(thinktank_geo_2005$gdist, na.rm = T), 
                thinktank_comp_2005$csize[1],thinktank_comp_2005$csize[1]/374,
                mean(random_mean_paths_2005), mean(random_mean_clus_2005))

#2006
network.density(thinktank_overlap_2006)
thinktank_deg_2006        <- sna::degree(thinktank_overlap_2006, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2006       <- sna::gtrans(thinktank_overlap_2006) #transitivity/clustering
thinktank_geo_2006      <- sna::geodist(thinktank_overlap_2006, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2006       <- sna::component.dist(thinktank_overlap_2006, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.01189, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2006 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2006) #path length
random_mean_clus_2006 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2006) #clustering

stats_2006 <- c(374, network.density(thinktank_overlap_2006),mean(thinktank_deg_2006),
                max(thinktank_deg_2006), mean(thinktank_clus_2006), mean(thinktank_geo_2006$gdist, na.rm = T), 
                thinktank_comp_2006$csize[1],thinktank_comp_2006$csize[1]/374,
                mean(random_mean_paths_2006), mean(random_mean_clus_2006))

#2007
network.density(thinktank_overlap_2007)
thinktank_deg_2007        <- sna::degree(thinktank_overlap_2007, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2007       <- sna::gtrans(thinktank_overlap_2007) #transitivity/clustering
thinktank_geo_2007      <- sna::geodist(thinktank_overlap_2007, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2007       <- sna::component.dist(thinktank_overlap_2007, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.009204, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2007 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2007) #path length
random_mean_clus_2007 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2007) #clustering

stats_2007 <- c(374, network.density(thinktank_overlap_2007),mean(thinktank_deg_2007),
                max(thinktank_deg_2007), mean(thinktank_clus_2007), mean(thinktank_geo_2007$gdist, na.rm = T), 
                thinktank_comp_2007$csize[1],thinktank_comp_2007$csize[1]/374,
                mean(random_mean_paths_2007), mean(random_mean_clus_2007))

#2008
network.density(thinktank_overlap_2008)
thinktank_deg_2008        <- sna::degree(thinktank_overlap_2008, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2008       <- sna::gtrans(thinktank_overlap_2008) #transitivity/clustering
thinktank_geo_2008      <- sna::geodist(thinktank_overlap_2008, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2008       <- sna::component.dist(thinktank_overlap_2008, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.0086162, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2008 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2008) #path length
random_mean_clus_2008 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2008) #clustering

stats_2008 <- c(374, network.density(thinktank_overlap_2008),mean(thinktank_deg_2008),
                max(thinktank_deg_2008), mean(thinktank_clus_2008), mean(thinktank_geo_2008$gdist, na.rm = T), 
                thinktank_comp_2008$csize[1],thinktank_comp_2008$csize[1]/374,
                mean(random_mean_paths_2008), mean(random_mean_clus_2008))

#2009
network.density(thinktank_overlap_2009)
thinktank_deg_2009        <- sna::degree(thinktank_overlap_2009, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2009       <- sna::gtrans(thinktank_overlap_2009) #transitivity/clustering
thinktank_geo_2009      <- sna::geodist(thinktank_overlap_2009, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2009       <- sna::component.dist(thinktank_overlap_2009, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.007655, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2009 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2009) #path length
random_mean_clus_2009 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2009) #clustering

stats_2009 <- c(374, network.density(thinktank_overlap_2009),mean(thinktank_deg_2009),
                max(thinktank_deg_2009), mean(thinktank_clus_2009), mean(thinktank_geo_2009$gdist, na.rm = T), 
                thinktank_comp_2009$csize[1],thinktank_comp_2009$csize[1]/374,
                mean(random_mean_paths_2009), mean(random_mean_clus_2009))

#2010
network.density(thinktank_overlap_2010)
thinktank_deg_2010        <- sna::degree(thinktank_overlap_2010, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2010       <- sna::gtrans(thinktank_overlap_2010) #transitivity/clustering
thinktank_geo_2010      <- sna::geodist(thinktank_overlap_2010, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2010       <- sna::component.dist(thinktank_overlap_2010, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.0068816, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2010 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2010) #path length
random_mean_clus_2010 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2010) #clustering

stats_2010 <- c(374, network.density(thinktank_overlap_2010),mean(thinktank_deg_2010),
                max(thinktank_deg_2010), mean(thinktank_clus_2010), mean(thinktank_geo_2010$gdist, na.rm = T), 
                thinktank_comp_2010$csize[1],thinktank_comp_2010$csize[1]/374,
                mean(random_mean_paths_2010), mean(random_mean_clus_2010))

#2011
network.density(thinktank_overlap_2011)
thinktank_deg_2011        <- sna::degree(thinktank_overlap_2011, cmode="freeman") # Freeman degree centrality without scaling i.e. raw count
thinktank_clus_2011       <- sna::gtrans(thinktank_overlap_2011) #transitivity/clustering
thinktank_geo_2011      <- sna::geodist(thinktank_overlap_2011, inf.replace = NA,
                                        count.paths = T) #mean path length
thinktank_comp_2011       <- sna::component.dist(thinktank_overlap_2011, 
                                                 connected = "strong") #components
#creating 100 Erdos Renyi random graphs to compare to: (all years can be compared to these)
set.seed(666)
gs<- list()
library(igraph)
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(374, 0.007182, type = "gnp", directed = F)
  E(gs[[x]])$weight <- sample(1:5, ecount(gs[[x]]), T)
} #100 random models
random_mean_paths_2011 <- sapply(gs, igraph::mean_distance, 1:100) #path length
mean(random_mean_paths_2011) #path length
random_mean_clus_2011 <- sapply(gs, igraph::transitivity, type = "global", 1:100)
mean(random_mean_clus_2011) #clustering

stats_2011 <- c(374, network.density(thinktank_overlap_2011),mean(thinktank_deg_2011),
                max(thinktank_deg_2011), mean(thinktank_clus_2011), mean(thinktank_geo_2011$gdist, na.rm = T), 
                thinktank_comp_2011$csize[1],thinktank_comp_2011$csize[1]/374,
                mean(random_mean_paths_2011), mean(random_mean_clus_2011))

#saving the file
network_descriptives <- data.frame(stats_1998, stats_1999, stats_2000, stats_2001, 
                                   stats_2002, stats_2003, stats_2004, stats_2005,
                                   stats_2006, stats_2007, stats_2008, stats_2009, 
                                   stats_2010, stats_2011)
colnames(network_descriptives) <- c(1998:2011)
rownames(network_descriptives) <- c("Network Size", "Density", "Average Degree Centrality",
                                    "Maximum Degree Centrality", "Average Clustering", 
                                    "Average Path Length", "Size of the Largest Component", 
                                    "Proportion of the Largest Component", "Expected Path Length", 
                                    "Expected Clustering")
network_descriptives <- round(network_descriptives, digits = 3)
write.csv(network_descriptives, file = "network_descriptives-gerber_framework.csv")

# Approach 2
thinktank_overlap_1998%v% "degree"   <- sna::degree(thinktank_overlap_1998, cmode="freeman")
thinktank_overlap_1998%v% "degscaled"<- sna::degree(thinktank_overlap_1998, cmode="freeman", rescale=TRUE)
thinktank_overlap_1998%v% "bet"      <- sna::betweenness(thinktank_overlap_1998)
thinktank_overlap_1998%v% "clos"     <- sna::closeness(thinktank_overlap_1998, cmode="suminvdir") # Closeness centrality
thinktank_overlap_1998%v% "eig"      <- sna::evcent(thinktank_overlap_1998) # Eigenvector

# Creating a data frame
thinktank_cent_df_1998_2 <- data.frame(row.names   = thinktank_overlap_1998 %v% "vertex.names",
                         degree      = thinktank_overlap_1998 %v% "degree",     
                         degree_scal = thinktank_overlap_1998 %v% "degscaled",
                         closeness   = thinktank_overlap_1998 %v% "clos",        
                         betweenness = thinktank_overlap_1998 %v% "bet",  
                         eigenvector = thinktank_overlap_1998 %v% "eig")

# Order the data by row names i.e. think tank IDs
thinktank_cent_df_1998_2 <- thinktank_cent_df_1998_2[order(row.names(thinktank_cent_df_1998_2)),]
head(thinktank_cent_df_1998_2)

# Order the data by degree centrality
#thinktank_cent_df_1998_2 <- thinktank_cent_df_1998_2[order(thinktank_cent_df_1998_2$degree, decreasing = TRUE),]
#head(thinktank_cent_df_1998_2)

# Lowest tie value
# Minimum value
min(thinktank_cent_df_1998_2$betweenness)
# Maximum value
max(thinktank_cent_df_1998_2$betweenness)
# Bottom six 
head(thinktank_cent_df_1998_2[order(-thinktank_cent_df_1998_2$betweenness),])
# Bottom five
head(thinktank_cent_df_1998_2[order(-thinktank_cent_df_1998_2$betweenness),], n=5)

# Highest tie value
max(degree(thinktank_overlap_1998))
# Top six
head(thinktank_cent_df_1998_2[order(thinktank_cent_df_1998_2$betweenness),])
# Top five
head(thinktank_cent_df_1998_2[order(thinktank_cent_df_1998_2$betweenness),], n=5)


# Save it to your computer as a spreadsheet
write.csv(thinktank_cent_df_1998_2, file="centralities_1998_2.csv")

#########################