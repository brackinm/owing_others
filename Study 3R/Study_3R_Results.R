# Borrowing is influenced by moral emotions
# REPLICATION

# Maria Brackin & Hugo Mercier
# RESULTS


# DATA DESCRIPTION
# Key variables:
## vignette - scenario presented to participant, varies by cost
## imposed by potential lender:
### 1 = moving boxes
### 2 = tedious paperwork
### 3 = building furniture

## order - counterbalancing of names
### A = John, then Peter
### B = Peter, then John

## emotions - which lender elicits the stronger of a specific 
## emotion, weak preference (somewhat) or strong preference (much)
### gratitude 
### guilt
### indebtedness

## choice - which of the two lenders is chosen for the loan
### John (no conditions)
### Peter (conditions)


# Preliminaries
# Install required packages
library(tidyverse)
library(binom)



# STEP 1: Import Data
# Set working directory to import data
setwd("FILEPATH")

# Import csv data file
final.data <- read.csv("Study_3_cleandata.csv")




# STEP 2: Plots

# OVERALL RESULTS
plot.data <- final.data %>% 
  select(ID, gratitude, guilt, indebtedness) %>%
  gather(emotion, response, "gratitude":"indebtedness")
plot.data$response <- factor(plot.data$response, levels = c("Much John", "Somewhat John", "Somewhat Peter", "Much Peter"))

overall <- ggplot(plot.data, aes(fill=response, y=..count.., x=emotion)) + 
  geom_bar(position="stack") + 
  scale_fill_grey() + 
  theme_minimal()
overall




# STEP 3: Analysis

# assign choices: "John" if somewhat or much more John, "Peter" if
# somewhat or much more Peter
group.data <- final.data %>% 
mutate(gra.group = ifelse(gratitude=="Much John"|gratitude=="Somewhat John", "John", "Peter")) %>%
  mutate(gui.group = ifelse(guilt=="Much John"|guilt=="Somewhat John", "John", "Peter")) %>%
  mutate(ind.group = ifelse(indebtedness=="Much John"|indebtedness=="Somewhat John", "John", "Peter"))

# total number of participants
nrow(group.data) #150


### GUILT ###
# H1: More participants expect to feel more guilty if 
# no extra cost is required for the loan.

# Number of participants choosing "John"
length(group.data$gui.group[group.data$gui.group=="John"]) #107

# 107 of 150 participants chose "John"
binom.test(107,150,0.5, alternative = "greater")



### INDEBTEDNESS ###
# H2: More participants expect to feel more indebted if 
# no extra cost is required for the loan.

# Number of participants choosing "John"
length(group.data$ind.group[group.data$ind.group=="John"]) #103

# 103 of 150 participants chose "John"
binom.test(103,150,0.5, alternative = "greater")



### GRATITUDE ###
# H3: More participants expect to feel more grateful if 
# no extra cost is required for the loan.

# Number of participants choosing "John"
length(group.data$gra.group[group.data$gra.group=="John"]) #136

# 136 of 150 participants chose "John"
binom.test(136,150,0.5, alternative = "greater")



### LENDER CHOICE ###
# H4: More than zero participants will choose the lender 
# who imposes an extra cost for the loan.

# Number of participants choosing "Peter"
length(final.data$choice[final.data$choice=="Peter (conditions)"]) #44

# 44 of 150 participants chose "Peter"
binom.test(44,150,0, alternative = "greater")


