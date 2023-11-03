# Emotional responses to debt are shaped by cooperative cognition
# Maria Brackin & Hugo Mercier
# RESULTS

# DATA DESCRIPTION
# Key variables:
## perspective - borrower or lender perspective

## study - 1a (benefits to borrower) or 1b (costs to lender)

## vignette - scenario for binary choice (high or low)
### 1 (1a) = sunglasses vs prescription glasses
### 2 (1a) = cooler vs refrigerator
### 3 (1a) = spring break vs grandma
### 1 (1b) = bonus vs concert
### 2 (1b) = inheritance vs fancy restaurant
### 3 (1b) = lottery vs weekend away



# Preliminaries
# Install required packages
library(tidyverse)
library(binom)



# STEP 1: Import Data
# Set working directory to import data
setwd("FILEPATH")

# Import csv data file
final.data <- read.csv("Study_1_cleandata.csv")




# STEP 2: Recode responses

### Study 1a: benefits to borrower ###

# Compute individual participants' overall score (0-3) 
# for each emotion

final.data <- final.data %>% group_by(ID) %>% mutate(gr.sum = sum(gratitude == "high")) 
final.data <- final.data %>% group_by(ID) %>% mutate(i.sum = sum(indebtedness == "high"))
final.data <- final.data %>% group_by(ID) %>% mutate(gu.sum = sum(guilt == "high"))
final.data <- final.data %>% group_by(ID) %>% mutate(sh.sum = sum(shame == "high"))
final.data <- final.data %>% group_by(ID) %>% mutate(a.sum = sum(anger == "high"))
final.data <- final.data %>% group_by(ID) %>% mutate(r.sum = sum(repayment == "high"))

final.data <- final.data %>% 
  mutate(gratitude = ifelse(gr.sum >= 2, "high", "low")) %>%
  mutate(indebtedness = ifelse(i.sum >= 2, "high", "low")) %>%
  mutate(guilt = ifelse(gu.sum >= 2, "high", "low")) %>%
  mutate(anger = ifelse(a.sum >= 2, "high", "low")) %>%
  mutate(shame = ifelse(sh.sum >= 2, "high", "low")) %>%
  mutate(repayment = ifelse(r.sum >= 2, "high", "low")) %>%
  select(study, perspective, ID, gratitude, indebtedness, guilt, anger, shame, repayment) %>%
  distinct()



# STEP 3: Plot results

## count responses for gratitude, indebtedness, guilt, anger, shame, 
## and repayment
count.emotions <- final.data %>% gather(emotion, response, "gratitude":"repayment") %>%
  filter(!is.na(response)) %>%
  mutate(study = case_when(study=="1a" ~ "Study 1a (benefits to borrower)",
                           study=="1b" ~ "Study 1b (costs to lender)"))

count.emotions <- count.emotions %>% group_by(study, emotion, response) %>% tally()

## convert to percent and order according to hypotheses
count.emotions <- count.emotions %>% 
  group_by(emotion, study) %>% mutate(percent = n/sum(n)*100) %>%
  filter(!is.na(response))
count.emotions$emotion <- factor(count.emotions$emotion, levels = c("guilt", "shame", "anger", "gratitude", "indebtedness", "repayment"))

## plot  
emotions <- ggplot(count.emotions, aes(fill=response, y=percent, x=emotion)) + 
  geom_bar(stat = "identity", position="stack") + 
  facet_wrap(vars(study)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  xlab("") +
  scale_fill_grey() + 
  theme_minimal()
emotions




# STEP 4: Statistical Analysis


## OVERALL RESPONSES ## 
# number of responses in Study 1a:
## overall
## borrowers
## lenders
length(final.data$ID[final.data$study=="1a"]) # 106
length(final.data$ID[final.data$study=="1a"&final.data$perspective=="borrower"]) # 53
length(final.data$ID[final.data$study=="1a"&final.data$perspective=="lender"]) # 53

# number of responses in Study 1b:
## overall
## borrowers
## lenders
length(final.data$ID[final.data$study=="1b"]) # 105
length(final.data$ID[final.data$study=="1b"&final.data$perspective=="borrower"]) # 53
length(final.data$ID[final.data$study=="1b"&final.data$perspective=="lender"]) # 52



## GUILT ##

#STUDY 1A
# H1a: More participants expect the borrower to feel guilty when 
# the benefits to the borrower are low.

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$guilt[final.data$study=="1a"&final.data$guilt=="low"]) # 87
length(final.data$guilt[final.data$study=="1a"&final.data$guilt=="low"&final.data$perspective=="borrower"]) # 49
length(final.data$guilt[final.data$study=="1a"&final.data$guilt=="low"&final.data$perspective=="lender"]) # 38

# Binomial test: high vs low responses
## 103 of 106 responses were "low"
binom.test(103,106,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 49 of 53 borrowers are "low"
binom.test(49,53,0.5, alternative = "greater") 

## 38 of 53 lenders are "low"
binom.test(38,53,0.5, alternative = "greater") 


# STUDY 1B
# H1b: More participants expect the borrower to feel guilty when 
# the costs to the lender are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$guilt[final.data$study=="1b"&final.data$guilt=="high"]) # 94
length(final.data$guilt[final.data$study=="1b"&final.data$guilt=="high"&final.data$perspective=="borrower"]) # 46
length(final.data$guilt[final.data$study=="1b"&final.data$guilt=="high"&final.data$perspective=="lender"]) # 48

# Binomial test: high vs low responses
## 64 of 105 responses were "high"
binom.test(94,105,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 35 of 53 borrowers are "high"
binom.test(46,53,0.5, alternative = "greater") 

## 29 of 52 lenders are "high"
binom.test(48,52,0.5, alternative = "greater") 



## SHAME ##

# STUDY 1A
# H2a: More participants expect the borrower to feel ashamed when 
# the benefits to the borrower are low.

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$shame[final.data$study=="1a"&final.data$shame=="low"]) # 87
length(final.data$shame[final.data$study=="1a"&final.data$shame=="low"&final.data$perspective=="borrower"]) # 49
length(final.data$shame[final.data$study=="1a"&final.data$shame=="low"&final.data$perspective=="lender"]) # 38


# Binomial test: high vs low responses
## 87 of 106 responses were "low"
binom.test(87,106,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 49 of 53 borrowers are "low"
binom.test(49,53,0.5, alternative = "greater") 

## 38 of 53 lenders are "low"
binom.test(38,53,0.5, alternative = "greater") 


# STUDY 1B
# H2b: More participants expect the borrower to feel ashamed when 
# the costs to the lender are high. 

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$shame[final.data$study=="1b"&final.data$shame=="high"]) # 93
length(final.data$shame[final.data$study=="1b"&final.data$shame=="high"&final.data$perspective=="borrower"]) # 46
length(final.data$shame[final.data$study=="1b"&final.data$shame=="high"&final.data$perspective=="lender"]) # 47

# Binomial test: high vs low responses
## 93 of 105 responses were "high"
binom.test(93,105,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 46 of 53 borrowers are "high"
binom.test(46,53,0.5, alternative = "greater") 

## 47 of 52 lenders are "high"
binom.test(47,52,0.5, alternative = "greater") 



## ANGER ##

# STUDY 1A
# H3a: More participants expect the lender to feel angry when 
# the benefits to the borrower are low.

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$anger[final.data$study=="1a"&final.data$anger=="low"]) # 96
length(final.data$anger[final.data$study=="1a"&final.data$anger=="low"&final.data$perspective=="borrower"]) # 47
length(final.data$anger[final.data$study=="1a"&final.data$anger=="low"&final.data$perspective=="lender"]) # 49


# Binomial test: high vs low responses
## 96 of 106 responses were "low"
binom.test(96,106,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 47 of 53 borrowers are "low"
binom.test(47,53,0.5, alternative = "greater") 

## 49 of 53 lenders are "low"
binom.test(49,53,0.5, alternative = "greater") 


# STUDY 1B
# H3b: More participants expect the lender to feel angry when 
# the costs to the lender are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$anger[final.data$study=="1b"&final.data$anger=="high"]) # 89
length(final.data$anger[final.data$study=="1b"&final.data$anger=="high"&final.data$perspective=="borrower"]) # 46
length(final.data$anger[final.data$study=="1b"&final.data$anger=="high"&final.data$perspective=="lender"]) # 43

# Binomial test: high vs low responses
## 89 of 105 responses were "high"
binom.test(89,105,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 46 of 53 borrowers are "high"
binom.test(46,53,0.5, alternative = "greater") 

## 43 of 52 lenders are "high"
binom.test(43,52,0.5, alternative = "greater") 



## GRATITUDE ##

# STUDY 1A
# H4a: More participants expect the borrower to feel grateful when 
# the benefits to the borrower are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$gratitude[final.data$study=="1a"&final.data$gratitude=="high"]) # 103
length(final.data$gratitude[final.data$study=="1a"&final.data$gratitude=="high"&final.data$perspective=="borrower"]) # 51
length(final.data$gratitude[final.data$study=="1a"&final.data$gratitude=="high"&final.data$perspective=="lender"]) # 52


# Binomial test: high vs low responses
## 103 of 106 responses were "high"
binom.test(103,106,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 51 of 53 borrowers are "high"
binom.test(51,53,0.5, alternative = "greater") 

## 52 of 53 lenders are "high"
binom.test(52,53,0.5, alternative = "greater") 


# STUDY 1B
# H4b: More participants expect the borrower to feel grateful when 
# the cost to the lender is high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$gratitude[final.data$study=="1b"&final.data$gratitude=="high"]) # 64
length(final.data$gratitude[final.data$study=="1b"&final.data$gratitude=="high"&final.data$perspective=="borrower"]) # 35
length(final.data$gratitude[final.data$study=="1b"&final.data$gratitude=="high"&final.data$perspective=="lender"]) # 29

# Binomial test: high vs low responses
## 64 of 105 responses were "high"
binom.test(64,105,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 35 of 53 borrowers are "high"
binom.test(35,53,0.5, alternative = "greater") 

## 29 of 52 lenders are "high"
binom.test(29,52,0.5, alternative = "greater") 




## INDEBTEDNESS ##

# STUDY 1A
# RQ1: Do more participants expect the borrower to feel indebted when 
# the benefits to the borrower are low?

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$indebtedness[final.data$study=="1a"&final.data$indebtedness=="low"]) # 40
length(final.data$indebtedness[final.data$study=="1a"&final.data$indebtedness=="low"&final.data$perspective=="borrower"]) # 21
length(final.data$indebtedness[final.data$study=="1a"&final.data$indebtedness=="low"&final.data$perspective=="lender"]) # 19


# Binomial test: high vs low responses
## 40 of 106 responses were "low"
binom.test(40,106,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 21 of 53 borrowers are "low"
binom.test(21,53,0.5, alternative = "greater") 

## 19 of 53 lenders are "low"
binom.test(19,53,0.5, alternative = "greater") 


# STUDY 1B
# H5b: More participants expect the borrower to feel indebted when 
# the costs to the lender are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$indebtedness[final.data$study=="1b"&final.data$indebtedness=="high"]) # 96
length(final.data$indebtedness[final.data$study=="1b"&final.data$indebtedness=="high"&final.data$perspective=="borrower"]) # 48
length(final.data$indebtedness[final.data$study=="1b"&final.data$indebtedness=="high"&final.data$perspective=="lender"]) # 48

# Binomial test: high vs low responses
## 96 of 105 responses were "high"
binom.test(96,105,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 48 of 53 borrowers are "high"
binom.test(48,53,0.5, alternative = "greater") 

## 48 of 52 lenders are "high"
binom.test(48,52,0.5, alternative = "greater") 



## REPAYMENT ##

# STUDY 1A
# RQ2: Do more participants (borrowers) prefer to repay first when 
# the benefits to them are low?

# number of participants in study 1a who answered repayment questions
# all in borrower perspective
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1a"]) # 53

# number of participants who answered "low"
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1a"&final.data$repayment=="low"]) # 22

# Binomial test: high vs low responses
## 22 of 53 responses were "low"
binom.test(22,53,0.5,alternative = "greater") 


# STUDY 1B
# H6b: More participants will want to repay the lender when the 
# costs to the lender are high.

# number of participants in study 1b who answered repayment questions
# all in borrower perspective
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1b"]) # 53

# number of participants who answered "high"
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1b"&final.data$repayment=="high"]) # 50

# Binomial test: high vs low responses
## 50 of 53 responses were "high"
binom.test(50,53,0.5,alternative = "greater") 

