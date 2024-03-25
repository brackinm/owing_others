# Emotional responses to debt are shaped by cooperative cognition
# Replication 2

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
data <- read.csv("Study_1R2_cleandata.csv")




# STEP 2: Recode responses

### Study 1a: benefits to borrower ###

# Compute individual participants' overall score (0-3) 
# for each emotion

data <- data %>% group_by(ID) %>% mutate(gr.sum = sum(gratitude == "high")) 
data <- data %>% group_by(ID) %>% mutate(i.sum = sum(indebtedness == "high"))
data <- data %>% group_by(ID) %>% mutate(gu.sum = sum(guilt == "high"))
data <- data %>% group_by(ID) %>% mutate(sh.sum = sum(shame == "high"))
data <- data %>% group_by(ID) %>% mutate(a.sum = sum(anger == "high"))
data <- data %>% group_by(ID) %>% mutate(r.sum = sum(repayment == "high"))

final.data <- data %>% 
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
length(final.data$ID[final.data$study=="1a"]) # 135
length(final.data$ID[final.data$study=="1a"&final.data$perspective=="borrower"]) # 67
length(final.data$ID[final.data$study=="1a"&final.data$perspective=="lender"]) # 68

# number of responses in Study 1b:
## overall
## borrowers
## lenders
length(final.data$ID[final.data$study=="1b"]) # 133
length(final.data$ID[final.data$study=="1b"&final.data$perspective=="borrower"]) # 66
length(final.data$ID[final.data$study=="1b"&final.data$perspective=="lender"]) # 67



## GUILT ##

#STUDY 1A
# H1a: More participants expect the borrower to feel guilty when 
# the benefits to the borrower are low.

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$guilt[final.data$study=="1a"&final.data$guilt=="low"]) # 112
length(final.data$guilt[final.data$study=="1a"&final.data$guilt=="low"&final.data$perspective=="borrower"]) # 59
length(final.data$guilt[final.data$study=="1a"&final.data$guilt=="low"&final.data$perspective=="lender"]) # 53

# Binomial test: high vs low responses
## 112 of 135 responses were "low"
binom.test(112,135,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 59 of 67 borrowers are "low"
binom.test(59,67,0.5, alternative = "greater") 

## 53 of 68 lenders are "low"
binom.test(53,68,0.5, alternative = "greater") 


# STUDY 1B
# H1b: More participants expect the borrower to feel guilty when 
# the costs to the lender are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$guilt[final.data$study=="1b"&final.data$guilt=="high"]) # 121
length(final.data$guilt[final.data$study=="1b"&final.data$guilt=="high"&final.data$perspective=="borrower"]) # 60
length(final.data$guilt[final.data$study=="1b"&final.data$guilt=="high"&final.data$perspective=="lender"]) # 61

# Binomial test: high vs low responses
## 121 of 133 responses were "high"
binom.test(121,133,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 60 of 66 borrowers are "high"
binom.test(60,66,0.5, alternative = "greater") 

## 63 of 67 lenders are "high"
binom.test(61,67,0.5, alternative = "greater") 



## SHAME ##

# STUDY 1A
# H2a: More participants expect the borrower to feel ashamed when 
# the benefits to the borrower are low.

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$shame[final.data$study=="1a"&final.data$shame=="low"]) # 113
length(final.data$shame[final.data$study=="1a"&final.data$shame=="low"&final.data$perspective=="borrower"]) # 59
length(final.data$shame[final.data$study=="1a"&final.data$shame=="low"&final.data$perspective=="lender"]) # 54


# Binomial test: high vs low responses
## 113 of 135 responses were "low"
binom.test(113,135,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 59 of 67 borrowers are "low"
binom.test(59,67,0.5, alternative = "greater") 

## 54 of 67 lenders are "low"
binom.test(54,68,0.5, alternative = "greater") 


# STUDY 1B
# H2b: More participants expect the borrower to feel ashamed when 
# the costs to the lender are high. 

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$shame[final.data$study=="1b"&final.data$shame=="high"]) # 121
length(final.data$shame[final.data$study=="1b"&final.data$shame=="high"&final.data$perspective=="borrower"]) # 61
length(final.data$shame[final.data$study=="1b"&final.data$shame=="high"&final.data$perspective=="lender"]) # 60

# Binomial test: high vs low responses
## 121 of 133 responses were "high"
binom.test(121,133,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 61 of 66 borrowers are "high"
binom.test(61,66,0.5, alternative = "greater") 

## 60 of 67 lenders are "high"
binom.test(60,67,0.5, alternative = "greater") 



## ANGER ##

# STUDY 1A
# H3a: More participants expect the lender to feel angry when 
# the benefits to the borrower are low.

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$anger[final.data$study=="1a"&final.data$anger=="low"]) # 123
length(final.data$anger[final.data$study=="1a"&final.data$anger=="low"&final.data$perspective=="borrower"]) # 60
length(final.data$anger[final.data$study=="1a"&final.data$anger=="low"&final.data$perspective=="lender"]) # 63


# Binomial test: high vs low responses
## 123 of 135 responses were "low"
binom.test(123,135,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 60 of 66 borrowers are "low"
binom.test(60,66,0.5, alternative = "greater") 

## 63 of 67 lenders are "low"
binom.test(63,67,0.5, alternative = "greater") 


# STUDY 1B
# H3b: More participants expect the lender to feel angry when 
# the costs to the lender are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$anger[final.data$study=="1b"&final.data$anger=="high"]) # 122
length(final.data$anger[final.data$study=="1b"&final.data$anger=="high"&final.data$perspective=="borrower"]) # 55
length(final.data$anger[final.data$study=="1b"&final.data$anger=="high"&final.data$perspective=="lender"]) # 67

# Binomial test: high vs low responses
## 122 of 133 responses were "high"
binom.test(122,133,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 55 of 67 borrowers are "high"
binom.test(55,67,0.5, alternative = "greater") 

## 67 of 68 lenders are "high"
binom.test(67,68,0.5, alternative = "greater") 



## GRATITUDE ##

# STUDY 1A
# H4a: More participants expect the borrower to feel grateful when 
# the benefits to the borrower are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$gratitude[final.data$study=="1a"&final.data$gratitude=="high"]) # 131
length(final.data$gratitude[final.data$study=="1a"&final.data$gratitude=="high"&final.data$perspective=="borrower"]) # 64
length(final.data$gratitude[final.data$study=="1a"&final.data$gratitude=="high"&final.data$perspective=="lender"]) # 67


# Binomial test: high vs low responses
## 131 of 135 responses were "high"
binom.test(131,135,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 64 of 67 borrowers are "high"
binom.test(64,67,0.5, alternative = "greater") 

## 67 of 68 lenders are "high"
binom.test(67,68,0.5, alternative = "greater") 


# STUDY 1B
# H4b: More participants expect the borrower to feel grateful when 
# the cost to the lender is high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$gratitude[final.data$study=="1b"&final.data$gratitude=="high"]) # 101
length(final.data$gratitude[final.data$study=="1b"&final.data$gratitude=="high"&final.data$perspective=="borrower"]) # 52
length(final.data$gratitude[final.data$study=="1b"&final.data$gratitude=="high"&final.data$perspective=="lender"]) # 49

# Binomial test: high vs low responses
## 101 of 133 responses were "high"
binom.test(101,133,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 52 of 66 borrowers are "high"
binom.test(52,66,0.5, alternative = "greater") 

## 49 of 67 lenders are "high"
binom.test(49,67,0.5, alternative = "greater") 




## INDEBTEDNESS ##

# STUDY 1A
# RQ1: Do more participants expect the borrower to feel indebted when 
# the benefits to the borrower are low?

# compute number of "low" responses in each category:
## overall
## borrowers
## lenders
length(final.data$indebtedness[final.data$study=="1a"&final.data$indebtedness=="low"]) # 27
length(final.data$indebtedness[final.data$study=="1a"&final.data$indebtedness=="low"&final.data$perspective=="borrower"]) # 13
length(final.data$indebtedness[final.data$study=="1a"&final.data$indebtedness=="low"&final.data$perspective=="lender"]) # 14


# Binomial test: high vs low responses
## 108 of 135 responses were "high"
binom.test(108,135,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 53 of 66 borrowers are "low"
binom.test(53,66,0.5, alternative = "greater") 

## 53 of 67 lenders are "low"
binom.test(53,67,0.5, alternative = "greater") 


# STUDY 1B
# H5b: More participants expect the borrower to feel indebted when 
# the costs to the lender are high.

# compute number of "high" responses in each category:
## overall
## borrowers
## lenders
length(final.data$indebtedness[final.data$study=="1b"&final.data$indebtedness=="high"]) # 118
length(final.data$indebtedness[final.data$study=="1b"&final.data$indebtedness=="high"&final.data$perspective=="borrower"]) # 60
length(final.data$indebtedness[final.data$study=="1b"&final.data$indebtedness=="high"&final.data$perspective=="lender"]) # 58

# Binomial test: high vs low responses
## 118 of 133 responses were "high"
binom.test(118,133,0.5,alternative = "greater") 

# Binomial test: independent verification for lenders and borrowers
## 60 of 66 borrowers are "high"
binom.test(60,66,0.5, alternative = "greater") 

## 58 of 67 lenders are "high"
binom.test(58,67,0.5, alternative = "greater") 



## REPAYMENT ##

# STUDY 1A
# RQ2: Do more participants (borrowers) prefer to repay first when 
# the benefits to them are low?

# number of participants in study 1a who answered repayment questions
# all in borrower perspective
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1a"]) # 67

# number of participants who answered "low"
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1a"&final.data$repayment=="low"]) # 28

# Binomial test: high vs low responses
## 28 of 67 responses were "low"
binom.test(28,67,0.5,alternative = "greater") 


# STUDY 1B
# H6b: More participants will want to repay the lender when the 
# costs to the lender are high.

# number of participants in study 1b who answered repayment questions
# all in borrower perspective
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1b"]) # 66

# number of participants who answered "high"
length(final.data$repayment[!is.na(final.data$repayment)&final.data$study=="1b"&final.data$repayment=="high"]) # 61

# Binomial test: high vs low responses
## 61 of 66 responses were "high"
binom.test(61,66,0.5,alternative = "greater") 



## ADDITIONAL ANALYSIS ##
# alignment between guilt, gratitude, indebtedness and 
# repayment intention.

response.data <- data %>%
  select(ID, study, indebtedness, guilt, gratitude, repayment)

data.matches <- response.data %>%
  filter(!is.na(repayment)) %>%
  rowwise() %>%
  mutate(guilt.match = ifelse(guilt == repayment, 1, 0)) %>%
  mutate(gratitude.match = ifelse(gratitude == repayment, 1, 0)) %>%
  mutate(indebtedness.match = ifelse(indebtedness == repayment, 1, 0)) %>%
  group_by(ID) %>%
  mutate(gu_match = sum(guilt.match)/3) %>%
  mutate(gr_match = sum(gratitude.match)/3) %>%
  mutate(in_match = sum(indebtedness.match)/3)



# T-tests between matching scores

# Study 1a
## gratitude vs guilt
## indebtedness vs guilt
## indebtedness vs gratitude
t.test(x=data.matches$gr_match[data.matches$study=="1a"], y=data.matches$gu_match[data.matches$study=="1a"])
t.test(x=data.matches$in_match[data.matches$study=="1a"], y=data.matches$gu_match[data.matches$study=="1a"])
t.test(x=data.matches$in_match[data.matches$study=="1a"], y=data.matches$gr_match[data.matches$study=="1a"])


# Study 1b
## gratitude vs guilt
## indebtedness vs guilt
## indebtedness vs gratitude
t.test(x=data.matches$gr_match[data.matches$study=="1b"], y=data.matches$gu_match[data.matches$study=="1b"])
t.test(x=data.matches$in_match[data.matches$study=="1b"], y=data.matches$gu_match[data.matches$study=="1b"])
t.test(x=data.matches$in_match[data.matches$study=="1b"], y=data.matches$gr_match[data.matches$study=="1b"])



