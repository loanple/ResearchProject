# ResearchProject

library(tidyverse)
library(dplyr)
library(ggplot2)

#GENERATING DATA
#Frequency of Mood swings
  #Experimental group -
    freq_moodswingsXG = sample(1:5, size=150, replace=TRUE, prob=c(.02,.367,.327,.167,.12))
    barplot(table(freq_moodswingsXG))
  #Control group -
    freq_moodswingsCG = sample(2:5, size=150, replace=TRUE, prob=c(.15,.19,.38,.28))
    barplot(table(freq_moodswingsCG))

  f_moodswingsXG <- cbind(group = "Experimental Group", scale = freq_moodswingsXG)
  f_moodswingsCG <- cbind(group = "Control Group", scale = freq_moodswingsCG)
    
  freq_moodswings <- rbind(f_moodswingsXG , f_moodswingsCG)

#Mental health state
  #Experimental group -
    men_stateXG = sample(1:5, size=150, replace=TRUE, prob=c(.01,.1,.16,.42,.31))
    barplot(table(men_stateXG))
  #Control group -
    men_stateCG = sample(1:5, size=150, replace=TRUE, prob=c(.27,.41,.19,.1,.03))
    barplot(table(men_stateCG))
    
  m_stateXG <- cbind(group = "Experimental Group", scale = men_stateXG)
  m_stateCG <- cbind(group = "Control Group", scale = men_stateCG)
    
  men_state <- rbind(m_stateXG, m_stateCG)

#Frequency of usage
  #Experimental group -
    freq_usageXG = sample(3:5, size=150, replace=TRUE, prob=c(.205,.337,.46))
    barplot(table(freq_usageXG)) 
  #Control group -
    freq_usageCG = sample(2:5, size=150, replace=TRUE, prob=c(.053,.313,.253,.38))
    barplot(table(freq_usageCG))
    
  freq_usageXG <- cbind(group = "Experimental Group", scale = freq_usageXG)
  freq_usageCG <- cbind(group = "Control Group", scale = freq_usageCG)
    
  freq_usage <- rbind(freq_usageXG, freq_usageCG)
    
#Gender
    G_XG = sample(c('M', 'F'), 150, replace=TRUE, prob=c(0.5, 0.5))
    G_CG = sample(c('M', 'F'), 150, replace=TRUE, prob=c(0.5, 0.5))
    G_XG <- cbind(group = "Experimental Group", gender = G_XG)
    G_CG <- cbind(group = "Control Group", gender = G_CG)
    gender <- rbind(G_XG, G_CG)
    gender


#Converted to excel to put together & created dataframe
    data = read.csv("/Users/loanple/Desktop/Tidy_data.csv")
    str(data)

#Reviewing descriptive statistics of data
  round(tapply(data$Mstate_Scale, data$Group, mean),0)
  round(tapply(data$Fmswing_Scale, data$Group, mean),0)
  round(tapply(data$Freq_usage, data$Group, mean),0)
  
  ggplot(data, aes(Group, Fmswing_Scale)) + geom_boxplot(aes(col=Group)) + labs(title = "Frequency Mood Swing by    Group")
  ggplot(data, aes(Group, Mstate_Scale)) + geom_boxplot(aes(col=Group)) + labs(title = "Mental Health State by      Group")


#T-test on Likert Scale of Men_state & Frequency of Mood Swings
    install.packages("ggpubr")
    library("ggpubr")
    
    res<-t.test(Mstate_Scale ~ Group, data=data, var.equal = TRUE)
    res
    
    res$p.value
    
    
    res<-t.test(Fmswing_Scale ~ Group, data=data, var.equal=TRUE)
    res
    
    "hi"


