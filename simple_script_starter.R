#check for packages
list.of.packages <- c("tidyverse", "devtools","readxl","rio","knitr","dataverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library(dataverse)
library(readxl)
library(tidyverse)
library(rio)
library(knitr)

#get the dataset overview
#be sure to not have a proxy behind a firewall
get_dataset("doi:10.7910/DVN/YG9IID")
#read in the file
f <- get_file("anonymized_survey.csv", "doi:10.7910/DVN/YG9IID")
# load it into memory
tmp <- tempfile(fileext = ".csv")
writeBin(as.vector(f), tmp)
#get data file
dat <- rio::import(tmp)
rm(f,tmp)


#format the year variable
current.year<-as.integer(format(Sys.Date(),"%Y"))
#use dply and pipe operator to crete data frame to plot later
plot.dat<-
  dat %>% #use initial data frame
  mutate(age=current.year-year_born) %>% #create a new variable by calculating the age
  filter(age>0&age<100) %>% #remove outliers (some pps gave an age that was too high or negative)
  group_by(Sex) %>% #group the following analysis by pp  sex
  summarise(mean_age=mean(age), #create summary variables (mean and sd)
            sd_age=sd(age),
            N=n(),
            se_age=sd_age/sqrt(N))

#plot the data frame
ggplot(aes(y=mean_age,x=Sex,fill=Sex),data=plot.dat)+ #set the aesthetics
  geom_bar(stat="identity")+ #make a bar plot
  geom_errorbar(aes(ymin=mean_age-se_age,ymax=mean_age+se_age,width=0.2))+ #and add error bars
  theme_classic()+ #use the classic theme to remove clutter
  xlab("Gender")+ #add axis labels
  ylab("Mean Age")+
  scale_fill_manual(values=c("pink","blue"))+ #set the colour of bars manually
  theme(legend.position = "none") #no legend needed
