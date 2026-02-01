### Reading from files
## get the working directory
getwd()
# C:/Users/Admin/Downloads/2. St Claret/R Programming
## Set the working directory
setwd("C:/Users/Admin/Downloads/2. St Claret/R Programming")
getwd()


# Installing the package for data analysis
install.packages("tidyverse")

library(tidyverse)

## Reading CSV files
titanic <- read.csv("titanic_data.csv",header=T)

# Structure of the data
str(titanic)

# First six records
head(titanic)

# Last six records
tail(titanic)

# Summary of the data
summary(titanic)

## Selecting specific columns
titanic$Survived # Selecting a single column

titanic[,c("Name","Gender","Fare")] # Selecting multiple columns

titanic[,5:8]# Select Columns by Index

###### FILTER Operations (Row Selection)
# Filter and select passengers above the age of 35
titanic[titanic$Age > 35,c("PassengerId","Gender","Age")]

# Selecting using SELECT - Select specific columns
sel_set_1 <- titanic %>% select(Pclass, Age, Fare, Survived)

# Females only
female_passengers <- titanic %>% 
  filter(Gender == "female") %>% select(Pclass, Age, Fare, Survived)

# Creating New Columns from File Data
titanic$Survived_Status = ifelse(titanic$Survived==1,"Survived","Not Survived")

# Create a FamilyCount Column using mutate()
titanic <- titanic %>% mutate(FamilyMembers=titanic$SibSp+titanic$Parch) 

# Create an Adult / Child Column using age
titanic <- titanic %>% mutate(AgeGroup=ifelse(titanic$Age>18,"Adult","Child"))


#### SORTING #########################
# Sort by ascending Fare
fares_asc <- titanic %>%  arrange(Fare)

# Sort by descending frequency
fares_dsc <- titanic %>% arrange(desc(Fare))

# Sort by Class then Gender then Age
class_gender_age <- titanic %>%  arrange(Pclass, Gender, Age)

#### Another way of ordering - optional
titanic$Fare[order(titanic$Fare)] # Ascending order
titanic$Fare[order(titanic$Fare,decreasing = T)] # Descending order


# Update the Age of passender id 1 to 23
# What is the current age of passenger id 1
titanic[titanic$PassengerId == 1,"Age"] # 22
titanic$Age[titanic$PassengerId == 1]

# Update it to 23
titanic$Age[titanic$PassengerId == 1] <- 23
titanic$Age[titanic$PassengerId == 1]

# Group By
# Find the number of male/female passengers
titanic %>% group_by(Gender) %>% summarise(Count = n())

# Count survivors by gender
titanic %>% group_by(Gender) %>% summarise(Survivors = sum(Survived))

# Count passengers by class
titanic %>% group_by(Pclass) %>% summarise(Count = n())

# Count survivors by class
titanic %>% group_by(Pclass) %>% summarise(Survivors = sum(Survived)) %>% 
  arrange(Survivors)

# Writing data to a file
write.csv(titanic, "Titanic_Modified.csv")


###### Optional Code for reading the files
############################################################################
# Reading from a text file
text_data <- readLines("text_data.txt")
text_data

students <- read.table("student_marks.txt", header = TRUE)
students

# Writing to a text file
student_data <- data.frame(
  Name  = c("Abhishek", "Mayuri", "Arun Peter", "Risha"),
  Age   = c(20, 21, 19, 22),
  Marks = c(85, 90, 78, 88)
)

write.table(
  student_data,
  "StudentDetails.txt",
  sep = "\t",
  row.names = FALSE
)






