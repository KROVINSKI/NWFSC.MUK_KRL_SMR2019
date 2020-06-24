##Hello World

## Author: Alboukadel Kassambara
## Title: DATA MANIPULATION IN R ; HOME/ DATA MANIPULATION IN R / IDENTIFY AND REMOVE DUPLICATE DATA IN R
## Date.Accessed: May 2020

# Data Novia Tutorial on how to remove duplicates

## About

# This tutorial describes how to identify and remove duplicate data in R.
# 
# You will learn how to use the following R base and dplyr functions:
#   
#   R base functions
#             duplicated(): for identifying duplicated elements and
#             unique(): for extracting unique elements,
#             distinct() [dplyr package] to remove duplicate rows in a data frame.



#*********************************
##Contents
#********************************* 
# 1. Required packages
# 2. Demo dataset
# 3. Find and drop duplicate elements
# 4. Extract unique elements
# 5. Remove duplicate rows in a data frame
# 6. Summary


#*********************************
##Libraries - Required packages
#********************************* 

library(tidyverse)


#*********************************
##2. Demo dataset
#*********************************


my_data <- as_tibble(iris)
my_data


#*********************************
##3. Find and drop duplicate elements
#*********************************

# The R function duplicated() returns a logical vector 
# where TRUE specifies which elements of a vector or 
# data frame are duplicates.

# Given the following vector:
  
x <- c(1, 1, 4, 5, 4, 6)
x


# find the position of duplicate elements in x, use
duplicated(x)

# extract the duplicate
x[duplicated(x)]


# If you want to remove duplicated elements, use !duplicated(), 
# where ! is a logical negation:

x[!duplicated(x)]
  

# Example of how to delete duplicate rows of data

# Following this way, 
# you can remove duplicate rows from a data frame 
# based on a column values, as follow:

# Remove duplicates based on Sepal.Width columns
my_data[!duplicated(my_data$Sepal.Width), ]

#///////
# NOTE /
#///////

# ! is a logical negation. !duplicated() means that we don’t want duplicate rows.

#///////
# NOTE /
#///////

  
  
#*********************************
##4. Extract unique elements
#*********************************    
  
# Recap, Given the following vector
x <- c(1, 1, 4, 5, 4, 6)

# Function to use to extract unique elements as follows:

unique(x)

# Use on a dataframe 
# apply unique() on a data frame, 
#                 for removing duplicated rows as follow:

unique(my_data)


#*********************************
## 5. Remove duplicate rows in a data frame
#********************************* 

#             DISTINCT             #

#The function distinct() [dplyr package] 
#can be used to keep only unique/distinct 
#rows from a data frame. 
#
#If there are duplicate rows, 
#only the first row is preserved. 

# It’s an efficient version of the R
# base function unique().


my_data %>% distinct()



# Remove duplicate rows based on certain columns (variables):


# Remove duplicated rows based on Sepal.Length
my_data %>% distinct(Sepal.Length, .keep_all = TRUE)

# Remove duplicated row based on 2 variables
# Remove duplicated rows based on 
# Sepal.Length and Petal.Width

my_data %>% distinct(Sepal.Length, Petal.Width, .keep_all = TRUE)



# The option .kep_all is used to keep all variables in the data.


#*********************************
## 6. Summary
#********************************* 

# In this chapter, we describe key functions for identifying and 
#  removing duplicate data:
#   
# Remove duplicate rows based on one or more column values: 
#  my_data %>% dplyr::distinct(Sepal.Length)

# R base function to extract unique elements from vectors and 
#  data frames: unique(my_data)

# R base function to determine duplicate elements: duplicated(my_data)






  
  



  










#**********E*N*D*****************# 
#*********************************
## END OF DOCUMENT | END OF SCRIPT  
#*********************************


