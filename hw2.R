###############################################################
#(1)

df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))

# df1 is defined by calling the function data.frame()
# This function takes three vectors as arguments
# The name of each vector will become the column name
# Each of the vector's values will populate a single column
# the function return's a 3x12 data frame where the row follows the template: (Name, State, Sales)

aggregate(df1$Sales, by=list(df1$State), FUN=sum)
#aggregate is a function that will split the data into groups then will return a new list with desired operations performed
#It takes all values for sales ordered by the occurrence of each state in the State column and then adds the value of each respective together

library(dplyr)
#import library for data manipulation functions

df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))
#use function to group states in df1 and then add together the sales of the each state entry

###############################################################

#(2)

df <- read.delim("WorldCupMatches.csv", sep=",", header=TRUE)

#(2a)
dim(df)

#(2b)
summary(df)

#(2c)
unique(df["Stadium"])

#(2d)
df2 <- df[is.na(df["Attendance"]) == FALSE,]
mean(df2$Attendance)

#(2e)
df %>% group_by(Home.Team.Name) %>% summarise(sum(Home.Team.Goals))

#(2f)
year <- df2 %>% group_by(Year) %>% summarise(mean(Attendance))
#There doesn't to be an apparent trend or pattern except for maybe that attendance that is greater than 
#the preceding entry then has smaller attendance coming after it for a couple of entries
  # Example:
    # 1930 - 32808.28 (Big)
    # 1934 - 21352.94 (Smaller)
    # 1938 - 20872.22 (Smaller)
    # 1950 - 47511.18 (Big)
    # 1954 - 29561.81 (Smaller)
    # 1958 - 23423.14 (Smaller)

###############################################################

#(3)

df3 <- read.delim("metabolite.csv", sep=",", header=TRUE)

#(3a)
alz <- df3[df3$Label == "Alzheimer",]
count(alz["Label"])

#(3b)
#Using: https://dplyr.tidyverse.org/reference/summarise_all.html
missing <- df3 %>% summarise(across(everything(), ~ sum(is.na(.))))

#(3c)
df4 <- df3[is.na(df3$Dopamine) == FALSE,]

#(3d)
c4_med <- median(df4[is.na(df4$c4.OH.Pro) == FALSE,]$c4.OH.Pro)
df4$c4.OH.Pro[is.na(df4$c4.OH.Pro)] <- c4_med

#(3e)
#69 entries for each column, since num_of_nas (18) / 69 > 0.25 then it will be dropped
threshhold <- df3 %>% summarise(across(everything(), ~ sum(is.na(.)) > 18))
df5 <- df3[,threshhold == FALSE]
