install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(ggplot2)

data<-read_excel("C:\\workspace\\R_projects\\nyt1.xlsx")

summary(data)

#> summary(data)
#Age             Gender       Impressions         Clicks          Signed_In     
#Min.   :  0.00   Min.   :0.000   Min.   : 0.000   Min.   :0.00000   Min.   :0.0000  
#1st Qu.:  0.00   1st Qu.:0.000   1st Qu.: 3.000   1st Qu.:0.00000   1st Qu.:0.0000  
#Median : 31.00   Median :0.000   Median : 5.000   Median :0.00000   Median :1.0000  
#Mean   : 29.48   Mean   :0.367   Mean   : 5.007   Mean   :0.09259   Mean   :0.7009  
#3rd Qu.: 48.00   3rd Qu.:1.000   3rd Qu.: 6.000   3rd Qu.:0.00000   3rd Qu.:1.0000  
#Max.   :108.00   Max.   :1.000   Max.   :20.000   Max.   :4.00000   Max.   :1.0000 

dim(data)
#[1] 458441      5

## Few of the clicks have "age=0" and this needs to be imputed 

rows_with_age_zero<-data$Age==0

data_with_zero<-data[rows_with_age_zero,]

dim(data_with_zero)
#[1] 137106      5


## replace the null values of age with mean age of 29 years from the summary

data$Age<-ifelse(data$Age==0,29,data$Age)

## Check the updated summary stats
summary(data)
# 
# Age             Gender       Impressions         Clicks          Signed_In     
# Min.   :  7.00   Min.   :0.000   Min.   : 0.000   Min.   :0.00000   Min.   :0.0000  
# 1st Qu.: 29.00   1st Qu.:0.000   1st Qu.: 3.000   1st Qu.:0.00000   1st Qu.:0.0000  
# Median : 31.00   Median :0.000   Median : 5.000   Median :0.00000   Median :1.0000  
# Mean   : 38.16   Mean   :0.367   Mean   : 5.007   Mean   :0.09259   Mean   :0.7009  
# 3rd Qu.: 48.00   3rd Qu.:1.000   3rd Qu.: 6.000   3rd Qu.:0.00000   3rd Qu.:1.0000  
# Max.   :108.00   Max.   :1.000   Max.   :20.000   Max.   :4.00000   Max.   :1.0000  
# 

data
# # A tibble: 458,441 × 7
# Age Gender Impressions Clicks Signed_In age_group    CTR
# <dbl>  <dbl>       <dbl>  <dbl>     <dbl> <fct>      <dbl>
#   1    36      0           3      0         1 35-44     0     
# 2    73      1           3      0         1 65+       0     
# 3    30      0           3      0         1 25-34     0     
# 4    49      1           3      0         1 45-54     0     
# 5    47      1          11      0         1 45-54     0     
# 6    47      0          11      1         1 45-54     0.0909
# 7    29      0           7      1         0 25-34     0.143 
# 8    46      0           5      0         1 45-54     0     
# 9    16      0           3      0         1 <18       0     
# 10    52      0           4      0         1 45-54     0     
# # ℹ 458,431 more rows
# # ℹ Use `print(n = ...)` to see more rows

dim(data)
# [1] 458441      5

## a. Create a new variable, age_group, that categorizes users as “<18”, ”18-24”, ”25-34”, ”35-44”, ”45-54”, “55-64” and “65+”.

## Define the age intervals and age_labels and label each row with their respective age_group

age_intervals <- c(0, 17, 24, 34, 44, 54, 64, Inf)
age_labels <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

data1 <- mutate(data, age_group = cut(data$Age, breaks = age_intervals, labels = age_labels))

data1
# # A tibble: 458,441 × 6
# Age Gender Impressions Clicks Signed_In age_group
# <dbl>  <dbl>       <dbl>  <dbl>     <dbl> <fct>    
#   1    36      0           3      0         1 35-44    
# 2    73      1           3      0         1 65+      
#   3    30      0           3      0         1 25-34    
# 4    49      1           3      0         1 45-54    
# 5    47      1          11      0         1 45-54    
# 6    47      0          11      1         1 45-54    
# 7    29      0           7      1         0 25-34    
# 8    46      0           5      0         1 45-54    
# 9    16      0           3      0         1 <18      
# 10    52      0           4      0         1 45-54    
# # ℹ 458,431 more rows

## check new dataframe dimensions

dim(data1)
##[1] 458441      6

### Q1 . Plot the distributions of number impressions and click-through-rate (CTR=#clicks/# impressions), for these 6 age categories. [You will turn in a plot called “single_day.pdf” with these results]

### Plot the distribution of Impressions by age_group 

p<-ggplot(data1, aes(x = Impressions, fill = age_group)) + 
        geom_density(alpha = 0.5) +
        xlab("Number of impressions") +
        ggtitle("Distribution of impressions by age group")

ggsave("c:\\workspace\\impressions.png",plot=p,dpi=300)
##Saving 6.92 x 6.92 in image


## Plot the distribution of CTR by age_group 

ctr_plot<-ggplot(data1, aes(x = CTR, fill = age_group)) +
       geom_density(alpha = 0.3) +
       xlab("Click-through-rate (CTR)") +
       ggtitle("Distribution of CTR by age group")

ggsave("c:\\workspace\\click_through_rate.png",plot=ctr_plot,dpi=300)
# Saving 6.92 x 6.92 in image
# Warning message:
#  Removed 3066 rows containing non-finite values (`stat_density()`). 


#*** Q2 : Define a new variable to segment or categorize users based on their click behavior ###


summary(data1)
# Age             Gender       Impressions         Clicks          Signed_In     
# Min.   :  7.00   Min.   :0.000   Min.   : 0.000   Min.   :0.00000   Min.   :0.0000  
# 1st Qu.: 29.00   1st Qu.:0.000   1st Qu.: 3.000   1st Qu.:0.00000   1st Qu.:0.0000  
# Median : 31.00   Median :0.000   Median : 5.000   Median :0.00000   Median :1.0000  
# Mean   : 38.16   Mean   :0.367   Mean   : 5.007   Mean   :0.09259   Mean   :0.7009  
# 3rd Qu.: 48.00   3rd Qu.:1.000   3rd Qu.: 6.000   3rd Qu.:0.00000   3rd Qu.:1.0000  
# Max.   :108.00   Max.   :1.000   Max.   :20.000   Max.   :4.00000   Max.   :1.0000  
# 
# age_group           CTR        
# <18  : 13828   Min.   :0.0000  
# 18-24: 40694   1st Qu.:0.0000  
# 25-34:195280   Median :0.0000  
# 35-44: 70860   Mean   :0.0185  
# 45-54: 64288   3rd Qu.:0.0000  
# 55-64: 44738   Max.   :1.0000  
# 65+  : 28753   NA's   :3066  


data1$click_behaviour <- cut(data1$CTR, breaks= c(0,0.01,0.05,0.1,1),
                             labels=c("Low","Medium","High","Very High"))


data1
# # A tibble: 458,441 × 8
# Age Gender Impressions Clicks Signed_In age_group    CTR click_behaviour
# <dbl>  <dbl>       <dbl>  <dbl>     <dbl> <fct>      <dbl> <fct>          
#   1    36      0           3      0         1 35-44     0      NA             
# 2    73      1           3      0         1 65+       0      NA             
# 3    30      0           3      0         1 25-34     0      NA             
# 4    49      1           3      0         1 45-54     0      NA             
# 5    47      1          11      0         1 45-54     0      NA             
# 6    47      0          11      1         1 45-54     0.0909 High           
# 7    29      0           7      1         0 25-34     0.143  Very High      
# 8    46      0           5      0         1 45-54     0      NA             
# 9    16      0           3      0         1 <18       0      NA             
# 10    52      0           4      0         1 45-54     0      NA             
# # ℹ 458,431 more rows
# # ℹ Use `print(n = ...)` to see more rows



#### Explore the data and make visual and quantitative comparisons across 
### user segments/ demographics (<18 year old male vs < 18 year old females or loggedin vs not, for example)


#This code creates a bar chart that shows the average CTR for each age group and gender. The stat = "summary" argument tells ggplot2 to compute the summary statistic (mean) of the ctr variable, and the position = "dodge" argument stacks the bars for each age group side-by-side.

ggplot(df, aes(x = age_group, y = ctr, fill = gender)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge") +
  labs(x = "Age group", y = "CTR", fill = "Gender") +
  theme_bw()