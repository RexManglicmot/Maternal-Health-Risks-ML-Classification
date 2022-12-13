Maternal Health Risks Classification KNN and Decision Trees
================
Rex Manglicmot

-   <a href="#status-continuing-working-document"
    id="toc-status-continuing-working-document">Status: Continuing Working
    Document</a>
-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#loading-the-libraries" id="toc-loading-the-libraries">Loading
    the Libraries</a>
-   <a href="#loading-the-data" id="toc-loading-the-data">Loading the
    Data</a>
-   <a href="#cleaning-the-data" id="toc-cleaning-the-data">Cleaning the
    Data</a>
-   <a href="#exploratory-data-analysis"
    id="toc-exploratory-data-analysis">Exploratory Data Analysis</a>
-   <a href="#modeling-k-neighrest-neighbors"
    id="toc-modeling-k-neighrest-neighbors">Modeling: K-Neighrest
    Neighbors</a>
-   <a href="#modeling-decision-trees"
    id="toc-modeling-decision-trees">Modeling: Decision Trees</a>
-   <a href="#limitations" id="toc-limitations">Limitations</a>
-   <a href="#conclusion" id="toc-conclusion">Conclusion</a>
-   <a href="#appendix-random-plots" id="toc-appendix-random-plots">Appendix
    (Random Plots)</a>
-   <a href="#inspiration-for-this-project"
    id="toc-inspiration-for-this-project">Inspiration for this project</a>

## Status: Continuing Working Document

Hi everyone. I’m continuing building my data analysis and R skills. As
such, I would love feedback to better improve this project via
<rexmanglicmot@gmail.com>. Any mistakes and misrepresentation of the
data are my own.

Things Need to Do/Questions:

-   provide more context on what is “maternal health risks” according to
    the site
-   rewrite intro and cite sources
-   state objective of this project more convincingly
-   provide more concepts/theory about KNN and Decision Trees in each of
    their sections
-   check grammar
-   provide more insights into the models
-   check validation tests for the moels
-   get feedback and incorporate

## Introduction

Maternal health risks has increased since the medieval days. However,
there is still a need to understand the underlying factors that
contribute to women health including metrics such as age and blood sugar
levels over various age groups.

This projects aims to learn more about such predictors by using machine
learning methods such as KNN and Random Forests to see if factors can
accurately predict maternal health risks.

This project is outlined in the following chapters:

1.  Loading the Libraries
2.  Loading the Data
3.  Cleaning the Data
4.  Exploratory Data Analysis
5.  Modeling: K-Neighrest Neighbors
6.  Modeling: Decision Trees
7.  Limitations
8.  Conclusion
9.  Appendix
10. Inspiration for this project

A special acknowledgement to the University of Irvine’s Machine Learning
Repository for providing the dataset to the public.[^1] A special
acknowledgement to Marzia Ahmed from the Daffodil International
University in Dhaka, Bangladesh and the rest of the coauthors for their
contribution of their research dataset.[^2]

Based on UCI’s Repository below are how each variable is measured:

1.  Age: years when the women was pregnant.
2.  Systolic BP (blood pressure): upper value of Blood Pressure in mmHg
3.  Diastolic BP (blood pressure): lower value of Blood Pressure in mmHg
4.  Blood Sugar: molar concentration, mmol/L
5.  Body Temp: in Celsius
6.  Heart Rate: : normal resting heart rate in beats per minute
7.  Risk Level: predicted Risk Intensity Level during pregnancy
    considering the previous attribute (unclear, assume it is heart
    rate?)

## Loading the Libraries

``` r
#load libraries
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(gghalves)
#library(GGally)
library(class)
library(ggridges)
library(party)
library(partykit)
library(rpart)
library(rpart.plot)
```

## Loading the Data

``` r
#load data
data <- read.csv('Maternal Health Risk Data Set.csv')
```

## Cleaning the Data

``` r
#print first few rows of the data
head(data,n=10)
```

    ##    Age SystolicBP DiastolicBP    BS BodyTemp HeartRate RiskLevel
    ## 1   25        130          80 15.00       98        86 high risk
    ## 2   35        140          90 13.00       98        70 high risk
    ## 3   29         90          70  8.00      100        80 high risk
    ## 4   30        140          85  7.00       98        70 high risk
    ## 5   35        120          60  6.10       98        76  low risk
    ## 6   23        140          80  7.01       98        70 high risk
    ## 7   23        130          70  7.01       98        78  mid risk
    ## 8   35         85          60 11.00      102        86 high risk
    ## 9   32        120          90  6.90       98        70  mid risk
    ## 10  42        130          80 18.00       98        70 high risk

``` r
#Change BS to BloodSugar for better understanding
colnames(data)[4] <-'BloodSugar'

#glimpse data
glimpse(data)
```

    ## Rows: 1,014
    ## Columns: 7
    ## $ Age         <int> 25, 35, 29, 30, 35, 23, 23, 35, 32, 42, 23, 19, 25, 20, 48…
    ## $ SystolicBP  <int> 130, 140, 90, 140, 120, 140, 130, 85, 120, 130, 90, 120, 1…
    ## $ DiastolicBP <int> 80, 90, 70, 85, 60, 80, 70, 60, 90, 80, 60, 80, 89, 75, 80…
    ## $ BloodSugar  <dbl> 15.00, 13.00, 8.00, 7.00, 6.10, 7.01, 7.01, 11.00, 6.90, 1…
    ## $ BodyTemp    <dbl> 98, 98, 100, 98, 98, 98, 98, 102, 98, 98, 98, 98, 98, 100,…
    ## $ HeartRate   <int> 86, 70, 80, 70, 76, 70, 78, 86, 70, 70, 76, 70, 77, 70, 88…
    ## $ RiskLevel   <chr> "high risk", "high risk", "high risk", "high risk", "low r…

Now, with the relevant libraries are loaded and the dataset variables
are clearly understood, next steps are to clean and check for missing
values, characters that are unothordox to each of the metrics via R
codes below.

``` r
#check for  missing data in full dataset 
#difficult to do when there are lots of observations
sum(is.na(data))
```

    ## [1] 0

``` r
#double check for missing data via different columns
#again difficult
sum(is.na(data$Age))
```

    ## [1] 0

This codes above returns the dataset in which if there are no NA, FALSE
will be printed in the whole dataset and the Age column.

Let’s try another method.

``` r
#check for missing data for each variable column using sum
#output is 0 for all columns
colSums(is.na(data))
```

    ##         Age  SystolicBP DiastolicBP  BloodSugar    BodyTemp   HeartRate 
    ##           0           0           0           0           0           0 
    ##   RiskLevel 
    ##           0

``` r
#find location of missing data, if any
which(is.na(data))
```

    ## integer(0)

``` r
#get percentage of missing data in dataset
mean(is.na(data))
```

    ## [1] 0

The dataset is cleaned and tidy. Next steps are to do EDA.

## Exploratory Data Analysis

``` r
#Summary statistics of each of the variables
str(data)
```

    ## 'data.frame':    1014 obs. of  7 variables:
    ##  $ Age        : int  25 35 29 30 35 23 23 35 32 42 ...
    ##  $ SystolicBP : int  130 140 90 140 120 140 130 85 120 130 ...
    ##  $ DiastolicBP: int  80 90 70 85 60 80 70 60 90 80 ...
    ##  $ BloodSugar : num  15 13 8 7 6.1 7.01 7.01 11 6.9 18 ...
    ##  $ BodyTemp   : num  98 98 100 98 98 98 98 102 98 98 ...
    ##  $ HeartRate  : int  86 70 80 70 76 70 78 86 70 70 ...
    ##  $ RiskLevel  : chr  "high risk" "high risk" "high risk" "high risk" ...

Many of the variables except RiskLevel are factors and let’s see what
the distributions are using the summary function.

``` r
#Structure of data without the RiskLevel variable because it is a character
summary(data[-7])
```

    ##       Age          SystolicBP     DiastolicBP       BloodSugar    
    ##  Min.   :10.00   Min.   : 70.0   Min.   : 49.00   Min.   : 6.000  
    ##  1st Qu.:19.00   1st Qu.:100.0   1st Qu.: 65.00   1st Qu.: 6.900  
    ##  Median :26.00   Median :120.0   Median : 80.00   Median : 7.500  
    ##  Mean   :29.87   Mean   :113.2   Mean   : 76.46   Mean   : 8.726  
    ##  3rd Qu.:39.00   3rd Qu.:120.0   3rd Qu.: 90.00   3rd Qu.: 8.000  
    ##  Max.   :70.00   Max.   :160.0   Max.   :100.00   Max.   :19.000  
    ##     BodyTemp        HeartRate   
    ##  Min.   : 98.00   Min.   : 7.0  
    ##  1st Qu.: 98.00   1st Qu.:70.0  
    ##  Median : 98.00   Median :76.0  
    ##  Mean   : 98.67   Mean   :74.3  
    ##  3rd Qu.: 98.00   3rd Qu.:80.0  
    ##  Max.   :103.00   Max.   :90.0

We see different distributions based on each metric. We see that the
SystolicBP and DiastolicBP are normally distributed,and thus nothing
interesting to probe with these metrics. The same can also be said about
bodyTemp, there is no variance between each summary statistics, further,
the regular body temp is \~98 degrees.

BloodSugar and Heartrate have interesting summary statistics, regarding
the max and min, respectively. BloodSugar max value is 19, while the
rest of the values are somewhat normally distributed. Heartrate has a
min value of 7, while the rest of the values are also somewhat normally
distributed. This means that both metrics have outliers, which skews the
mean towards the outliers. Unlike HeartRate, the mean of BloodSugar is
near the median indicating that the majority of the observatins are near
the 6-8 value range.

Age is interesing because the min and max are 10 and 70, respectively.
Generally, as the age increases, there tends to be more complications
and thus, this is a variable that I will focus on.

But first, let’s just focus on the RiskLevel variable and see what
values are present and each of their count, and visualize the count via
a barchart.

``` r
#what are the of categories within Risk Level
distinct(data,RiskLevel)
```

    ##   RiskLevel
    ## 1 high risk
    ## 2  low risk
    ## 3  mid risk

``` r
#sum categories within Risk Level
data %>%
  group_by(RiskLevel) %>%
  count()
```

    ## # A tibble: 3 × 2
    ## # Groups:   RiskLevel [3]
    ##   RiskLevel     n
    ##   <chr>     <int>
    ## 1 high risk   272
    ## 2 low risk    406
    ## 3 mid risk    336

``` r
#Visualize count the number of categories within Risk Level
ggplot(data, aes(x=RiskLevel, fill=RiskLevel)) +
  geom_bar(width=.5, color ='black') +
  labs(title = 'Risk Level Bar Chart',
    x= 'Types of Risks',
    y= "Count") +
  scale_fill_brewer(palette = 'RdPu') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits=c('high risk', 'mid risk', 'low risk')) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, 
            color = "black")
```

    ## Warning: The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(count)` instead.

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

From the above code and bar chart we see that there are three different
types of Risk Levels associated with this dataset that the author define
as; high, mid, and low risks. From the bar chart above we conclude that
the counts are high \< mid \< low.

Now, let’s focus on exploring the Age variable.

``` r
#Histogram of Ages
#it looks bimodal
ggplot(data, aes(Age)) +
  geom_histogram(binwidth=5, fill = 'pink', 
                 color = 'white') +
  theme_bw() +
  ggtitle('Age Distribution of Participants') +
  labs(y = 'Frequency Distribution') +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

A histogram is a means to show the frequency distribution as
observations are put into designated bins.

Some insights by ,anipulating the bindwidth, I find the shape of the
distribution is *bimodal* signifying that there could be two groups I
could possible take a look at. The first and second peak is Age\~20 and
Age\~50. A possible path is that I could break up the dataset into two
parts based on the peaks (modes) and each art would have its own summary
statistics.

But for now, let’s continue with the EDA within age variable.

``` r
ggplot(data, aes(x=RiskLevel, y=Age, fill=RiskLevel)) +
  geom_boxplot(width=0.5,
               outlier.color='darkgreen') +
  theme_bw() +
  scale_fill_brewer(palette = 'RdPu') +
  scale_x_discrete(limits=c('high risk', 'mid risk', 'low risk')) +
  ggtitle('Age Distribution based on 
  Risk Level Category') +
  labs(y = 'Age Distribution') +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Another way is see the distribution is via boxplot. Like the summary()
function this plot shows the min, Q1, median, Q3, and max. But unlike
that function, the boxplot helps us visualize the data and point out the
outliers, which are not shown in the summary table, as shown as green
dots here.

Some insights from this graph is that shape of the boxes and whisker
lengths are almost identical between the mid and low risk categories.
This means that based on this dataset’s Age, it is difficult to
distinguish between the two. In addition, both have outliers that are
above the age, 50. The outliers skews the data, and in this case, pulls
the mid and low risk categories upwards to the higher age range. The
best way to visualize this is via a density plot, which we will go over
in the next section.

The key insight is that of the high risk category boxplot. The “box”
which contains 50% of the data is much bigger in comparison to mid and
low risk. What this means is that women between the ages of roughly
22-48 could possibly lie within a high risk category of maternal risks.
Extrapolating further, the median is also much higher, age\~35. What
this prompts is that indicating that are women in the mid thirties could
possibly lie within a high risk category. This is an interesting
finding.

``` r
ggplot(data, aes(x=RiskLevel, y=Age, fill=RiskLevel)) +
  geom_half_violin() +
  geom_half_point(alpha=0.2, color='black',
                  size=0.8, position = 'dodge') +
  scale_fill_brewer(palette = 'RdPu') +
  theme_bw() +
  labs(title = 'Half-violin and Half-scatter plots
  of Age based on Risk Level Categories') +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot(data, aes(x=Age, y=RiskLevel, fill=RiskLevel)) +
  geom_density_ridges_gradient() +
    scale_fill_brewer(palette = 'RdPu') +
  theme_bw() +
    labs(title = 'Ridge plots
  of Age based on Risk Level Categories') +
  theme(plot.title = element_text(hjust = 0.5))
```

    ## Picking joint bandwidth of 3.19

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

To continue with the Age variable, I wanted to broaden my R skills by
using different visualizations the programming language offers. To
further compliment the boxplot, I wanted to use gghalves and create a
half-violin and half-scatter plot.

Now, let’s move on to a tradtional appraoch to see if there are any
correlations between the variables, excluding RiskLevel. One such
approach is a correlation matrix that conventiently describes the data,
by looking at the raw data.

``` r
#Create data without last variable that contained chr
# -c(7) corresponds to the last variable
corrplot_data <-data[,-c(7)]

#Check to see if RiskLevel is deleted
names(corrplot_data)
```

    ## [1] "Age"         "SystolicBP"  "DiastolicBP" "BloodSugar"  "BodyTemp"   
    ## [6] "HeartRate"

``` r
#Simple Corrplot
corrplot(cor(corrplot_data), 
         col=brewer.pal(n=10, name='RdBu'),
         tl.cex = 0.8)
```

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Let’s do some classification, but first let’s go into KNN concepts.

## Modeling: K-Neighrest Neighbors

![](https://static.javatpoint.com/tutorial/machine-learning/images/k-nearest-neighbor-algorithm-for-machine-learning2.png)

**K-Nearest Neighbors** is a distance algorithm. In short, it will
classify an unknown observation (i.e., a “new data point” in the picture
above), with it’s known metrics, to a category based on the proximity to
it’s nearest neighbors.[^3]

First, we set the number of K-neighbors and then the KNN model
calculates the **Euclidean Distance** between the unknown data point and
the aforementioned K-neighbors (see pic below) and classifies the
unknown data point based on majority.

![](https://static.javatpoint.com/tutorial/machine-learning/images/k-nearest-neighbor-algorithm-for-machine-learning4.png)

For example, suppose we have 2 Categories (A and B) and an unknown
observation, and we want to know what classification it would be.
Suppose we set k=5, then the model will look at the **5 nearest
neighbors** and determine the unknown observation based on the
classification of those 5 nearest neighbors. (see pic below)

![](https://static.javatpoint.com/tutorial/machine-learning/images/k-nearest-neighbor-algorithm-for-machine-learning5.png)

We will do this classification with our dataset. We will split into the
traditional 80/20 split that will split 80% of the data into the
training set and 20% of the data into the testing set. (It is important
to note that we never want to train with our testing data.)

Let’s review the structure and make a copy of the original dataset as we
move forward.

``` r
#see structure of the dataset
str(data)
```

    ## 'data.frame':    1014 obs. of  7 variables:
    ##  $ Age        : int  25 35 29 30 35 23 23 35 32 42 ...
    ##  $ SystolicBP : int  130 140 90 140 120 140 130 85 120 130 ...
    ##  $ DiastolicBP: int  80 90 70 85 60 80 70 60 90 80 ...
    ##  $ BloodSugar : num  15 13 8 7 6.1 7.01 7.01 11 6.9 18 ...
    ##  $ BodyTemp   : num  98 98 100 98 98 98 98 102 98 98 ...
    ##  $ HeartRate  : int  86 70 80 70 76 70 78 86 70 70 ...
    ##  $ RiskLevel  : chr  "high risk" "high risk" "high risk" "high risk" ...

``` r
#make a copy of the original dataset
data2 <-data
```

We see that dependent variable, RiskLevel, is a character and for
purposes of KNN, RiskLevel needs to converted into a factor.

``` r
#convert the RiskLevel column, our target, into a factor
data2$RiskLevel <- as.factor(data2$RiskLevel)
```

Now we need to normalize the 6 remaining variables, but first in order
to be more efficient, let’s write a function that will iterate over each
variable column instead of assigned values within a variable to an
object and piecing it together in another dataframe. Again, we are not
interested in the RiskLevel, so I will disregard that variable column
when I code the lapply.

``` r
#need to normalize data with a range between 0 and 1
#create a function to iterate for each variable column
data_norm <- function (x) {
  ((x-min(x))/ (max(x)- min(x)))
}

#create the normalize data
#lapply will iterate over each of the variables and store it into a 
#dataframe object except for target variable, RiskLevel
data3 <- as.data.frame(lapply(data2[,-7], data_norm))
```

Let’s compare the the summary statistics of data2 (pre-normalization) to
data3 (post-normalization).

``` r
#check to see if ALL values range from 0 to 1
#important to do this before doing KNN
summary(data2[-7])
```

    ##       Age          SystolicBP     DiastolicBP       BloodSugar    
    ##  Min.   :10.00   Min.   : 70.0   Min.   : 49.00   Min.   : 6.000  
    ##  1st Qu.:19.00   1st Qu.:100.0   1st Qu.: 65.00   1st Qu.: 6.900  
    ##  Median :26.00   Median :120.0   Median : 80.00   Median : 7.500  
    ##  Mean   :29.87   Mean   :113.2   Mean   : 76.46   Mean   : 8.726  
    ##  3rd Qu.:39.00   3rd Qu.:120.0   3rd Qu.: 90.00   3rd Qu.: 8.000  
    ##  Max.   :70.00   Max.   :160.0   Max.   :100.00   Max.   :19.000  
    ##     BodyTemp        HeartRate   
    ##  Min.   : 98.00   Min.   : 7.0  
    ##  1st Qu.: 98.00   1st Qu.:70.0  
    ##  Median : 98.00   Median :76.0  
    ##  Mean   : 98.67   Mean   :74.3  
    ##  3rd Qu.: 98.00   3rd Qu.:80.0  
    ##  Max.   :103.00   Max.   :90.0

``` r
summary(data3)
```

    ##       Age           SystolicBP      DiastolicBP       BloodSugar     
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.1500   1st Qu.:0.3333   1st Qu.:0.3137   1st Qu.:0.06923  
    ##  Median :0.2667   Median :0.5556   Median :0.6078   Median :0.11538  
    ##  Mean   :0.3312   Mean   :0.4800   Mean   :0.5384   Mean   :0.20969  
    ##  3rd Qu.:0.4833   3rd Qu.:0.5556   3rd Qu.:0.8039   3rd Qu.:0.15385  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
    ##     BodyTemp       HeartRate     
    ##  Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.000   1st Qu.:0.7590  
    ##  Median :0.000   Median :0.8313  
    ##  Mean   :0.133   Mean   :0.8109  
    ##  3rd Qu.:0.000   3rd Qu.:0.8795  
    ##  Max.   :1.000   Max.   :1.0000

data 3 is summarized and ready to go into the KNN model.

Let’s do a pre-check when splitting the data into a 80/20 split before
actually doing the split.

``` r
#create split concept
a <- .8*1014
b <- .2*1014
a+b
```

    ## [1] 1014

``` r
811+203
```

    ## [1] 1014

Great! Both the test and train dataframes have the same number of
observations as in the original dataframe.

Let’s split the data and create the model.

``` r
#create 80/20 split for testing and training set based on the rows
data3_train <- data3[1:811, ]
data3_test <- data3[812:1014, ]

#create the prediction model
data3_pred <- knn(
  data3_train,
  data3_test,
  #create training labels but labels are present in data2 and on RiskLevel,
  #which is column 7
  data2[1:811,7],
  #need to specify value of K
  #general rule of thumb is sq.rt. of 1014 is 31-ish
  k=31)

#validate pred labels with the actual labels on the RiskLevel, which is column 7
table_knn <- table(Predicted = data3_pred, Actual = data2[812:1014,7])

#view table
table_knn
```

    ##            Actual
    ## Predicted   high risk low risk mid risk
    ##   high risk        41        4        6
    ##   low risk          3       47       22
    ##   mid risk         15       27       38

The model predicted for each classification:

-   High Risk: 41 correct out of 100 = 41% correct
-   Low Risk: 47 correct out of 78 = 60% correct
-   Mid Risk: 38 correct out of 66 = 58% correct

This is not good!

Let’s calculate the miscalculation error nonetheless.

``` r
#Calculate error
1-sum(diag(table_knn))/sum(table_knn)
```

    ## [1] 0.3793103

The miscalculation is 38% meaning the **accuracy of entire KNN model is
62%**. This is not optimal.

Perhaps KNN is not the best model for classification of this dataset.

Let’s try another classification model, Decision Trees.

## Modeling: Decision Trees

Decision Trees are another way for classification. It is a tree-like
flowchart that is used to decide how to classify an observation. With
every decision there will be a “yes/no” and branch out to the next node
and will continue to do so until all the metrics are used. Thus, similar
to KNN we will use the metrics to predict the classification a person is
one of the 3 RiskLevel types.

``` r
#set seed for replcation and also no need to normalize the data
set.seed(123)

#split the data
#recall that the dependent variable needs to be a factor, not a chracter so need to use data2 because it is already converted. 
data_dt <- sample(2, nrow(data2), replace= TRUE, prob= c(0.8, 0.2))
data_dt_train <-data2[data_dt==1, ]
data_dt_test <-data2[data_dt==2, ]

#create the decision tree model
#RiskLevel is the dependent variable such that we want to predict
#recall that the "." means everything, so in this case it means all the the variables, 6 total. Using train data
tree <- ctree(RiskLevel~., data_dt_train)
```

``` r
#the output for the two codes is shown but not executed because
#the plots are not clear.
#Both show the same info, I screened shot the plot(tree) below
#print tree and it has 27 nodes
print(tree)

#plot the tree
plot(tree)
```

![Decision Tree Plot](Decision%20Tree%20Plot.png)

In RStudio, this plot renders normal, but does not in GitHub. The plot
tends to scrunch up and not give the best quality.

Thus, the Decision Tree Plot above is a screen shot and is also
available within this repository.

``` r
#use the model on the test data
tree_test_results <- predict(tree, data_dt_test)

#put results into a confusion matrix
table_tree <- table(Predicted= tree_test_results, Acutal= data_dt_test$RiskLevel)

#see the results
table_tree
```

    ##            Acutal
    ## Predicted   high risk low risk mid risk
    ##   high risk        51        5        7
    ##   low risk          2       72       30
    ##   mid risk          8        6       19

``` r
#Calculate error
1-sum(diag(table_tree))/sum(table_tree)
```

    ## [1] 0.29

In terms of HighRisk the model predicted 53/61 right. In terms of
LowRisk the model predicted 72/82 right. In terms of MidRisk, the model
predicted 13/56 right.

The miscalculation is 31% meaning the accuracy of the model is 78%.

Let’s try another plotting Decision trees another way.

``` r
#lets try another plotting format
tree2 <-rpart(RiskLevel~., data_dt_train)
rpart.plot(tree2)
```

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

## Limitations

## Conclusion

I will finish this part when the above sections are completed.

## Appendix (Random Plots)

``` r
#Scatterplot for Age and BS
#Not much of a relationship
ggplot(data, aes(Age, BloodSugar)) +
  geom_point(color='lightpink') +
  geom_smooth() +
  theme_bw()
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](Maternal-Health-Risks-Classification-KNN-and-Decision-Trees_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## Inspiration for this project

[^1]: <https://archive.ics.uci.edu/ml/datasets/Maternal+Health+Risk+Data+Set>

[^2]: Ahmed M., Kashem M.A., Rahman M., Khatun S. (2020) Review and
    Analysis of Risk Factor of Maternal Health in Remote Area Using the
    Internet of Things (IoT). In: Kasruddin Nasir A. et al. (eds)
    InECCE2019. Lecture Notes in Electrical Engineering, vol 632.
    Springer, Singapore.

[^3]: <https://www.javatpoint.com/k-nearest-neighbor-algorithm-for-machine-learning>
