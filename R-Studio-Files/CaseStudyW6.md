# GDPCaseStudy
Armand Post  
June 12, 2016  

## GDP and Education Level Case Study

The purpose of this case study is to assess GDP Rank and GDP in USD using data from the worldbank.org website.  Two datasets are used and merged with the purpose of appending income group with the GDP data.  Since countries with large populations may have a large total GDP, but low GDP per capita, grouping by income group will give a better assessment of a country's relative productivity and citizens' quality of life.

## Deliverable
Since reproducibility is an important aspect of this assignment, the deliverable will be a github repository with R Markdown code, data dictionary, and the original datasets.  The code will be runable with one alteration, changing the working directory to one on the running computer's local drive.  However, there are a number of packages used which may need to be installed.

## The Code
Code chunks were given names and the code was intentionally split into chunks that perform specific functions.  This was with the purpose in mind of making references to the code easier to follow.





### Set working directory (you'll need to update this to your local working directory)

```r
setwd("/SMU/Doing Data Science/Unit 6/CaseStudy/CaseStudyWeek6")
```

### Load Libraries, install "stringr" package if needed

```r
##install.packages("stringr")
library(plyr)
library(downloader)
library(stringr) # needed to place leading zeros on GDP Rank for order by
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(scales) # needed for formatting y-axis labels to non-scientific type
```

### Download the datasets

```r
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",destfile="GDPData.csv")
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",destfile="EducationalData.csv")

cat("Data downloaded into current working directory\n")
```

```
## Data downloaded into current working directory
```

### Load the data into R

```r
GDP <- read.csv("GDPData.csv",skip=4)
Education <- read.csv("EducationalData.csv",na.strings=c("", "NA"))
```

### Assess the data.  Note that this part of the analysis is commented out since it is not neccesary for final output and was slowing down viewing of the html output.

```r
##head(GDP)
##head(Education)
##summary(GDP)
##summary(Education)
```

### Assign header names for GDP, and remove blank columns

```r
GDP <- GDP[,-3,-7]
colnames(GDP) <- c('CountryCode','GDPRank','CountryName','GDP')
GDP <- GDP[,-5,-6]
GDP <- GDP[,-5,-6]
GDP <- GDP[,-5,-6]
GDP <- GDP[,-5,-6]
GDP <- GDP[,-5,-6]
head(GDP)
```

```
##   CountryCode GDPRank    CountryName          GDP
## 1         USA       1  United States  16,244,600 
## 2         CHN       2          China   8,227,103 
## 3         JPN       3          Japan   5,959,718 
## 4         DEU       4        Germany   3,428,131 
## 5         FRA       5         France   2,612,878 
## 6         GBR       6 United Kingdom   2,471,784
```

### From the GDP Summary, it was found that 24 values for GDP are missing.  Those values are removed below.  Also remove blank rows and NAs

```r
GDP <- subset(GDP, GDP != "..")
GDP <- subset(GDP, CountryName != "Djibouti")
GDP <- subset(GDP, GDP != "")
GDP <- na.omit(GDP)
```


### Merge the Datasets and reduce to only columns needed.

```r
FullData <- merge(GDP,Education, by.x = "CountryCode", by.y = "CountryCode", all=TRUE)

GDPRecords <- GDP[,c("CountryCode","CountryName")]
EduRecords <- Education[,c("CountryCode","Short.Name")]
FullData2 <- FullData[,c("CountryCode","GDPRank","CountryName","GDP","Short.Name","Income.Group","Government.Accounting.concept","Region")]
FullData2 <- FullData2[order(FullData2$GDP),]
```

### Question 1.  Show row counts.  Note that two records were in GDP, but not Education (Andorra, South Sudan).  Additonally, 21 records were in Education, but not GDP

```r
nrow(GDPRecords)
```

```
## [1] 204
```

```r
nrow(EduRecords)
```

```
## [1] 234
```

```r
nrow(FullData2)
```

```
## [1] 235
```

### Remove the records that didn't match and non-country specific data.  Also add in a GDP rank column with leading zeros for sorting and sort by descending order.  Once removing records with no GDP rank, the record count is reduced to 190 from 235.

```r
FullData3 <- subset(FullData2, GDPRank != "")

FullData3$GDPRank2 <- str_pad(FullData3$GDPRank, width=3, side="left", pad="0")
FullData3 <- FullData3[order(FullData3$GDPRank2, decreasing = TRUE),]
nrow(FullData3)
```

```
## [1] 190
```

```r
head(FullData3)
```

```
##     CountryCode GDPRank           CountryName   GDP            Short.Name
## 205         TUV     190                Tuvalu   40                 Tuvalu
## 105         KIR     189              Kiribati  175               Kiribati
## 132         MHL     188      Marshall Islands  182       Marshall Islands
## 161         PLW     187                 Palau  228                  Palau
## 186         STP     186 São Tomé and Principe  263  São Tomé and Principe
## 68          FSM     185 Micronesia, Fed. Sts.  326             Micronesia
##            Income.Group Government.Accounting.concept              Region
## 205 Lower middle income                          <NA> East Asia & Pacific
## 105 Lower middle income                          <NA> East Asia & Pacific
## 132 Lower middle income                          <NA> East Asia & Pacific
## 161 Upper middle income                          <NA> East Asia & Pacific
## 186 Lower middle income                          <NA>  Sub-Saharan Africa
## 68  Lower middle income                          <NA> East Asia & Pacific
##     GDPRank2
## 205      190
## 105      189
## 132      188
## 161      187
## 186      186
## 68       185
```

### For Question 2, once the data is in descending order, show the 13th from lowest GDP country.
#### From the below, we can see that Grenada and St. Kitts and Nevis are tied for 13th from last.

```r
head(FullData3,13)
```

```
##     CountryCode GDPRank                    CountryName   GDP
## 205         TUV     190                         Tuvalu   40 
## 105         KIR     189                       Kiribati  175 
## 132         MHL     188               Marshall Islands  182 
## 161         PLW     187                          Palau  228 
## 186         STP     186          São Tomé and Principe  263 
## 68          FSM     185          Micronesia, Fed. Sts.  326 
## 201         TON     184                          Tonga  472 
## 51          DMA     183                       Dominica  480 
## 42          COM     182                        Comoros  596 
## 220         WSM     181                          Samoa  684 
## 213         VCT     180 St. Vincent and the Grenadines  713 
## 78          GRD     178                        Grenada  767 
## 106         KNA     178            St. Kitts and Nevis  767 
##                         Short.Name        Income.Group
## 205                         Tuvalu Lower middle income
## 105                       Kiribati Lower middle income
## 132               Marshall Islands Lower middle income
## 161                          Palau Upper middle income
## 186          São Tomé and Principe Lower middle income
## 68                      Micronesia Lower middle income
## 201                          Tonga Lower middle income
## 51                        Dominica Upper middle income
## 42                         Comoros          Low income
## 220                          Samoa Lower middle income
## 213 St. Vincent and the Grenadines Upper middle income
## 78                         Grenada Upper middle income
## 106            St. Kitts and Nevis Upper middle income
##     Government.Accounting.concept                    Region GDPRank2
## 205                          <NA>       East Asia & Pacific      190
## 105                          <NA>       East Asia & Pacific      189
## 132                          <NA>       East Asia & Pacific      188
## 161                          <NA>       East Asia & Pacific      187
## 186                          <NA>        Sub-Saharan Africa      186
## 68                           <NA>       East Asia & Pacific      185
## 201                          <NA>       East Asia & Pacific      184
## 51                           <NA> Latin America & Caribbean      183
## 42                           <NA>        Sub-Saharan Africa      182
## 220                          <NA>       East Asia & Pacific      181
## 213                  Consolidated Latin America & Caribbean      180
## 78                      Budgetary Latin America & Caribbean      178
## 106                  Consolidated Latin America & Caribbean      178
```

### For Question 3, What are the average GDP rankings for the "High income: OECD" and "High income: non OECD" groups?

```r
## Convert GDP to numeric
FullData3$GDP = gsub(pattern = ",", replacement = "", FullData3$GDP)
FullData3[, c(4)] <- sapply(FullData3[, c(4)], as.numeric)
ddply(FullData3, .(Income.Group), summarize, GDPMean = mean(GDP))
```

```
##           Income.Group    GDPMean
## 1 High income: nonOECD  104349.83
## 2    High income: OECD 1483917.13
## 3           Low income   14410.78
## 4  Lower middle income  256663.48
## 5  Upper middle income  231847.84
## 6                 <NA>   10220.00
```

```r
head(FullData3)
```

```
##     CountryCode GDPRank           CountryName GDP            Short.Name
## 205         TUV     190                Tuvalu  40                Tuvalu
## 105         KIR     189              Kiribati 175              Kiribati
## 132         MHL     188      Marshall Islands 182      Marshall Islands
## 161         PLW     187                 Palau 228                 Palau
## 186         STP     186 São Tomé and Principe 263 São Tomé and Principe
## 68          FSM     185 Micronesia, Fed. Sts. 326            Micronesia
##            Income.Group Government.Accounting.concept              Region
## 205 Lower middle income                          <NA> East Asia & Pacific
## 105 Lower middle income                          <NA> East Asia & Pacific
## 132 Lower middle income                          <NA> East Asia & Pacific
## 161 Upper middle income                          <NA> East Asia & Pacific
## 186 Lower middle income                          <NA>  Sub-Saharan Africa
## 68  Lower middle income                          <NA> East Asia & Pacific
##     GDPRank2
## 205      190
## 105      189
## 132      188
## 161      187
## 186      186
## 68       185
```
### High income: nonOECD countries have a mean GDP of $104,349 USD in millions while High income: OECD countries have a mean GDP of $1,483,917 USD in millions.

### Question 4.  Plot the GDP data for all countries

```r
scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
```

```
## <ScaleContinuousPosition>
##  Range:  
##  Limits:    0 --    1
```

```r
Graph1 <- ggplot(FullData3, aes(x = GDPRank2, y = GDP, color = Income.Group))
Graph1 + geom_point() + scale_y_continuous(labels = comma) + geom_point() + xlab("GDP Rank") + ylab("USD in millions") + ggtitle("Scatterplot of GDP by country")
```

![](CaseStudyW6_files/figure-html/Plot-1.png)<!-- -->

### Question 4.  Since the data has a few high GDP countries and many low GDP countries, the above graph is difficult to interpret due to the scale being off.  The following step performs a Log Transformation of GDP and replots.

```r
scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
```

```
## <ScaleContinuousPosition>
##  Range:  
##  Limits:    0 --    1
```

```r
Graph1 <- ggplot(FullData3, aes(x = GDPRank2, y = log(GDP), color = Income.Group))
Graph1 + geom_point() + scale_y_continuous(labels = comma) + geom_point() + xlab("GDP Rank") + ylab("log transformed USD in millions") + ggtitle("Scatterplot of GDP by country")
```

![](CaseStudyW6_files/figure-html/Log Plot-1.png)<!-- -->

### Once the data is log transformed, it takes on an S-Shape.  We can also see high income: OECD countries make up most of the high GDP Rank countries, while high income: nonOECD countries tend to lag behind their OECD counterparts.  There are some exceptions, most notably China which has a very high GDP rank despite being classified as a lower middle income nation.  Lower middle income countries typically appear around the middle and low end of GDP Rank.

### Cut the GDP rankings into quintile Groups.

```r
FullData3$Quintile <- with(FullData3, factor(findInterval(GDP, c(-Inf, quantile(GDP, probs=c(0.20, 0.40, 0.60, 0.80)), Inf)), labels=c("Q5","Q4","Q3","Q2","Q1")))
```

### Make a crosstab of Quintiles vs Income Groups

```r
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# create new variable for income group so it can be shortened for display purposes
FullData3$Income.Group2 <- NA

# shorten income group variable names to fit and create order
FullData3$Income.Group2[FullData3$Income.Group == "High income: OECD"] <- "1 HI: OECD"
FullData3$Income.Group2[FullData3$Income.Group == "High income: nonOECD"] <- "2 HI: nOECD"
FullData3$Income.Group2[FullData3$Income.Group == "Upper middle income"] <- "3 UMI"
FullData3$Income.Group2[FullData3$Income.Group == "Lower middle income"] <- "4 LMI"
FullData3$Income.Group2[FullData3$Income.Group == "Low income"] <- "5 LI"

# Frequency count
crosstab(FullData3, row.vars = "Quintile", col.vars = "Income.Group2", type = "f")
```

```
##          Income.Group2 1 HI: OECD 2 HI: nOECD 3 UMI 4 LMI 5 LI Sum
## Quintile                                                          
## Q5                              0           2     9    16   11  38
## Q4                              1           4     8     8   16  37
## Q3                              1           8     8    12    9  38
## Q2                             10           5     9    13    1  38
## Q1                             18           4    11     5    0  38
## Sum                            30          23    45    54   37 189
```

```r
# Total Percentages
crosstab(FullData3, row.vars = "Quintile", col.vars = "Income.Group2", type = "t", percentages = FALSE)
```

```
##          Income.Group2 1 HI: OECD 2 HI: nOECD 3 UMI 4 LMI 5 LI  Sum
## Quintile                                                           
## Q5                           0.00        0.01  0.05  0.08 0.06 0.20
## Q4                           0.01        0.02  0.04  0.04 0.08 0.20
## Q3                           0.01        0.04  0.04  0.06 0.05 0.20
## Q2                           0.05        0.03  0.05  0.07 0.01 0.20
## Q1                           0.10        0.02  0.06  0.03 0.00 0.20
## Sum                          0.16        0.12  0.24  0.29 0.20 1.00
```

```r
# Column percentages
crosstab(FullData3, row.vars = "Quintile", col.vars = "Income.Group2", type = "c")
```

```
##          Income.Group2 1 HI: OECD 2 HI: nOECD  3 UMI  4 LMI   5 LI
## Quintile                                                          
## Q5                           0.00        8.70  20.00  29.63  29.73
## Q4                           3.33       17.39  17.78  14.81  43.24
## Q3                           3.33       34.78  17.78  22.22  24.32
## Q2                          33.33       21.74  20.00  24.07   2.70
## Q1                          60.00       17.39  24.44   9.26   0.00
## Sum                        100.00      100.00 100.00 100.00 100.00
```

### Question 5.  From the above, there are 5 countries that are lower middle income and in the first quintile.  These countries are in order of GDP Rank: China, India, Indonesia, Thailand, and Egypt.  We can also see from the counts that one country did not have an income group classifcation and was excluded from both the above plot and tables.

### Also of note are the distributions for the income group categories.  High income: OECD countries tend to fall in the two upper quintiles while high income: non OECD have a somewhat normal distribution amongst quintiles.  Upper middle income countries have the most evenly spread distributions amongst quintiles.  Lower middle income and lower income are skewed towards the lower quartiles.


## The code below contains additional analysis beyond the five questions.  Government accounting concepts and geographical region are assessed to see if they may be indicitive of a high income country.


### Make a crosstab of Quintiles vs government accounting concepts.

```r
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# Frequency count
crosstab(FullData3, row.vars = "Quintile", col.vars = "Government.Accounting.concept", type = "f")
```

```
##          Government.Accounting.concept Budgetary Consolidated Sum
## Quintile                                                         
## Q5                                             6           11  17
## Q4                                             9           16  25
## Q3                                             9           24  33
## Q2                                             7           25  32
## Q1                                             4           33  37
## Sum                                           35          109 144
```

```r
# Total Percentages
crosstab(FullData3, row.vars = "Quintile", col.vars = "Government.Accounting.concept", type = "t", percentages = FALSE)
```

```
##          Government.Accounting.concept Budgetary Consolidated  Sum
## Quintile                                                          
## Q5                                          0.04         0.08 0.12
## Q4                                          0.06         0.11 0.17
## Q3                                          0.06         0.17 0.23
## Q2                                          0.05         0.17 0.22
## Q1                                          0.03         0.23 0.26
## Sum                                         0.24         0.76 1.00
```

```r
# Column percentages
crosstab(FullData3, row.vars = "Quintile", col.vars = "Government.Accounting.concept", type = "c")
```

```
##          Government.Accounting.concept Budgetary Consolidated
## Quintile                                                     
## Q5                                         17.14        10.09
## Q4                                         25.71        14.68
## Q3                                         25.71        22.02
## Q2                                         20.00        22.94
## Q1                                         11.43        30.28
## Sum                                       100.00       100.00
```

### From the above, 45 countries did not have a record for government accounting concept.  Consolidated accounting concepts were used 76% of the time vs 24% for budgetary.  While the sample size is somewhat small for budgetary, consolidated accounting concepts are more often used in higher GDP ranked countries which may indicate it leads to higher economic growth.

### Make crosstabs of Quintiles vs Region

```r
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# create new variable for income group so it can be shortened for display purposes
FullData3$Region2 <- NA

# shorten income group variable names to fit and create order
FullData3$Region2[FullData3$Region == "North America"] <- "1 No Am"
FullData3$Region2[FullData3$Region == "East Asia & Pacific"] <- "2 Ea As Pa"
FullData3$Region2[FullData3$Region == "Europe & Central Asia"] <- "3 Eu Ce As"
FullData3$Region2[FullData3$Region == "Middle East & North Africa"] <- "4 Mi Ea No Af"
FullData3$Region2[FullData3$Region == "Latin America & Caribbean"] <- "5 La Am Ca"
FullData3$Region2[FullData3$Region == "South Asia"] <- "6 So As"
FullData3$Region2[FullData3$Region == "Sub-Saharan Africa"] <- "7 Su Sa Af"

# Frequency count
crosstab(FullData3, row.vars = "Quintile", col.vars = "Region2", type = "f")
```

```
##          Region2 1 No Am 2 Ea As Pa 3 Eu Ce As 4 Mi Ea No Af 5 La Am Ca 6 So As 7 Su Sa Af Sum
## Quintile                                                                                      
## Q5                     0         11          0             0         10       2         15  38
## Q4                     1          4         10             1          5       0         16  37
## Q3                     0          2          9             5          9       2         11  38
## Q2                     0          3         16             8          5       3          3  38
## Q1                     2          9         15             4          6       1          1  38
## Sum                    3         29         50            18         35       8         46 189
```

```r
# Total Percentages
crosstab(FullData3, row.vars = "Quintile", col.vars = "Region2", type = "t", percentages = FALSE)
```

```
##          Region2 1 No Am 2 Ea As Pa 3 Eu Ce As 4 Mi Ea No Af 5 La Am Ca 6 So As 7 Su Sa Af  Sum
## Quintile                                                                                       
## Q5                  0.00       0.06       0.00          0.00       0.05    0.01       0.08 0.20
## Q4                  0.01       0.02       0.05          0.01       0.03    0.00       0.08 0.20
## Q3                  0.00       0.01       0.05          0.03       0.05    0.01       0.06 0.20
## Q2                  0.00       0.02       0.08          0.04       0.03    0.02       0.02 0.20
## Q1                  0.01       0.05       0.08          0.02       0.03    0.01       0.01 0.20
## Sum                 0.02       0.15       0.26          0.10       0.19    0.04       0.24 1.00
```

```r
# Column percentages
crosstab(FullData3, row.vars = "Quintile", col.vars = "Region2", type = "c")
```

```
##          Region2 1 No Am 2 Ea As Pa 3 Eu Ce As 4 Mi Ea No Af 5 La Am Ca 6 So As 7 Su Sa Af
## Quintile                                                                                  
## Q5                  0.00      37.93       0.00          0.00      28.57   25.00      32.61
## Q4                 33.33      13.79      20.00          5.56      14.29    0.00      34.78
## Q3                  0.00       6.90      18.00         27.78      25.71   25.00      23.91
## Q2                  0.00      10.34      32.00         44.44      14.29   37.50       6.52
## Q1                 66.67      31.03      30.00         22.22      17.14   12.50       2.17
## Sum               100.00     100.00     100.00        100.00     100.00  100.00     100.00
```

## From the above, all 189 countries had a region category assigned.  The data becomes somewhat thin, but some trends can be observed.  North America only has three countries, but two appear in the upper quintile.  East Asia and the South Pacific has a U shaped curve with most countries in the highest and lowest quintiles.  Europe and Central Asia is skewed towards the two upper quintiles.  The Middle East and North Africa is skewed towards the two upper quintiles, but has some countries in the lower two.  Latin America and the Caribbean has the closest to a normal distribution, but is more skewed towards the lower quintiles.  South Asia has a small sample size, but skews more towards the upper quintiles.  Sub-Saharan Africa is heavily skewed towards the lower quintiles.
