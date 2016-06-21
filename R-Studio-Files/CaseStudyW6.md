# GDPCaseStudy
Armand Post  
June 12, 2016  

## GDP and Education Level Case Study

The purpose of this case study is to assess GDP Rank and GDP in USD using data from the worldbank.org website.  Two datasets are used and merged with the purpose of appending income group with the GDP data.  Since countries with large populations may have a large total GDP, but low GDP per capita, grouping by income group will give a better assessment of a country's relative productivity and citezens' quality of life.

### Deliverable
Since reproducibility is an important aspect of this assignment, the deliverable will be a github repository with R Markdown code, data dictionary, and the original datasets.  The code will be runable with one alteration, changing the working directory to one on the running computer's local drive.  However, there are a number of packages used which may need to be installed.

## The Code
Code chunks were given names and the code was intentionally split into chunks that perform specific functions.  This was with the purpose in mind of making references to the code easier to follow.





## Set working directory (you'll need to update this to your local working directory)

```r
setwd("/SMU/Doing Data Science/Unit 6/CaseStudy/CaseStudyWeek6")
```

## Load Libraries, install "stringr" package if needed

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

## Download the datasets

```r
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",destfile="GDPData.csv")
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",destfile="EducationalData.csv")

cat("Data downloaded into current working directory\n")
```

```
## Data downloaded into current working directory
```

## Load the data into R

```r
GDP <- read.csv("GDPData.csv",skip=4)
Education <- read.csv("EducationalData.csv",na.strings=c("", "NA"))
```

## Assess the data

```r
##head(GDP)
##head(Education)
summary(GDP)
```

```
##        X                        X.1        X.2         
##         : 98                      :132   Mode:logical  
##  ABW    :  1   178                :  2   NA's:326      
##  ADO    :  1   .. Not available.  :  1                 
##  AFG    :  1   1                  :  1                 
##  AGO    :  1   10                 :  1                 
##  ALB    :  1   100                :  1                 
##  (Other):223   (Other)            :188                 
##                           X.3               X.4      X.5    
##                             : 98              : 99    :320  
##    East Asia & Pacific      :  1   ..         : 23   a:  1  
##    Euro area                :  1    767       :  2   b:  1  
##    Europe & Central Asia    :  1    1,008     :  1   c:  1  
##    Latin America & Caribbean:  1    1,129     :  1   d:  1  
##    Lower middle income      :  1    1,129,598 :  1   e:  1  
##  (Other)                    :223   (Other)    :199   f:  1  
##    X.6            X.7            X.8            X.9         
##  Mode:logical   Mode:logical   Mode:logical   Mode:logical  
##  NA's:326       NA's:326       NA's:326       NA's:326      
##                                                             
##                                                             
##                                                             
##                                                             
## 
```

```r
summary(Education)
```

```
##   CountryCode                   Long.Name                 Income.Group
##  ABW    :  1   American Samoa        :  1   High income: nonOECD:37   
##  ADO    :  1   Antigua and Barbuda   :  1   High income: OECD   :30   
##  AFG    :  1   Arab Republic of Egypt:  1   Low income          :40   
##  AGO    :  1   Argentine Republic    :  1   Lower middle income :56   
##  ALB    :  1   Aruba                 :  1   Upper middle income :47   
##  ARE    :  1   Barbados              :  1   NA's                :24   
##  (Other):228   (Other)               :228                             
##                         Region   Lending.category    Other.groups
##  Europe & Central Asia     :57   Blend:16         Euro area: 16  
##  Sub-Saharan Africa        :47   IBRD :63         HIPC     : 40  
##  Latin America & Caribbean :38   IDA  :63         NA's     :178  
##  East Asia & Pacific       :36   NA's :92                        
##  Middle East & North Africa:21                                   
##  (Other)                   :11                                   
##  NA's                      :24                                   
##                Currency.Unit Latest.population.census
##  Euro                 : 20   2001   :46              
##  CFA franc            : 14   2000   :35              
##  U.S. dollar          : 13   2002   :21              
##  East Caribbean dollar:  6   2006   :15              
##  Australian dollar    :  3   2004   :12              
##  (Other)              :154   (Other):79              
##  NA's                 : 24   NA's   :26              
##  Latest.household.survey
##  MICS, 2006: 23         
##  MICS, 2000:  8         
##  DHS, 2005 :  7         
##  DHS, 2007 :  6         
##  MICS, 2005:  6         
##  (Other)   : 72         
##  NA's      :112         
##                                                                      Special.Notes
##  Fiscal year end: June 30; reporting period for national accounts data: FY. :  7  
##  Fiscal year end: March 31; reporting period for national accounts data: CY.:  7  
##  Fiscal year end: June 30; reporting period for national accounts data: CY. :  6  
##  Fiscal year end: March 31; reporting period for national accounts data: FY.:  3  
##  Fiscal year end: March 20; reporting period for national accounts data: FY.:  2  
##  (Other)                                                                    : 66  
##  NA's                                                                       :143  
##  National.accounts.base.year National.accounts.reference.year
##  2000   :42                  Min.   :1987                    
##  1990   :16                  1st Qu.:1996                    
##  1995   :10                  Median :2000                    
##  1994   : 7                  Mean   :1999                    
##  1997   : 6                  3rd Qu.:2002                    
##  (Other):85                  Max.   :2007                    
##  NA's   :68                  NA's   :197                     
##  System.of.National.Accounts SNA.price.valuation
##  Min.   :1993                VAB :158           
##  1st Qu.:1993                VAP : 36           
##  Median :1993                NA's: 40           
##  Mean   :1993                                   
##  3rd Qu.:1993                                   
##  Max.   :1993                                   
##  NA's   :149                                    
##  Alternative.conversion.factor PPP.survey.year
##  1990-95:  8                   Min.   :2005   
##  1987-95:  5                   1st Qu.:2005   
##  1993   :  3                   Median :2005   
##  1991   :  2                   Mean   :2005   
##  1992-95:  2                   3rd Qu.:2005   
##  (Other): 27                   Max.   :2005   
##  NA's   :187                   NA's   :89     
##  Balance.of.Payments.Manual.in.use External.debt.Reporting.status
##  BPM4:  9                          Actual     : 93               
##  BPM5:162                          Estimate   : 13               
##  NA's: 63                          Preliminary: 22               
##                                    NA's       :106               
##                                                                  
##                                                                  
##                                                                  
##  System.of.trade Government.Accounting.concept
##  General:111     Budgetary   : 36             
##  Special: 72     Consolidated:111             
##  NA's   : 51     NA's        : 87             
##                                               
##                                               
##                                               
##                                               
##  IMF.data.dissemination.standard
##  GDDS:95                        
##  SDDS:68                        
##  NA's:71                        
##                                 
##                                 
##                                 
##                                 
##  Source.of.most.recent.Income.and.expenditure.data
##  IHS, 2007  : 10                                  
##  IHS, 2000  :  9                                  
##  ES/BS, 2005:  6                                  
##  IHS, 2006  :  6                                  
##  ES/BS, 2004:  5                                  
##  (Other)    :105                                  
##  NA's       : 93                                  
##  Vital.registration.complete Latest.agricultural.census
##  Yes :103                    2001     : 14             
##  NA's:131                    2000     : 12             
##                              1999-2000: 11             
##                              2002     :  8             
##                              2003     :  8             
##                              (Other)  : 81             
##                              NA's     :100             
##  Latest.industrial.data Latest.trade.data Latest.water.withdrawal.data
##  Min.   :1995           Min.   :1975      Min.   :1990                
##  1st Qu.:2002           1st Qu.:2007      1st Qu.:2000                
##  Median :2004           Median :2008      Median :2000                
##  Mean   :2003           Mean   :2007      Mean   :2001                
##  3rd Qu.:2005           3rd Qu.:2008      3rd Qu.:2000                
##  Max.   :2006           Max.   :2008      Max.   :2006                
##  NA's   :139            NA's   :46        NA's   :82                  
##  X2.alpha.code   WB.2.code            Table.Name           Short.Name 
##  AD     :  1   AD     :  1   Afghanistan   :  1   Afghanistan   :  1  
##  AE     :  1   AE     :  1   Albania       :  1   Albania       :  1  
##  AF     :  1   AF     :  1   Algeria       :  1   Algeria       :  1  
##  AG     :  1   AG     :  1   American Samoa:  1   American Samoa:  1  
##  AL     :  1   AL     :  1   Andorra       :  1   Andorra       :  1  
##  (Other):202   (Other):203   Angola        :  1   Angola        :  1  
##  NA's   : 27   NA's   : 26   (Other)       :228   (Other)       :228
```

## Assign header names for GDP, and remove blank columns

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

## From the GDP Summary, it was found that 24 values for GDP are missing.  Those values are removed below.  Also remove blank rows and NAs

```r
GDP <- subset(GDP, GDP != "..")
GDP <- subset(GDP, CountryName != "Djibouti")
GDP <- subset(GDP, GDP != "")
GDP <- na.omit(GDP)
```


## Merge the Datasets and reduce to only columns needed.

```r
FullData <- merge(GDP,Education, by.x = "CountryCode", by.y = "CountryCode", all=TRUE)

GDPRecords <- GDP[,c("CountryCode","CountryName")]
EduRecords <- Education[,c("CountryCode","Short.Name")]
FullData2 <- FullData[,c("CountryCode","GDPRank","CountryName","GDP","Short.Name","Income.Group")]
FullData2 <- FullData2[order(FullData2$GDP),]
```

## Question 1.  Show row counts.  Note that two records were in GDP, but not Education (Andorra, South Sudan).  Additonally, 21 records were in Education, but not GDP

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

## Remove the records that didn't match and non country specific data.  Also add in a GDP rank column with leading zeros for sorting and sort by descending order.

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
##            Income.Group GDPRank2
## 205 Lower middle income      190
## 105 Lower middle income      189
## 132 Lower middle income      188
## 161 Upper middle income      187
## 186 Lower middle income      186
## 68  Lower middle income      185
```

## For Question 2, once the data is in descending order, show the 13th from lowest GDP country.
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
##                         Short.Name        Income.Group GDPRank2
## 205                         Tuvalu Lower middle income      190
## 105                       Kiribati Lower middle income      189
## 132               Marshall Islands Lower middle income      188
## 161                          Palau Upper middle income      187
## 186          São Tomé and Principe Lower middle income      186
## 68                      Micronesia Lower middle income      185
## 201                          Tonga Lower middle income      184
## 51                        Dominica Upper middle income      183
## 42                         Comoros          Low income      182
## 220                          Samoa Lower middle income      181
## 213 St. Vincent and the Grenadines Upper middle income      180
## 78                         Grenada Upper middle income      178
## 106            St. Kitts and Nevis Upper middle income      178
```

## For Question 3, What are the average GDP rankings for the "High income: OECD" and "High income: non OECD" groups?

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
##            Income.Group GDPRank2
## 205 Lower middle income      190
## 105 Lower middle income      189
## 132 Lower middle income      188
## 161 Upper middle income      187
## 186 Lower middle income      186
## 68  Lower middle income      185
```
### High income: nonOECD countries have a mean GDP of $104,349 while High income: OECD countries have a mean income of $1,483,917

## Question 4.  Plot the GDP data for all countries

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
Graph1 + geom_point() + scale_y_continuous(labels = comma) + geom_point()
```

![](CaseStudyW6_files/figure-html/Plot-1.png)<!-- -->

## Question 4.  Since the data has a few high GDP countries and many low GDP countries, the above graph is difficult to interpret due to the scale being off.  The following step performs a Log Transformation of GDP and replots.

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
Graph1 + geom_point() + scale_y_continuous(labels = comma) + geom_point()
```

![](CaseStudyW6_files/figure-html/Log Plot-1.png)<!-- -->

### Once the data is log transformed, it takes on an S-Shape.  We can also see high income: OECD countries make up most of the high GDP Rank countries, while high income: nonOECD countries tend to lag behind their OECD counterparts.  There are some exceptions, most notably China which has a very high GDP rank despite being classified as a lower middle income nation.  Lower middle income countries typically appear around the middle and low end of GDP Rank.

## Cut the GDP rankings into quintile Groups.

```r
FullData3$Quintile <- with(FullData3, factor(findInterval(GDP, c(-Inf, quantile(GDP, probs=c(0.20, 0.40, 0.60, 0.80)), Inf)), labels=c("Q5","Q4","Q3","Q2","Q1")))
```

## Make a crosstab of Quintiles vs Income Groups

```r
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

FullData3$Income.Group2 <- NA

# shorten income group variable names to fit and create order
FullData3$Income.Group2[FullData3$Income.Group == "High income: OECD"] <- "1 HI: OECD"
FullData3$Income.Group2[FullData3$Income.Group == "High income: nonOECD"] <- "2 HI: nOECD"
FullData3$Income.Group2[FullData3$Income.Group == "Upper middle income"] <- "3 UMI"
FullData3$Income.Group2[FullData3$Income.Group == "Lower middle income"] <- "4 LMI"
FullData3$Income.Group2[FullData3$Income.Group == "Low income"] <- "5 LI"
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
##            Income.Group GDPRank2 Quintile Income.Group2
## 205 Lower middle income      190       Q5         4 LMI
## 105 Lower middle income      189       Q5         4 LMI
## 132 Lower middle income      188       Q5         4 LMI
## 161 Upper middle income      187       Q5         3 UMI
## 186 Lower middle income      186       Q5         4 LMI
## 68  Lower middle income      185       Q5         4 LMI
```

```r
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
# Frequency count
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

## Question 5.  From the above, there are 5 countries that are lower middle income and in the first quintile.
## Also of note are the distributions for the income group categories.  High income: OECD countries tend to fall in the two upper quintiles, while high income: non OECD and Upper Middle Income countries are spread somewhat evenly amongst quintiles.  Lower middle income and Lower income are skewed towards tht lower quartiles.
