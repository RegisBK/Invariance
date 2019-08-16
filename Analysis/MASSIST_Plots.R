## Code to create and visualize correlation/covariance matrices for different types of invariance
library(lavaan)
library(corrplot)
library(tidyverse)
## Start by borrowing the USNA data

## First Semester ####
## Create vector of item names
names<-c(paste0("D", rep(1:6)), paste0("S", rep(1:6)))

## All ####
## Save vector of item means
means.1st<-c(3.44, 4.19, 3.13, 3.91, 3.96, 3.49, 
             3.20, 2.48, 3.16, 3.24, 3.42, 3.05)
names(means.1st)<-names

## Save covariance matrix
## Create list of matrix values
upper<-' 1.31  0.24  0.31  0.28  0.26  0.30  0.05 -0.03  
0.01 -0.20 -0.02 -0.03  0.77  0.27  0.24  0.26  
0.34 -0.13 -0.22 -0.08 -0.20 -0.07 -0.10  1.53  
0.23  0.18  0.31  0.14  0.03  0.21 -0.14  0.16  
0.19  1.09  0.31  0.39 -0.01 -0.07 -0.07 -0.07  
0.06  0.01  0.83  0.39 -0.13 -0.21 -0.16 -0.18  
0.01 -0.12  1.26 -0.11 -0.16 -0.11 -0.21 -0.02 
-0.02  1.78  0.91  1.12  0.55  0.63  0.90  1.68  
0.82  0.62  0.50  0.78  1.80  0.56  0.64  0.91  
1.56  0.39  0.51  1.60  0.58  1.65'

## Use getCov function to turn into covariance matrix (upper corner)
cov.1st<-getCov(upper, lower=F, names=names)

## By Grade ####
## Now for individual grade groups

## Save vector of item means for each grade group
means.1st.AB<-c(3.38, 4.29, 3.14, 3.95, 4.06, 3.63,
                2.78, 2.07, 2.78, 3.01, 3.19, 2.66)
names(means.1st.AB)<-names

means.1st.C<-c(3.52, 4.06, 3.12, 3.88, 3.83, 3.30, 
               3.59, 2.80, 3.46, 3.46, 3.64, 3.44)
names(means.1st.C)<-names

means.1st.DF<-c(3.52, 3.99, 3.12, 3.77, 3.84, 3.26,
                4.21, 3.58, 4.17, 3.80, 3.96, 3.94)
names(means.1st.DF)<-names

## Combine three sets of means into single list
means.1st.group<-list(means.1st.AB, means.1st.C, means.1st.DF)

## Save covariance matrices for each grade group
## Create list of matrix values for A/B Group
upper.AB<-'1.32  0.22  0.32  0.30  0.26  0.32  0.03 -0.04
-0.01 -0.24 -0.04 -0.07  0.74  0.28  0.23  0.24  
0.31 -0.09 -0.15 -0.05 -0.22  -0.05 -0.07  1.51  
0.28  0.11  0.30  0.18  0.02  0.24 -0.19  0.07  
0.19  1.17  0.31  0.39  0.04 -0.02 -0.02 -0.03  
0.08  0.03  0.78  0.35 -0.08 -0.16 -0.08 -0.15  
0.01 -0.09  1.20 -0.06 -0.08 -0.06 -0.14  0.03  
0.01  1.77  0.75  0.98  0.52  0.63  0.71  1.37  
0.64  0.53  0.35  0.55  1.75  0.43  0.59  0.72  
1.58  0.36  0.32  1.69  0.52  1.49'


## Use getCov function to turn into covariance matrix (upper corner)
cov.1st.AB<-getCov(upper.AB, lower=F, names=names)

## Create list of matrix values for C Group
upper.C<-'1.27  0.38  0.26  0.23  0.35  0.33  0.01 -0.14 
-0.11 -0.21 -0.03 -0.15  0.73  0.25  0.21  0.30 
0.36 -0.07 -0.19  0.01 -0.17  -0.08 -0.01  1.49  
0.09  0.20  0.24  0.12  0.02  0.21 -0.07  0.21  
0.18  0.94  0.28  0.37  0.05 -0.09 -0.07 -0.13  
0.01  0.00  0.87  0.42 -0.07 -0.17 -0.17 -0.17  
0.08 -0.04  1.26  0.01 -0.20  0.00 -0.30  0.07  
0.04  1.24  0.47  0.75  0.26  0.25  0.60   1.39  
0.49  0.39  0.36  0.43  1.37  0.43  0.40  0.61  
1.41  0.25  0.53  1.35  0.36  1.35'

## Use getCov function to turn into covariance matrix (upper corner)
cov.1st.C<-getCov(upper.C, lower=F, names=names)

## Create list of matrix values for D/F Group
upper.DF<-'1.33  0.08  0.38  0.37  0.14  0.22 -0.04  0.05  
0.16 -0.15 -0.02  0.15  0.87  0.26  0.27  0.21  
0.27  0.04 -0.07  0.03  0.09  0.14  0.04  1.72         
0.30  0.46  0.48  0.01  0.06  0.09 -0.08  0.44  
0.24  1.04  0.32  0.36 -0.10 -0.01 -0.07  0.03  
0.18  0.19  0.87  0.37 -0.08 -0.09 -0.14 -0.12  
0.10 -0.08  1.36  0.06  0.24  0.00 -0.01 -0.08  
0.33  0.94  0.43  0.62  0.20  0.30  0.46  1.61  
0.37  0.35  0.34  0.68  1.14  0.32  0.35  0.58  
1.20  0.18  0.26  1.12  0.25  1.28'

## Use getCov function to turn into covariance matrix (upper corner)
cov.1st.DF<-getCov(upper.DF, lower=F, names=names)

## Combine three sets of covariance matrices into single list
cov.1st.group<-list(cov.1st.AB, cov.1st.C, cov.1st.DF)

## Second Semester

## All

## By Grade


## Plots ####



cov.1st %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="Combined Data", mar=c(0,0,1,0))
## So this is what the full data set looks like

## We know that this data some support for invariance, so we'd expect all the correlation plots to look similar
cov.1st.AB %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="AB Data", mar=c(0,0,1,0))
cov.1st.C %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="C Data", mar=c(0,0,1,0))
cov.1st.DF %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="DF Data", mar=c(0,0,1,0))

## What about data that worked better?
