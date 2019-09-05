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

## Second Semester ####

## All ####
means.2nd<-c(3.43,
             4.02,
             3.01,
             3.93,
             3.84,
             3.41,
             3.39,
             2.96,
             3.20,
             3.50,
             3.42,
             3.19)
names(means.2nd)<-names

upper.2<-'1.29	0.36	0.34	0.24	0.32	0.33	-0.07	0.00	-0.07	-0.10	-0.02	-0.02
0.96	0.27	0.29	0.39	0.37	-0.23	-0.19	-0.13	-0.23	-0.03	-0.18
1.58	0.21	0.26	0.41	-0.07	-0.02	0.01	-0.13	0.14	0.02
1.00	0.41	0.47	-0.14	-0.10	-0.07	-0.06	0.05	-0.07
0.85	0.43	-0.11	-0.15	-0.12	-0.13	0.01	-0.12
1.27	-0.18	-0.13	-0.12	-0.23	0.03	-0.06
1.81	1.04	1.27	0.60	0.55	1.00
1.86	0.94	0.56	0.55	0.93
1.86	0.54	0.65	1.04
1.47	0.22	0.48
1.54	0.63
1.66'


## Use getCov function to turn into covariance matrix (upper corner)
cov.2nd<-getCov(upper.2, lower=F, names=names)

## By Grade ####
## Save vector of item means for each grade group
means.2nd.AB<-c(3.42, 4.15, 3.05, 4.08, 3.97, 3.59,
                2.80, 2.31, 2.65, 3.16, 3.15, 2.67)
names(means.2nd.AB)<-names

means.2nd.C<-c(3.43, 3.91, 2.93, 3.79, 3.73, 3.26, 
               3.80, 3.40, 3.50, 3.75, 3.59, 3.53)
names(means.2nd.C)<-names

means.2nd.DF<-c(3.48, 3.84, 3.03, 3.77, 3.69, 3.20,
                4.24, 3.89, 4.14, 3.95, 3.82, 4.00)
names(means.2nd.DF)<-names

## Combine three sets of means into single list
means.2nd.group<-list(means.2nd.AB, means.2nd.C, means.2nd.DF)

## Save covariance matrices for each grade group
## Create list of matrix values for A/B Group
upper.2.AB<-'1.40	0.44	0.39	0.25	0.41	0.39	-0.23	-0.11	-0.16	-0.13	-0.06	-0.21
	0.94	0.28	0.32	0.40	0.38	-0.26	-0.17	-0.17	-0.27	-0.03	-0.19
1.56	0.27	0.30	0.41	-0.03	-0.01	0.02	-0.12	0.12	0.05
0.89	0.31	0.43	-0.17	-0.05	-0.06	-0.06	0.03	-0.14
0.88	0.45	-0.11	-0.14	-0.13	-0.20	0.01	-0.21
1.21	-0.13	0.01	-0.02	-0.18	0.10	-0.04
1.81	0.76	1.26	0.53	0.55	0.90
1.49	0.70	0.32	0.45	0.71
1.84	0.45	0.61	0.98
1.56	0.20	0.33
1.67	0.57
1.56
'


## Use getCov function to turn into covariance matrix (upper corner)
cov.2nd.AB<-getCov(upper.2.AB, lower=F, names=names)

## Create list of matrix values for C Group
upper.2.C<-'1.20	0.31	0.25	0.16	0.17	0.22	0.06	0.09	0.01	-0.04	-0.02	0.13
	0.89	0.26	0.23	0.33	0.31	-0.04	-0.05	0.06	-0.15	0.04	-0.01
1.66	0.10	0.22	0.35	-0.03	0.00	0.04	-0.06	0.21	0.03
1.03	0.44	0.40	0.03	0.08	0.08	0.04	0.14	0.15
0.80	0.35	0.02	0.00	0.02	0.02	0.11	0.11
1.19	-0.03	-0.02	0.04	-0.16	0.07	0.07
1.28	0.65	0.73	0.29	0.28	0.53
1.46	0.53	0.39	0.40	0.56
1.38	0.26	0.35	0.56
1.21	0.08	0.31
1.34	0.42
1.29
'

## Use getCov function to turn into covariance matrix (upper corner)
cov.2nd.C<-getCov(upper.2.C, lower=F, names=names)

## Create list of matrix values for D/F Group
upper.2.DF<-'1.21	0.27	0.39	0.42	0.40	0.41	0.06	0.06	-0.07	-0.15	0.10	0.16
	1.13	0.24	0.23	0.38	0.32	-0.11	-0.03	0.04	0.00	0.07	-0.11
1.50	0.20	0.24	0.48	-0.14	0.02	0.00	-0.24	0.09	-0.02
1.14	0.54	0.60	0.07	-0.08	0.02	-0.01	0.14	0.11
0.81	0.43	0.08	-0.03	0.02	-0.05	0.01	0.03
1.48	-0.05	-0.08	-0.18	-0.19	0.01	0.13
0.90	0.41	0.42	0.31	0.17	0.41
1.35	0.36	0.37	0.12	0.31
1.01	0.27	0.50	0.44
1.13	0.04	0.21
1.16	0.41
1.11
'

## Use getCov function to turn into covariance matrix (upper corner)
cov.2nd.DF<-getCov(upper.2.DF, lower=F, names=names)

## Combine three sets of covariance matrices into single list
cov.2nd.group<-list(cov.2nd.AB, cov.2nd.C, cov.2nd.DF)

## Plots ####

## Full
cov.1st %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="Combined Data", mar=c(0,0,1,0))

## By groups
## We know that this data some support for invariance, so we'd expect all the correlation plots to look similar
cov.1st.AB %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="AB Data", mar=c(0,0,1,0))
cov.1st.C %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="C Data", mar=c(0,0,1,0))
cov.1st.DF %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="DF Data", mar=c(0,0,1,0))

## What about data that worked better?

cov.2nd %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="Combined Data 2nd Semester", mar=c(0,0,1,0))


## We know that this data some support for invariance, so we'd expect all the correlation plots to look similar
cov.2nd.AB %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="AB Data 2nd Semester", mar=c(0,0,1,0))
cov.2nd.C %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="C Data 2nd Semester", mar=c(0,0,1,0))
cov.2nd.DF %>% cov2cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="DF Data 2nd Semester", mar=c(0,0,1,0))

## Use built-in data where strong invariance doesn't hold

data("HolzingerSwineford1939")
str(HolzingerSwineford1939)

HolzingerSwineford1939 %>% select(starts_with("x")) %>% cor() %>% corrplot(., method="color", tl.col="black", diag=T, title="Holzinger Data", mar=c(0,0,1,0))

HolzingerSwineford1939 %>% filter(school=="Grant-White") %>% 
  select(starts_with("x")) %>% cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, title="Holzinger Data - Grant School", mar=c(0,0,1,0))

HolzingerSwineford1939 %>% filter(school=="Pasteur") %>% 
  select(starts_with("x")) %>% cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, title="Holzinger Data - Pasteur School", mar=c(0,0,1,0))
     
library(reshape2)
library(ggridges)
library(ggpubr)
melt.Holz<-HolzingerSwineford1939 %>% select(school, starts_with("x")) %>% melt()                         
ggplot(melt.Holz, aes(x=value, y=school)) + geom_density_ridges() + facet_grid(.~variable)

ggplot(melt.Holz, aes(y=value, fill=school))  + geom_boxplot() + facet_grid(.~variable) 

ggboxplot(melt.Holz, x = "school", y="value", facet.by = "variable", add=c("jitter"), alpha=0.5)

ggplot(melt.Holz, aes(x=school, y=value, fill=school)) + geom_violin() + facet_grid(.~variable) + theme(axis.text.x=element_blank())


## So we know the breakdown is when intercepts are constrained across schools
model <- ' visual =~ x1 + x2 + x3;
textual =~ x4 + x5 + x6; 
speed =~ x7 + x8 + x9 ' 

fit <- cfa(model,
           data=HolzingerSwineford1939) 
summary(fit, standardized = TRUE,
        fit.measures = TRUE) 

config <- cfa(model,
              data=HolzingerSwineford1939,
              group="school") 
weak <- cfa(model,
              data=HolzingerSwineford1939,
              group="school",
              group.equal="loadings")
strong<- cfa(model,
               data=HolzingerSwineford1939,
               group="school", group.equal =
                 c("loadings", "intercepts"))
strict<- cfa(model,
               data=HolzingerSwineford1939,
               group="school", group.equal =
                 c("loadings", "intercepts",
                   "residuals"))
anova(config, weak, strong, strict) 

library(semTools)
measurementInvariance(model=model, data=HolzingerSwineford1939,
                      group="school")

## What about looking at it by gender?

config.MF <- cfa(model,
              data=HolzingerSwineford1939,
              group="sex") 
weak.MF <- cfa(model,
            data=HolzingerSwineford1939,
            group="sex",
            group.equal="loadings")
strong.MF<- cfa(model,
             data=HolzingerSwineford1939,
             group="sex", group.equal =
               c("loadings", "intercepts"))
strict.MF<- cfa(model,
             data=HolzingerSwineford1939,
             group="sex", group.equal =
               c("loadings", "intercepts",
                 "residuals"))
anova(config.MF, weak.MF, strong.MF, strict.MF) 

library(semTools)
measurementInvariance(model=model, data=HolzingerSwineford1939,
                      group="sex")

## This doesn't even meet weak invariance
HolzingerSwineford1939 %>% filter(sex==1) %>% 
  select(starts_with("x")) %>% cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, title="Holzinger Data - Gender 1", mar=c(0,0,1,0))

HolzingerSwineford1939 %>% filter(sex==2) %>% 
  select(starts_with("x")) %>% cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, title="Holzinger Data - Gender 2", mar=c(0,0,1,0))

melt.Holz.MF<-HolzingerSwineford1939 %>% select(sex, starts_with("x")) %>% melt()
ggplot(melt.Holz.MF, aes(x=sex, y=value, fill=sex)) + geom_violin() + facet_grid(.~variable) + theme(axis.text.x=element_blank())

