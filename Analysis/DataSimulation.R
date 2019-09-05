## Simulating invariance data using guide from 
## https://cran.r-project.org/web/packages/simstandard/vignettes/simstandard_tutorial.html

library(simstandard)
library(lavaan)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(corrplot)

# lavaan syntax for model
m <- "
A =~ 0.7 * A1 + 0.8 * A2 + 0.9 * A3 + 0.3 * B1
B =~ 0.7 * B1 + 0.8 * B2 + 0.9 * B3
B ~ 0.6 * A
"

# Simulate data
d <- sim_standardized(m, n = 100000)

## Fit model

test.m <- ' 
A =~ A1 + A2 + A3
B =~ B1 + B2 + B3;
' 

fit <- cfa(test.m,
           data=d) 
summary(fit, standardized = TRUE,
        fit.measures = TRUE, modindices=T) ## Looks pretty good, high RMSEA because crossloading is missing

## Can also ask only to simulate observed variables
d <- sim_standardized(m, n = 100000, latent = F, errors = F)

## Visualize like MASSIST data

d %>% select(A1:B3) %>%  cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, 
           title="Simulated Data", mar=c(0,0,1,0),
           addCoef.col = "grey")



## If I want to do a multigroup model, is it enough to just simulate multiple datasets and bind them?
m3x4<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group1 <- sim_standardized(m3x4, n = 500, latent = F, errors = F)
group1$group<-1
group2 <- sim_standardized(m3x4, n = 500, latent = F, errors = F)
group2$group<-2

two.group<-rbind(group1, group2)

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

fit.test.m3x4 <- cfa(test.m3x4,
           data=two.group) 
summary(fit.test.m3x4, standardized = TRUE,
        fit.measures = TRUE, modindices=T) ## fit looks great

## Try multigroup
fit.test.m3x4.group <- cfa(test.m3x4,
                     data=two.group, group="group") 
summary(fit.test.m3x4.group, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Fix loadings equal
fit.test.m3x4.group.load <- cfa(test.m3x4,
                           data=two.group, group="group", 
                           group.equal=c("loadings")) 
summary(fit.test.m3x4.group.load, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix intercepts equal
fit.test.m3x4.group.load.int <- cfa(test.m3x4,
                                data=two.group, group="group", 
                                group.equal=c("loadings", "intercepts")) 
summary(fit.test.m3x4.group.load.int, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix error variances equal
fit.test.m3x4.group.load.int.resid <- cfa(test.m3x4,
                                    data=two.group, group="group", 
                                    group.equal=c("loadings", "intercepts", "residuals")) 
summary(fit.test.m3x4.group.load.int.resid, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Other methods (depreciated)
library(semTools)
measurementInvariance(model=test.m3x4,
                      data=two.group, group="group")

## Now visualize

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, 
           title="Simulated Data", mar=c(0,0,1,0))

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="circle", tl.col="black", diag=T, 
           title="Simulated Data - Combined", mar=c(0,0,1,0))

group1 %>% select(-group) %>% cor() %>% 
  corrplot(., method="circle", tl.col="black", type="upper", diag=T, 
           title="Group 1", mar=c(0,0,1,0))

group2 %>% select(-group) %>% cor() %>% 
  corrplot(., method="circle", tl.col="black", type="upper", diag=T, 
           title="Group 2", mar=c(0,0,1,0))

## Non-invariant loading ####

## If I want to do a multigroup model, is it enough to just simulate multiple datasets and bind them?
m3x4.group1<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group1 <- sim_standardized(m3x4.group1, n = 500, latent = F, errors = F)
group1$group<-1

m3x4.group2<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.2*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group2 <- sim_standardized(m3x4.group2, n = 500, latent = F, errors = F)
group2$group<-2

two.group<-rbind(group1, group2)

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

fit.test.m3x4 <- cfa(test.m3x4,
                     data=two.group) 
summary(fit.test.m3x4, standardized = TRUE,
        fit.measures = TRUE, modindices=T) ## fit looks great

## Try multigroup
fit.test.m3x4.group <- cfa(test.m3x4,
                           data=two.group, group="group") 
summary(fit.test.m3x4.group, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Fix loadings equal
fit.test.m3x4.group.load <- cfa(test.m3x4,
                                data=two.group, group="group", 
                                group.equal=c("loadings")) 
summary(fit.test.m3x4.group.load, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix intercepts equal
fit.test.m3x4.group.load.int <- cfa(test.m3x4,
                                    data=two.group, group="group", 
                                    group.equal=c("loadings", "intercepts")) 
summary(fit.test.m3x4.group.load.int, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix error variances equal
fit.test.m3x4.group.load.int.resid <- cfa(test.m3x4,
                                          data=two.group, group="group", 
                                          group.equal=c("loadings", "intercepts", "residuals")) 
summary(fit.test.m3x4.group.load.int.resid, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Other methods (depreciated)
library(semTools)
measurementInvariance(model=test.m3x4,
                      data=two.group, group="group")

## Now visualize

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, 
           title="Simulated Data", mar=c(0,0,1,0))

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, 
           title="Simulated Data - Combined \nGroup I10 Lowered", mar=c(0,0,2,0))

group1 %>% select(-group) %>% cor() %>% 
  corrplot(., method="square", tl.col="black", type="lower", diag=T, 
           title="Group 1", mar=c(0,0,1,0))

group2 %>% select(-group) %>% cor() %>% 
  corrplot(., method="square", tl.col="black", type="upper", diag=T, 
           title="Group 2", mar=c(0,0,1,0))

corrplot(group1 %>% select(-group) %>% cor(), type = "lower", method="square", tl.pos = "d", tl.col="black")
corrplot(group2 %>% select(-group) %>% cor(), add = TRUE, type = "upper", method = "square",
         diag = FALSE, tl.pos = "n", cl.pos = "n", 
         title="Group 1 bottom Group 2 top", mar=c(0,0,1,0))

## Trying with mean structures (intercepts) ####

## The simstructure() function doesn't allow for 
## inclusion of intercept terms. What if I just added a constant to 
## all values of one indicator variable

m3x4.group1<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group1 <- sim_standardized(m3x4.group1, n = 500, latent = F, errors = F)
## Fit model to see where intercepts are (all around zero)
fit.test.m3x4.intercept <- cfa(test.m3x4, data=group1, meanstructure=T) 
summary(fit.test.m3x4.intercept, standardized = TRUE,
        fit.measures = TRUE, modindices=T)

group1$I5<-group1$I5+5
group1$group<-1

## And now I5 has an intercept of 5 :)
fit.test.m3x4.intercept <- cfa(test.m3x4, data=group1, meanstructure=T) 
summary(fit.test.m3x4.intercept, standardized = TRUE,
        fit.measures = TRUE, modindices=T)

## So use this method to simulate data 
m3x4.group2<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3

'

group2 <- sim_standardized(m3x4.group2, n = 500, latent = F, errors = F)
group2$group<-2

two.group<-rbind(group1, group2)

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

fit.test.m3x4 <- cfa(test.m3x4,
                     data=two.group, meanstructure=T) 
summary(fit.test.m3x4, standardized = TRUE,
        fit.measures = TRUE, modindices=T) ## fit looks great, I5 intercept is in between both groups

## Try multigroup
fit.test.m3x4.group <- cfa(test.m3x4,
                           data=two.group, group="group") 
summary(fit.test.m3x4.group, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Fix loadings equal
fit.test.m3x4.group.load <- cfa(test.m3x4,
                                data=two.group, group="group", 
                                group.equal=c("loadings")) 
summary(fit.test.m3x4.group.load, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix intercepts equal (this is very bad)
fit.test.m3x4.group.load.int <- cfa(test.m3x4,
                                    data=two.group, group="group", 
                                    group.equal=c("loadings", "intercepts")) 
summary(fit.test.m3x4.group.load.int, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix error variances equal (this is very messed up)
fit.test.m3x4.group.load.int.resid <- cfa(test.m3x4,
                                          data=two.group, group="group", 
                                          group.equal=c("loadings", "intercepts", "residuals")) 
summary(fit.test.m3x4.group.load.int.resid, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Other methods (depreciated)
library(semTools)
measurementInvariance(model=test.m3x4,
                      data=two.group, group="group")

## Now visualize

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, 
           title="Simulated Data", mar=c(0,0,1,0))

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="number", tl.col="black", diag=T, 
           title="Simulated Data - Combined \nGroup 1 I5 Intercept Raised", mar=c(0,0,2,0))

two.group %>% filter(group==1) %>% select(-group) %>% cor() %>% 
  corrplot(., method="square", tl.col="black", type="lower", diag=T, 
           title="Group 1", mar=c(0,0,1,0))

two.group %>% filter(group==2) %>% select(-group) %>% cor() %>% 
  corrplot(., method="square", tl.col="black", type="upper", diag=T, 
           title="Group 2", mar=c(0,0,1,0))

corrplot(group1 %>% select(-group) %>% cor(), type = "lower", method="number", tl.pos = "d", tl.col="black")
corrplot(group2 %>% select(-group) %>% cor(), add = TRUE, type = "upper", method = "number",
         diag = FALSE, tl.pos = "n", cl.pos = "n", 
         title="Group 1 bottom Group 2 top", mar=c(0,0,1,0))

## Plot means
library(reshape2)
melt.two.group<-two.group %>% melt(id="group")
melt.two.group$group<-melt.two.group$group %>% as.factor()

ggplot(melt.two.group, aes(x=group, y=value, fill=group))+ geom_violin() +  facet_grid(.~variable)

## What if different in intercept is smaller? ####
m3x4.group1<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group1 <- sim_standardized(m3x4.group1, n = 500, latent = F, errors = F)

group1$I5<-group1$I5+1
group1$group<-1

m3x4.group2<-'
F1 =~ 0.6*I1 + 0.6*I2 + 0.6*I3 + 0.6*I4
F2 =~ 0.7*I5 + 0.7*I6 + 0.7*I7 + 0.7*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3

'

group2 <- sim_standardized(m3x4.group2, n = 500, latent = F, errors = F)
group2$group<-2

two.group<-rbind(group1, group2)

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

fit.test.m3x4 <- cfa(test.m3x4,
                     data=two.group, meanstructure=T) 
summary(fit.test.m3x4, standardized = TRUE,
        fit.measures = TRUE, modindices=T) ## fit looks great, I5 intercept is in between both groups

## Try multigroup
fit.test.m3x4.group <- cfa(test.m3x4,
                           data=two.group, group="group") 
summary(fit.test.m3x4.group, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Fix loadings equal
fit.test.m3x4.group.load <- cfa(test.m3x4,
                                data=two.group, group="group", 
                                group.equal=c("loadings")) 
summary(fit.test.m3x4.group.load, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix intercepts equal (this is very bad)
fit.test.m3x4.group.load.int <- cfa(test.m3x4,
                                    data=two.group, group="group", 
                                    group.equal=c("loadings", "intercepts")) 
summary(fit.test.m3x4.group.load.int, standardized = TRUE,
        fit.measures = TRUE, modindices=F)
## Fix error variances equal
fit.test.m3x4.group.load.int.resid <- cfa(test.m3x4,
                                          data=two.group, group="group", 
                                          group.equal=c("loadings", "intercepts", "residuals")) 
summary(fit.test.m3x4.group.load.int.resid, standardized = TRUE,
        fit.measures = TRUE, modindices=F)

## Other methods (depreciated)
library(semTools)
measurementInvariance(model=test.m3x4,
                      data=two.group, group="group")

## Now visualize

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="color", tl.col="black", diag=T, 
           title="Simulated Data", mar=c(0,0,1,0))

two.group %>% select(-group) %>%   cor() %>% 
  corrplot(., method="number", tl.col="black", diag=T, 
           title="Simulated Data - Combined \nGroup 1 I5 Intercept Raised by 1", mar=c(0,0,2,0))

two.group %>% filter(group==1) %>% select(-group) %>% cor() %>% 
  corrplot(., method="square", tl.col="black", type="lower", diag=T, 
           title="Group 1", mar=c(0,0,1,0))

two.group %>% filter(group==2) %>% select(-group) %>% cor() %>% 
  corrplot(., method="square", tl.col="black", type="upper", diag=T, 
           title="Group 2", mar=c(0,0,1,0))

corrplot(group1 %>% select(-group) %>% cor(), type = "lower", method="number", tl.pos = "d", tl.col="black")
corrplot(group2 %>% select(-group) %>% cor(), add = TRUE, type = "upper", method = "number",
         diag = FALSE, tl.pos = "n", cl.pos = "n", 
         title="Group 1 bottom Group 2 top", mar=c(0,0,1,0))

## Plot means
library(reshape2)
melt.two.group<-two.group %>% melt(id="group")
melt.two.group$group<-melt.two.group$group %>% as.factor()

ggplot(melt.two.group, aes(x=group, y=value, fill=group))+ geom_violin() +  facet_grid(.~variable)


# Display First 6 rows
head(d) %>% 
  kable() %>% 
  kable_styling()


## Visualize covariance/correlations using homebrew function

ggcor <- function(d) {
  require(ggplot2)
  as.data.frame(d) %>%
    tibble::rownames_to_column("rowname") %>%
    tidyr::gather(colname, r, -rowname) %>%
    dplyr::mutate(rowname = forcats::fct_rev(rowname)) %>% 
    dplyr::mutate(colname = factor(colname, levels = rev(levels(rowname)))) %>% 
    ggplot(aes(colname, rowname, fill = r)) +
    geom_tile(color = "gray90") +
    geom_text(aes(
      label = formatC(
        r, 
        digits = 2, 
        format = "f") %>% 
        stringr::str_replace_all("0\\.",".") %>% 
        stringr::str_replace_all("1.00","1")), 
      color = "white", 
      fontface = "bold",
      family = "serif") +
    scale_fill_gradient2(NULL,
                         na.value = "gray20",
                         limits = c(-1.01, 1.01),
                         high = "#924552",
                         low = "#293999"
    ) +
    coord_equal() +
    scale_x_discrete(NULL,position = "top") +
    scale_y_discrete(NULL) +
    theme_light(base_family = "serif", base_size = 14) 
}

cov(d) %>% 
  ggcor

## Not sure I really understand what all this is doing
fx <-matrix(c( .9,.8,.6,rep(0,4),.6,.8,-.7),ncol=2)  
fy <- matrix(c(.6,.5,.4),ncol=1)
rownames(fx) <- c("V","Q","A","nach","Anx")
rownames(fy)<- c("gpa","Pre","MA")
Phi <-matrix( c(1,0,.7,.0,1,.7,.7,.7,1),ncol=3)
gre.gpa <- sim.structural(fx,Phi,fy)
print(gre.gpa,2) 

## Other option
population.model <- ' f1 =~ x1 + 0.8*x2 + 1.2*x3
                      f2 =~ x4 + 0.5*x5 + 1.5*x6
f3 =~ x7 + 0.1*x8 + 0.9*x9

f3 ~ 0.5*f1 + 0.6*f2
'

# generate data
set.seed(1234)
myData <- simulateData(population.model, sample.nobs=100L)

# population moments
fitted(sem(population.model))

# sample moments
round(cov(myData), 3)
round(colMeans(myData), 3)

# fit model
myModel <- ' f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f3 ~ f1 + f2 '
fit <- sem(myModel, data=myData)
summary(fit, standardized = TRUE,
        fit.measures = TRUE) 


