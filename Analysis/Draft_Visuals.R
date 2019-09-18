## Based on feedback from Sept 6 meeting - revise invariance data blobs

library(simstandard)
library(lavaan)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(corrplot)

## Make all factor loadings equal and high (0.8)

m3x4<-'
F1 =~ 0.8*I1 + 0.8*I2 + 0.8*I3 + 0.8*I4
F2 =~ 0.8*I5 + 0.8*I6 + 0.8*I7 + 0.8*I8
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

## Are we ok that this doesn't look like "real" data in the sense that it's not categorical

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

fit.test.m3x4 <- cfa(test.m3x4,
                     data=two.group) 
summary(fit.test.m3x4, standardized = TRUE,
        fit.measures = TRUE, modindices=T) ## fit looks great, Factor correlations around 0.2

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

## Should factor covariances be equal in multigroup models? 
## I think they can be but this isn't usually checked 
## https://users.ugent.be/~yrosseel/lavaan/multiplegroup6Dec2012.pdf


## Now visualize
two.group %>% select(-group) %>%   cor() %>% 
   corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
              title="Simulated Data - Combined", mar=c(0,0,1,0))

group1 %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data - Group 1", mar=c(0,0,1,0))

group2 %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data - Group 2", mar=c(0,0,1,0))

## Loading differences

m3x4<-'
F1 =~ 0.8*I1 + 0.8*I2 + 0.8*I3 + 0.8*I4
F2 =~ 0.8*I5 + 0.8*I6 + 0.8*I7 + 0.8*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

m3x4.lowI10.grp2<-'
F1 =~ 0.8*I1 + 0.8*I2 + 0.8*I3 + 0.8*I4
F2 =~ 0.8*I5 + 0.8*I6 + 0.8*I7 + 0.8*I8
F3 =~ 0.8*I9 + 0.5*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group1.load <- sim_standardized(m3x4.highI10.grp1, n = 500, latent = F, errors = F)
group1.load$group<-1
group2.load <- sim_standardized(m3x4.lowI10.grp2, n = 500, latent = F, errors = F)
group2.load$group<-2

two.group.load<-rbind(group1.load, group2.load)

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

two.group.load %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data I10 Loading - Combined", mar=c(0,0,1,0))

group1.load %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data I10 Loading - Group 1", mar=c(0,0,1,0))

group2.load %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data I10 Loading - Group 2", mar=c(0,0,1,0))

library(reshape2)
melt.two.group.load<-two.group.load %>% melt(id="group")
melt.two.group.load$group<-melt.two.group.load$group %>% as.factor()

ggplot(melt.two.group.load, aes(x=group, y=value, fill=group))+ geom_violin() +  facet_grid(.~variable)


melt.two.group<-two.group %>% melt(id="group")
melt.two.group$group<-melt.two.group$group %>% as.factor()

ggplot(melt.two.group, aes(x=group, y=value, fill=group))+ geom_violin() +  facet_grid(.~variable)

## Intercept invariance

## Loading differences

m3x4<-'
F1 =~ 0.8*I1 + 0.8*I2 + 0.8*I3 + 0.8*I4
F2 =~ 0.8*I5 + 0.8*I6 + 0.8*I7 + 0.8*I8
F3 =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12
F1  =~  0.3*F2
F1  =~  0.2*F3
F2  =~  0.1*F3
'

group1.intcp <- sim_standardized(m3x4, n = 500, latent = F, errors = F)
group1.intcp$group<-1
group1.intcp$I3<-group1.intcp$I3+4

group2 <- sim_standardized(m3x4, n = 500, latent = F, errors = F)
group2$group<-2

two.group.intcp<-rbind(group1.intcp, group2)

test.m3x4<-'
F1 =~ I1 + I2 + I3 + I4
F2 =~ I5 + I6 + I7 + I8
F3 =~ I9 + I10 + I11 + I12
'

two.group.intcp %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data I3 Intercept - Combined", mar=c(0,0,1,0))

group1.intcp %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data I3 Intercept - Group 1", mar=c(0,0,1,0))

group2 %>% select(-group) %>%   cor() %>% 
  corrplot(., method="square", tl.col="black", diag=T, type="lower", tl.pos="d",
           title="Simulated Data I3 Intercept - Group 2", mar=c(0,0,1,0))

melt.two.group.intcp<-two.group.intcp %>% melt(id="group")
melt.two.group.intcp$group<-melt.two.group.intcp$group %>% as.factor()

ggplot(melt.two.group.intcp, aes(x=group, y=value, fill=group))+ geom_violin() +  facet_grid(.~variable)

