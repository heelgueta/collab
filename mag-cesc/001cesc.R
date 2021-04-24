###step 1
###general setup
#rm(list = ls()) #sirve para borrar todo; usar con cuidado
options(scipen=999) #disables scientific notation
options(max.print=1000000) #enable long outputs
#install.packages("lavaan","psych","sjmisc","GPArotation")

###step 2
###load dataset
df <- read.csv("mag-cesc/cesc.csv",fileEncoding="UTF-8-BOM")
colnames(df)

#remove cases with missing data
df <- na.omit(df)

###step 3
###descriptive statistics
table(df$estab)
table(df$sexoe)
table(df$curso)
round(mean(df$edade),2);round(sd(df$edade),2)
round(mean(df$prome),2);round(sd(df$prome),2)


###step 4
#classic psychometric analyses
round(psych::alpha(df[07:10],check.keys=TRUE)$total$std.alpha,3)
round(psych::alpha(df[11:16],check.keys=TRUE)$total$std.alpha,3)
round(psych::alpha(df[17:21],check.keys=TRUE)$total$std.alpha,3)
round(psych::alpha(df[22:29],check.keys=TRUE)$total$std.alpha,3)
round(psych::alpha(df[30:33],check.keys=TRUE)$total$std.alpha,3)
round(psych::alpha(df[34:36],check.keys=TRUE)$total$std.alpha,3)
round(psych::alpha(df[37:39],check.keys=TRUE)$total$std.alpha,3)

###step 5
#cfa
#ccon =~ ccon1 + ccon2 + ccon4 + ccon5
#cemo =~ cemo1 + cemo2 + cemo3 + cemo4 + cemo5 + cemo6
#ccog =~ ccog1 + ccog2 + ccog3 + ccog4 + ccog5
#clas =~ clas1 + clas2 + clas3 + clas4 + clas5 + clas6 + clas7 + clas8
#clno =~ clno1 + clno2 + clno3 + clno4
#clnv =~ clnv1 + clnv2 + clnv3
#clpa =~ clpa1 + clpa2 + clpa3
#define model
mod <- '
prom =~ prome
ccon =~ ccon1 + ccon2 + ccon4 + ccon5
cemo =~ cemo1 + cemo2 + cemo3 + cemo4 + cemo5 + cemo6
ccog =~ ccog1 + ccog2 + ccog3 + ccog4 + ccog5
'
mod <- '
prom =~ prome
ccon =~ ccon1 + ccon2 + ccon4 + ccon5
cemo =~ cemo1 + cemo2 + cemo3 + cemo4 + cemo5 + cemo6
ccog =~ ccog1 + ccog2 + ccog3 + ccog4 + ccog5
clas =~ clas1 + clas2 + clas3 + clas4 + clas5 + clas6 + clas7 + clas8
clno =~ clno1 + clno2 + clno3 + clno4
clnv =~ clnv1 + clnv2 + clnv3
clpa =~ clpa1 + clpa2 + clpa3

'


#test
fit <- lavaan::cfa(mod, data=df,estimator="MLR")
lavaan::fitMeasures(fit, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit)

###step 6
#invariance across sexes for 5p df
fitconfig <- lavaan::cfa(mod, data=df,estimator="MLR", group="sexoe")
fitmetric <- lavaan::cfa(mod, data=df,estimator="MLR", group="sexoe", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=df,estimator="MLR", group="sexoe", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=df,estimator="MLR", group="sexoe", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)

###step 7
#invariance across curso
fitconfig <- lavaan::cfa(mod, data=df,estimator="MLR", group="curso")
fitmetric <- lavaan::cfa(mod, data=df,estimator="MLR", group="curso", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=df,estimator="MLR", group="curso", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=df,estimator="MLR", group="curso", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)


###step 8
#obtain factor scores, plot them?
lavaan::lavPredict(fit)