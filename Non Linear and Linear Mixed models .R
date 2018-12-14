# ---- import-packages ----
library(nlme)
library(ggplot2)
library(gridExtra)

# ---- import-data ----
HP <- read.csv("harrypotter.csv")
attach(HP)

HP$year = factor(year, 
                 ordered=TRUE, 
                 levels=c("2001", "2002", "2003", "2004","2005", "2007", "2009", "2010", "2011"))
HP <- groupedData(revenue~weeknum | year, 
                  data = HP)
# ---- lme-data-prep ----
HP$logrevenue <- log(HP$revenue)
HP$film <- as.factor(HP$film)

# ---- graph-data-1 ----
ggplot(HP, aes(weeknum, revenue)) + 
  geom_point() + facet_wrap( ~ year)

# ---- graph-data-2 ----
ggplot(HP, aes(weeknum, log(revenue))) + 
  geom_point() + facet_wrap( ~ year)

# ---- nlme-fit ----
A1 <- 1e+6; lrc1 <- -10; A2 <- 1e+6; lrc2 <- -10

params <- getInitial(revenue ~ SSbiexp(weeknum, A1, lrc1, A2, lrc2), data = HP)

A1 <- params[[1]]; lrc1 <- params[[2]] ; A2 <- params[[3]]; lrc2 <- params[[4]]

f1 <- revenue~ A1*exp(-exp(lrc1)*weeknum)+A2*exp(-exp(lrc2)*weeknum) 

fm1 <- nlme(f1,
            data = HP,
            fixed =  A1 + lrc1 + A2 + lrc2 ~1,
            random =   A1  + A2  ~ 1,
            start = c(params),control = list(maxIter = 500))

# ---- nlme-summary ----
summary(fm1)

# ---- nlme-ci ----
intervals(fm1)

# ---- nlme-plot ----
plot(augPred(fm1))



# ---- lm-anova-1 ----
fm_lm <- lm(logrevenue ~ weeknum + theaters, data = HP)
fm_lm2 <- lm(logrevenue ~ weeknum + theaters + weeknum:theaters, data = HP)
anova(fm_lm,fm_lm2) # Conclude remove interaction

# ---- lme-anova-1 ----
fm2 <- gls(logrevenue ~ weeknum+theaters, 
           method = "REML", 
           data = HP)

fm3<- lme(logrevenue~weeknum+theaters, 
          random = ~1|film, 
          method = "REML", 
          data = HP)

# Test for random effect
anova(fm2,fm3) # Conclude random effect is significant


# ---- lme-anova-2 ----
# Test for Test for random effect of slope in weeknum
ctrl <- lmeControl(opt='optim');
fm3b<- lme(logrevenue~weeknum+theaters, 
           random = ~1|film, 
           method = "REML", 
           data = HP, 
           control = ctrl)
fm4<- lme(logrevenue~weeknum+theaters, 
          random = ~1|film, 
          correlation = corAR1(form = ~weeknum|film),
          data = HP, 
          control = ctrl)
anova(fm3b,fm4)
# Conclude nested random effect is significant. need an intercept and slope


# ---- lme-summary ----
summary(fm4)

# ---- lme-ci ----
intervals(fm4)

# ---- lme-plot ----
plot(augPred(fm4))

# ---- random-effects ----
ranef(fm4)

# ---- residuals ----
residuals1 <- resid(fm1)
residuals5 <- resid(fm4)



# ---- diagnostics-nlme ----
p1 <- ggplot(HP,aes(fitted(fm1), resid(fm1, type = "normalized"))) + 
  geom_point() + 
  stat_smooth(method = "loess", se = F) + 
  geom_hline(yintercept = 0, 
             col = "red", 
             linetype = "dashed") + 
  xlab("Fitted values") + 
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Fitted Plot")

p2 <- ggplot(HP, aes(sample = resid(fm1)))+stat_qq() + stat_qq_line() + 
  xlab("Theoretical Quantiles") + 
  ylab("Sample Quantiles") + 
  ggtitle("Normal Q-Q") 

grid.arrange(p1, p2, ncol = 1)

# ---- diagnostics-lme----
p3 <- ggplot(HP,aes(fitted(fm4), resid(fm4, type = "normalized"))) + 
  geom_point() + 
  stat_smooth(method = "loess", se = F) + 
  geom_hline(yintercept = 0, 
             col = "red", 
             linetype = "dashed") + 
  xlab("Fitted values") + 
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Fitted Plot")

p4 <- ggplot(HP, aes(sample = resid(fm3b)))+stat_qq() + stat_qq_line() + 
  xlab("Theoretical Quantiles") + 
  ylab("Sample Quantiles") + 
  ggtitle("Normal Q-Q") 
  
grid.arrange(p3, p4, ncol = 1)

