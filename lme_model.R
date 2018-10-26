library(lme4)
library(merTools)
library(MuMIn)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(data.table)
library(tibble)
library(dplyr)
library(tidyr)
library(plyr)
library(sjPlot)
library(corrplot)



# read data
df_lme <- read.csv("/Users/Maime/Dropbox/Me/Insight/Shleep/lme_nodrop.csv")

# create df_lme_wide
assess_wide <- read.csv("/Users/Maime/Dropbox/Me/Insight/Shleep/users_wide.csv")
unique_users <- unique(df_lme$user_id)
df_lme_wide <- assess_wide[assess_wide$user_id %in% unique_users, ]

# look at hours against days in app
head(df_lme)
plot(jitter(df_lme$hours), log(df_lme$days_since))

#---------------------------- model ----------------------------#
mod <- lmer(hours ~ log(days_since_last) + log(days_since) + age + factor(weekday) +
              factor(gender_female) + (1 + log(days_since_last) + log(days_since) | user_id), 
            data = df_lme)

summary(mod)

# interpret in terms of minutes and doubling time
mean_minutes <- function(betacoef, multiplier){
  value <- 60*betacoef * log(multiplier)
  return(value)
}
mean_minutes(-6.725e-02, 2)
mean_minutes(2.616e-01, 2)

# 95% CI
conf_minutes <- function(betacoef, stderror, multiplier){
  value <- 60*(betacoef * log(multiplier) + 1.96*c(-1, 1)*stderror)
  return(value)
}
conf_minutes(-6.725e-02, 1.471e-02, 2)
conf_minutes(2.616e-01, 1.260e-02, 2)


##### get p value using gaussian approximation
coefs <- data.frame(coef(summary(mod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# get r squared
r.squaredGLMM(mod)

# get RMSE
RMSE.merMod(mod)

# get fitted values
pred_lme = predict(mod)
plot(jitter(df_lme$hours), pred_lme)

#---------------------------- plots ----------------------------#

# correlation plot
corrs <- cor(df_lme[, c('days_since', 'days_since_last', 'age', 'weekday', 'gender_female')])
corrs <- cor(data.frame(log(df_lme$days_since), log(df_lme$days_since_last), df_lme$age,
                        df_lme$weekday, df_lme$gender_female))
colnames(corrs) <- c('Days in app', 'Days between', 'Age', 'Weekday', 'Gender: F')
corrplot.mixed(corrs, lower.col = 'black', number.cex = 1.5)

# PLOT predicted vs actual with marginal histograms
hist_top <- ggplot()+geom_histogram(aes(df_lme$hours), fill = 'palevioletred3', 
                                    alpha = .5, color = 'palevioletred3') + xlab('Actual hours')
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())
scatter <- ggplot() + geom_point(aes(df_lme$hours, pred_lme), alpha = .5, colour = 'palevioletred3') + 
  geom_smooth(aes(df_lme$hours, pred_lme), method = "lm", colour = 'maroon4', alpha = .5) +
  xlab('Actual hours') + ylab('Predicted hours')
hist_right <- ggplot()+geom_histogram(aes(pred_lme), fill = 'palevioletred3', alpha = .5,
                                      colour = 'palevioletred3')+ coord_flip() + xlab('Predicted hours')
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

### look at individual effects
user_coefs <- coef(mod)$user_id
dim(user_coefs)
# make user_id column from rownames
setDT(user_coefs, keep.rownames = TRUE)[]
colnames(user_coefs)[1] <- 'user_id'
user_coefs$user_id <- as.numeric(user_coefs$user_id)
user_coefs <- merge(user_coefs, df_lme_wide, by = 'user_id')


#----------------------- correlations between assess and random effects -----------------------#
tmp <- user_coefs[, -c('user_id', '(Intercept)', 'log(days_since)', 'age.x', 'factor(weekday)1')]
corrs <- as.data.frame(cor(tmp[,-1], tmp[,1]))
order.corrs <- order(corrs$`log(days_since_last)`)
corrs['question'] <- rownames(corrs)
corrs[order.corrs,]


#---------------------------- model diagnostics ----------------------------#
# check for constant variance
plot(mod)
ggplot(data.frame(fittedvals=predict(modnew), 
                  pearson=residuals(modnew, type="pearson")),
       aes(x=fittedvals, y=pearson)) + geom_jitter(colour = 'palevioletred', width = .5, shape = 1) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'maroon4', size = 1) +
  xlab('Fitted values') + ylab('Pearson residuals') +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"))

# qq plot for normality
qqnorm(residuals(modnew, type = 'working'))
qqline(residuals(modnew))
sjp.glmer(mod, type = "re.qq")

# histogram for normality
ggplot()+geom_histogram(aes(residuals(mod, type = 'pearson')), 
                        fill = 'palevioletred3', alpha = .5, 
                        color = 'palevioletred3') + xlab('Residuals') +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"))


#----------------------- plot for days in tracker and days between effects -----------------------#
multiplier <- seq(1, 20, by = 1)
#minutes <- mean_minutes(2.616e-01, multiplier)
minutes <- mean_minutes(-6.725e-02, multiplier)
tmp <- data.frame(multiplier = multiplier, 
                  minute_increase = minutes)
ggplot(data=tmp, aes(x=multiplier, y=minute_increase)) +
  geom_line(color = 'palevioletred', size = 3)+
  geom_point(color = 'maroon4', size = 3) + ylab('Sleep decrease (min)') + xlab('Multiplier') +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold")) +
  scale_x_continuous(breaks = multiplier)

