
tail(states)
str(states)
# create summary of variables for expense and csat columns
sts.ex.sat <- subset(states, select= c("expense", "csat"))
summary(sts.ex.sat)
#correlation between expense and csat
cor(sts.ex.sat)
# plot correlation expense and csat
qplot(expense,csat, data=states, geom="line")
plot(sts.ex.sat)
# Fit regression model
sat.mod <- lm(csat ~ expense, data=states)
summary(sat.mod)
#negative association expense and csat-- add in percent variable
summary(lm(csat ~ expense + percent, data= states))
class(sat.mod)
names(sat.mod)
confint(sat.mod)
# Congressional voting patterns impact on sat scores above expense
sat.voting.mod <- lm(csat~ expense + house + senate, data= na.omit(states))
summary(sat.voting.mod)
sat.mod <- update(sat.mod, data=na.omit(states))
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))
# Model interactions --does expense association SAT scores and expense depend on median income?
sat.mod.percent <- lm(csat ~ expense*income, data=states)
coef(summary(sat.mod.percent))
# SAT scores by region
str(states$region)
states$region <- factor(states$region)
sat.region <- lm(csat ~ region, data=states)
coef(summary(sat.region))
anova(sat.region)
plot(sat.region)

