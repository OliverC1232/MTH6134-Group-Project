#Group project - Poisson regression for Ashford goals
#goals scored and conceded vs home/away and opponent rank

library(MASS)

matches <- read.csv("ashford_matches.csv")
matches$home_away <- factor(matches$home_away, levels = c("Away", "Home"))
str(matches)


#exploratory analysis
#for Poisson data the mean and variance should be roughly equal
aggregate(cbind(goals_scored, goals_conceded) ~ home_away, data = matches, FUN = mean)
aggregate(cbind(goals_scored, goals_conceded) ~ home_away, data = matches, FUN = var)
table(matches$goals_scored)
matches[matches$goals_scored == 8, ]    #the 8-0 win

#Figure 1
png("fig1_goals_vs_rank.png", width = 1600, height = 700, res = 200)
par(mfrow = c(1, 2))
cols <- ifelse(matches$home_away == "Home", "darkblue", "darkorange")
plot(jitter(matches$opponent_rank, 0.3), jitter(matches$goals_scored, 0.3), col = cols, pch = 16,
     xlab = "Opponent final rank (1 = strongest)", ylab = "Goals scored", main = "Goals scored")
legend("topleft", c("Home", "Away"), col = c("darkblue", "darkorange"), pch = 16, bty = "n")
plot(jitter(matches$opponent_rank, 0.3), jitter(matches$goals_conceded, 0.3), col = cols, pch = 16,
     xlab = "Opponent final rank (1 = strongest)", ylab = "Goals conceded", main = "Goals conceded")
dev.off()


#goals scored
#Poisson with log link, no offset needed since every match is 90 mins
GS.int <- glm(goals_scored ~ home_away * opponent_rank, family = poisson, data = matches)
anova(GS.int, test = "Chisq")    #interaction not significant, drop it

GS.glm <- glm(goals_scored ~ home_away + opponent_rank, family = poisson, data = matches)
drop1(GS.glm, test = "Chisq")    #keep both main effects

#check if rank needs a quadratic term
GS.quad <- glm(goals_scored ~ home_away + opponent_rank + I(opponent_rank^2),
               family = poisson, data = matches)
anova(GS.glm, GS.quad, test = "Chisq")

summary(GS.glm)

#goodness of fit - residual deviance vs chi-squared on its df
1 - pchisq(GS.glm$deviance, GS.glm$df.residual)

#estimate phi as the Pearson statistic over the residual df
phi <- sum(residuals(GS.glm, type = "pearson")^2) / GS.glm$df.residual
phi

#leverages above 2p/n, Cook's distance above 8/(n-2p)
n <- nrow(matches)
p <- 3
which(hatvalues(GS.glm) > 2 * p / n)
which(cooks.distance(GS.glm) > 8 / (n - 2 * p))
cooks.distance(GS.glm)[8]
rstandard(GS.glm)[abs(rstandard(GS.glm)) > 2]

#Figure 2
png("fig2_diagnostics.png", width = 1600, height = 700, res = 200)
par(mfrow = c(1, 2))
plot(fitted(GS.glm), rstandard(GS.glm), xlab = "Fitted values",
     ylab = "Standardised deviance residuals", main = "Residuals vs fitted")
abline(h = c(-2, 0, 2), lty = c(2, 1, 2))
text(fitted(GS.glm)[c(7, 8)], rstandard(GS.glm)[c(7, 8)], labels = c(7, 8), pos = 2)
plot(cooks.distance(GS.glm), type = "h", xlab = "Match number",
     ylab = "Cook's distance", main = "Cook's distance")
abline(h = 8 / (n - 2 * p), lty = 2)
dev.off()

#refit without the 8-0 match to see how much it matters
GS.no8 <- glm(goals_scored ~ home_away + opponent_rank, family = poisson,
              data = matches[matches$goals_scored < 8, ])
summary(GS.no8)
drop1(GS.no8, test = "Chisq")

#rate ratios with 95% CIs
exp(cbind(RR = coef(GS.glm), confint(GS.glm)))
exp(cbind(RR = coef(GS.no8), confint(GS.no8)))

#rate ratio from top of the table to the bottom (23 places)
exp(23 * c(coef(GS.glm)[3], confint(GS.glm)[3, ]))

#expected goals against rank 1, 12 and 24, away and home
new.data <- data.frame(home_away = factor(rep(c("Away", "Home"), 3), levels = c("Away", "Home")),
                       opponent_rank = c(1, 1, 12, 12, 24, 24))
pred <- predict(GS.glm, newdata = new.data, se.fit = TRUE)
cbind(new.data,
      expected = exp(pred$fit),
      lower = exp(pred$fit - 1.96 * pred$se.fit),
      upper = exp(pred$fit + 1.96 * pred$se.fit))


#goals conceded - same steps
GC.int <- glm(goals_conceded ~ home_away * opponent_rank, family = poisson, data = matches)
anova(GC.int, test = "Chisq")

GC.glm <- glm(goals_conceded ~ home_away + opponent_rank, family = poisson, data = matches)
drop1(GC.glm, test = "Chisq")
summary(GC.glm)    #note Wald p-value for home is 0.056 but LRT gives 0.0496

1 - pchisq(GC.glm$deviance, GC.glm$df.residual)
phi.GC <- sum(residuals(GC.glm, type = "pearson")^2) / GC.glm$df.residual
phi.GC    #less than 1 so underdispersed, Poisson SEs are conservative
which(cooks.distance(GC.glm) > 8 / (n - 2 * p))

exp(cbind(RR = coef(GC.glm), confint(GC.glm)))
exp(23 * c(coef(GC.glm)[3], confint(GC.glm)[3, ]))
