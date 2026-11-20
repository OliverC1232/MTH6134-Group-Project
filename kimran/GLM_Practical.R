## GLM practical 
library(MASS)
pl <- read.csv("league_players.csv")
m  <- read.csv("ashford_matches.csv")
pl$position <- factor(pl$position, levels = c("DEF", "GK", "MID", "FWD"))
m$home_away <- factor(m$home_away, levels = c("Away", "Home"))

## Player injuries (binomial GLM)
table(pl$prev_injuries, pl$injured)
full.glm <- glm(injured ~ age + minutes_played + training_load + prev_injuries + bmi + position,
                family = binomial, data = pl)
summary(full.glm)
drop1(full.glm, test = "Chisq")
inj.glm <- step(full.glm, trace = 0)
summary(inj.glm)
anova(inj.glm, full.glm, test = "Chisq")
confint.default(inj.glm)
exp(cbind(OR = coef(inj.glm), confint(inj.glm)))
inj.probit <- update(inj.glm, family = binomial(link = "probit"))
inj.cll    <- update(inj.glm, family = binomial(link = "cloglog"))
inj.array <- cbind(rbind(coef(inj.glm), coef(inj.probit), coef(inj.cll)),
                   c(deviance(inj.glm), deviance(inj.probit), deviance(inj.cll)))
colnames(inj.array)[5] <- "Residual dev."
rownames(inj.array) <- c("Logit", "Probit", "Comp log-log")
round(inj.array, 4)
nd <- data.frame(age = 25, training_load = 3, prev_injuries = c(0, 2, 4))
predict(inj.glm, newdata = nd, type = "response")
pl$fit <- fitted(inj.glm)
grp <- cut(pl$fit, quantile(pl$fit, 0:10/10), include.lowest = TRUE)
cbind(obs = tapply(pl$injured, grp, sum), exp = round(tapply(pl$fit, grp, sum), 1))

## Ashford goals scored (Poisson GLM)
plot(m$opponent_rank, m$goals_scored, pch = ifelse(m$home_away == "Home", 19, 1),
     xlab = "Opponent final rank (1 = top)", ylab = "Goals scored by Ashford")
legend("topleft", pch = c(19, 1), legend = c("Home", "Away"))
tapply(m$goals_scored, m$home_away, mean)
goals.glm <- glm(goals_scored ~ home_away + opponent_rank, family = poisson, data = m)
summary(goals.glm)
drop1(goals.glm, test = "Chisq")
goals.int <- glm(goals_scored ~ home_away * opponent_rank, family = poisson, data = m)
anova(goals.glm, goals.int, test = "Chisq")
confint.default(goals.glm)
exp(cbind(RR = coef(goals.glm), confint(goals.glm)))
pchisq(deviance(goals.glm), df.residual(goals.glm), lower.tail = FALSE)
sum(residuals(goals.glm, type = "pearson")^2) / df.residual(goals.glm)
cd <- cooks.distance(goals.glm)
sort(cd, decreasing = TRUE)[1:3]
m[which.max(cd), ]
goals.no8 <- update(goals.glm, data = m[-which.max(cd), ])
summary(goals.no8)$coef
                                   
                                   
