## Diagnostic plots for the two GLMs (base R only)
pl <- read.csv("league_players.csv")
m  <- read.csv("ashford_matches.csv")
m$home_away <- factor(m$home_away, levels = c("Away", "Home"))
inj.glm   <- glm(injured ~ age + training_load + prev_injuries, family = binomial, data = pl)
goals.glm <- glm(goals_scored ~ home_away + opponent_rank, family = poisson, data = m)

ink <- "grey20"; mute <- "grey55"; hl <- "#C0392B"; pt <- "#2C6FB7"
par_set <- function() par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1), las = 1,
                          col.axis = ink, col.lab = ink, fg = mute, cex.main = 1.1, font.main = 1)

## ---------- Figure 1: injury model (binomial) ----------
png("diag_injury.png", width = 1500, height = 520, res = 130)
par_set()
# (a) Binned residuals: average raw residual in 20 bins of fitted probability
p   <- fitted(inj.glm); r <- pl$injured - p
bins <- cut(p, quantile(p, 0:20/20), include.lowest = TRUE)
pb <- tapply(p, bins, mean); rb <- tapply(r, bins, mean); nb <- tapply(p, bins, length)
se <- 2 * sqrt(tapply(p * (1 - p), bins, sum)) / nb
plot(pb, rb, pch = 19, col = pt, ylim = range(c(rb, se, -se)) * 1.1,
     xlab = "Mean fitted probability (bin)", ylab = "Mean residual (bin)",
     main = "(a) Binned residuals")
lines(pb, se, lty = 2, col = mute); lines(pb, -se, lty = 2, col = mute)
abline(h = 0, col = mute)
# (b) Deviance residuals vs linear predictor
plot(predict(inj.glm), residuals(inj.glm, "deviance"), pch = 1, col = pt, cex = 0.7,
     xlab = expression("Linear predictor " * hat(eta)), ylab = "Deviance residual",
     main = "(b) Deviance residuals")
abline(h = 0, col = mute)
# (c) Cook's distance
cdi <- cooks.distance(inj.glm)
plot(cdi, type = "h", col = pt, ylim = c(0, max(cdi) * 1.15), xlab = "Player index", ylab = "Cook's distance",
     main = "(c) Influence")
top <- order(cdi, decreasing = TRUE)[1:3]
text(top, cdi[top], top, pos = 3, cex = 0.8, col = ink)
dev.off()

## ---------- Figure 2: goals model (Poisson) ----------
png("diag_goals.png", width = 1500, height = 520, res = 130)
par_set()
rd <- residuals(goals.glm, "deviance"); cdg <- cooks.distance(goals.glm)
is8 <- seq_along(rd) == which.max(cdg)
# (a) Deviance residuals vs fitted
plot(fitted(goals.glm), rd, pch = 19, col = ifelse(is8, hl, pt),
     xlab = expression("Fitted mean " * hat(mu)), ylab = "Deviance residual",
     main = "(a) Residuals vs fitted")
abline(h = 0, col = mute)
text(fitted(goals.glm)[is8], rd[is8], "Match 8 (8-0)", pos = 2, cex = 0.8, col = ink)
# (b) Normal Q-Q of deviance residuals
q <- qqnorm(rd, plot.it = FALSE)
plot(q, pch = 19, col = ifelse(is8, hl, pt), xlab = "Theoretical quantiles",
     ylab = "Deviance residual", main = "(b) Normal Q-Q")
qqline(rd, col = mute)
# (c) Cook's distance
plot(cdg, type = "h", lwd = 2, col = ifelse(is8, hl, pt), xlab = "Match", ylab = "Cook's distance",
     main = "(c) Influence")
abline(h = 4 / nrow(m), lty = 2, col = mute)
text(which(is8), cdg[is8], "Match 8", pos = 4, cex = 0.8, col = ink)
dev.off()

print(round(cbind(fit = pb, resid = rb, band = se), 3))
print(sort(cdi, decreasing = TRUE)[1:3]); print(pl[top, ])
print(summary(rd)); print(4/nrow(m))
