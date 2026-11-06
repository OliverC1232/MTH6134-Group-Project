# statistical modelling II
# Player injury analysis

list.files()

csv_files <- list.files(pattern = "\\.csv$")
csv_files
players <- read.csv(csv_files[1])
head(players)
dim(players)
str(players)

summary(players)
colSums(is.na(players))
table(players$injured)
prop.table(table(players$injured))
table(players$position)
table(players$training_load)

boxplot(prev_injuries ~ factor(injured),
        data = players,
        names = c("Not injured", "Injured"),
        xlab = "Current-season injury status",
        ylab = "Previous injuries",
        main = "Previous injuries by Current Injury Status",
        col = c("lightblue", "salmon"))

boxplot(age ~ factor(injured),
        data = players,
        names = c("Not injured", "Injured"),
        xlab = "Current-season injury status",
        ylab = "Age (years)",
        main = "Players Age by Injury Status",
        col = c("lightblue", "salmon"))

boxplot(training_load ~ factor(injured),
        data = players,
        names = c("Not injured", "Injured"),
        xlab = "Current-season injury status",
        ylab = "Training load (1-5)",
        main = "Training load by Injury Status",
        col = c("lightblue", "salmon"))

table(players$position, players$injured)
prop.table(table(players$position, players$injured), margin = 1)

players$position <- as.factor(players$position)
injury_model <- glm(
  injured ~ prev_injuries + age + training_load + position,
  data = players,
  family = binomial
)
summary(injury_model)

coef(summary(injury_model))

exp(coef(injury_model))
exp(confint(injury_model))

players$predicted_probability <- predict(
  injury_model,
  type = "response"
)

players$predicted_injury <- ifelse(
  players$predicted_probability >= 0.5, 1, 0
)

table(
  Actual = players$injured,
  Predicted = players$predicted_injury
)

cm <- table(
  Actual = players$injured,
  Predicted = players$predicted_injury
)
accuracy <- sum(diag(cm)) / sum(cm)
sensitivity <- cm["1", "1"] / sum(cm["1", ])
specificity <- cm["0", "0"] / sum(cm["0", ])
accuracy
sensitivity
specificity

nobs(injury_model)
colSums(is.na(players))
sum(complete.cases(players[, c(
  "injured",
  "prev_injuries",
  "age",
  "training_load",
  "position"
)]))

par(mfrow = c(2, 2))
plot(injury_model)
par(mfrow = c(1, 1))

cooks_values <- cooks.distance(injury_model)
max(cooks_values)
which(cooks_values > 4 / nobs(injury_model))
sum(cooks_values > 4 / nobs(injury_model))

top_influential <- order(cooks_values, decreasing = TRUE)[1:5]
data.frame(
  Player = top_influential,
  Cooks_Distance = cooks_values[top_influential]
)

model_results <- data.frame(
  Estimate = coef(summary(injury_model))[, "Estimate"],
  P_value = coef(summary(injury_model))[, "Pr(>|z|)"],
  Odds_Ratio = exp(coef(injury_model))
)
round(model_results, 4)

round(model_results[c(
  "prev_injuries",
  "age",
  "training_load"
), ], 3)

round(model_results[grep("position", rownames(model_results)), ], 3)

levels(players$position)

round(exp(confint(injury_model)), 3)

cor(
  players[, c("prev_injuries", "age", "training_load")],
  use = "complete.obs"
)

team_injury_rates <- prop.table(
  table(players$team, players$injured),
  margin = 1
)[, "1"]
summary(team_injury_rates)
range(team_injury_rates)
sd(team_injury_rates)

sort(team_injury_rates)
head(sort(team_injury_rates, decreasing = TRUE), 5)

team_summary <- data.frame(
  team = names(table(players$team)),
  players = as.vector(table(players$team)),
  injured = as.vector(tapply(players$injured, players$team, sum)),
  injury_rate = as.vector(tapply(players$injured, players$team, mean))
)
head(team_summary[order(team_summary$injury_rate, decreasing = TRUE), ], 5)

head(sort(team_injury_rates), 5)











