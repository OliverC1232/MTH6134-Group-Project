players <- read.csv("league_players.csv")
position_injury_table <- table(
  players$position,
  players$injured
)
position_injury_table

position_chisq <- chisq.test(position_injury_table)
position_chisq
position_chisq$expected

players$injury_history <- ifelse(
  players$prev_injuries == 0,
  "No previous injuries",
  "Previous injuries"
)
history_injury_table <- table(
  players$injury_history,
  players$injured
)
history_injury_table

history_chisq <- chisq.test(history_injury_table)
history_chisq
history_chisq$expected

three_way_table <- table(
  players$injury_history,
  players$injured,
  players$position
)
three_way_table

position_tests <- lapply(
  c("DEF", "FWD", "GK", "MID"),
  function(pos) {
    tab <- three_way_table[, , pos]
    test <- chisq.test(tab, correct = FALSE)
    
    data.frame(
      Position = pos,
      Chi_squared = round(unname(test$statistic), 3),
      p_value = round(test$p.value, 4),
      Min_expected = round(min(test$expected), 2)
    )
  }
)
do.call(rbind, position_tests)

counts <- as.data.frame(three_way_table)
names(counts) <- c(
  "history", "injured", "position", "frequency"
)
model_two_way <- glm(
  frequency ~ (history + injured + position)^2,
  family = poisson,
  data = counts
)
model_three_way <- glm(
  frequency ~ history * injured * position,
  family = poisson,
  data = counts
)
anova(model_two_way, model_three_way, test = "Chisq")

drop1(model_two_way, test = "Chisq")

model_reduced <- glm(
  frequency ~ history * injured +
    history * position,
  family = poisson,
  data = counts
)
anova(model_reduced, model_two_way, test = "Chisq")

model_simple <- glm(
  frequency ~ history * injured + position,
  family = poisson,
  data = counts
)
anova(model_simple, model_reduced, test = "Chisq")

deviance(model_simple)
pchisq(deviance(model_simple),
       df.residual(model_simple),
       lower.tail = FALSE)



