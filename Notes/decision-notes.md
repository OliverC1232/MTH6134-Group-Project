# Decision Notes

This document records the analytical decisions made during the Statistical Modelling II group project.

## 6th November 2026 – Player Injury Analysis

- **Dataset:** league_players.csv (620 players across 24 clubs).
- **Research focus:** Investigating factors associated with player injuries.
- **Model considered:** Logistic regression.
- **Outcome variable:** injured (0 = not injured, 1 = injured).
- **Explanatory variables:** Previous injuries, age, training load and playing position.
- **Reason for model choice:** Logistic regression is appropriate because the outcome variable is binary.
- **Additional analysis:** Compared injury rates across clubs.
- **Current findings:** Injury rates ranged from 8.33% to 44.44% across clubs.

## 20th November 2026 - Contingency Table
- **Dataset:** league_players.csv (620 players across 24 clubs).
- **Research focus:** Investigating associations between injury history, current injuries and playing position
- **Model considered:** Contingency Table
- **Key Findings:** Previous injury history was significantly associated with current injuries, while playing position was not significantly associated with injuries overall.
- **Model Comparison:** Simple log-linear model was considered
- **Final Model:** frequency ~ history * injured + position, with no significant evidence of poor fit (p=0.1663)
- **Next Steps:** Share findings and start prep for write up

These notes will be updated as the group discusses and finalises its analytical decisions.
