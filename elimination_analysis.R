library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

generation.data <- data.table(read.csv('DanData/generationData.csv'))
level.data <- data.table(read.csv('DanData/levelData.csv'))
challenge.data <- data.table(read.csv('DanData/ChallengeData.csv'))
user.level.data <- data.table(read.csv('DanData/userLevelData.csv'))
user.challenge.data <- data.table(read.csv('DanData/userChallengeData.csv'))

## Do generation parameters predict actualy performance (sawtooth graph)
ggplot(user.level.data) +
  stat_summary(aes(lvl., totalScore, color=isBrowser),
               fun.y = function(x) mean(x), 
               geom = "line")

ggplot(user.level.data) +
  stat_summary(aes(lvl., numSolvedChallenges, color=isBrowser), 
               fun.y = function(x) mean(x), 
               geom = "line")

## What factors predict which words are selected


