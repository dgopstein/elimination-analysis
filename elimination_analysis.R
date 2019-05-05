library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

generation.data <- data.table(read.csv('DanData/generationData.csv'))
level.data <- data.table(read.csv('DanData/levelData.csv'))
challenge.data <- data.table(read.csv('DanData/ChallengeData.csv'))
user.level.data <- data.table(read.csv('DanData/userLevelData.csv'))
user.challenge.data <- data.table(read.csv('DanData/userChallengeData.csv'))

user.level.data.full <- level.data[user.level.data, on='lvl.']

#user.level.data.agg <- user.level.data.full[, .(user.count=.N, mean.norm.score = mean(totalScore / maxScore)), by=.(lvl.)]
user.level.data.agg <- user.level.data.full[,.(lvl.,totalScore,maxScore),by=.(user.id, lvl.)]

# only keep the first time a player reaches a specific level
user.level.first <- user.level.data.full[, .(totalScore=totalScore[1L], maxScore=maxScore[1L]), .(userID, lvl.)]
user.level.first[, norm.score := totalScore/maxScore]

user.level.first.agg <- user.level.first[, .(user.count=.N, mean.norm.score = mean(totalScore / maxScore)), by=.(lvl.)]

## Do generation parameters predict actualy performance (sawtooth graph)
ggplot(user.level.first.agg) +
  geom_line(aes(lvl., 1 - mean.norm.score)) +
  #geom_point(aes(x=lvl., y=1-mean.norm.score, size=user.count)) +
  geom_text(aes(x=lvl., y=1-mean.norm.score, label=user.count, color='w')) +
  geom_vline(xintercept=c(5, 10, 15, 20, 25))

ggplot(user.level.first, aes(lvl., 1 - norm.score)) +
  #geom_density_2d() +
  #stat_density_2d(aes(fill=..level..), geom='polygon') +
  #geom_tile() +
  geom_bin2d() +
  scale_fill_viridis_c(direction = -1)

generation.user.level.lm <- lm(norm.score ~ mixedWordsLengths + targetLength + minWordFrequency + maxWordFrequency + maxConseqLetter,
    generation.data[user.level.first, on='lvl.'])

summary(generation.user.level.lm)

## What factors predict which words are selected
# unravel the challenge potential word list
challenge.data.words <- challenge.data[, .(word = unlist(strsplit(as.character(allPossibleWords), ' '))), by=.(lvl., ch.)]


merge(user.challenge.data, challenge.data.words, by=c('lvl.', 'ch.'), allow.cartesian = TRUE)
