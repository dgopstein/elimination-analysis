library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

generation.data <- data.table(read.csv('DanData/generationData.csv'))
level.data <- data.table(read.csv('DanData/levelData.csv'))
challenge.data <- data.table(read.csv('DanData/ChallengeData.csv'))
user.level.data <- data.table(read.csv('DanData/userLevelData.csv'))
user.challenge.data <- data.table(read.csv('DanData/userChallengeData.csv'))
word.count.data <- data.table(read.csv('DanData/wordCount.csv'))
bad.word.data <- data.table(read.csv('DanData/badWords.txt', header=FALSE))
trigram.entropy.data <- data.table(read.csv('trigram-entropy.csv'))


colnames(bad.word.data) <- 'word'

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
user.challenge.data[, selectedWord := trim(selectedWord)]

user.level.data.full <- level.data[user.level.data, on='lvl.']

#user.level.data.agg <- user.level.data.full[, .(user.count=.N, mean.norm.score = mean(totalScore / maxScore)), by=.(lvl.)]
user.level.data.agg <- user.level.data.full[,.(lvl.,totalScore,maxScore),by=.(userID, lvl.)]

# only keep the first time a player reaches a specific level
user.level.first <- user.level.data.full[, .(totalScore=totalScore[1L], maxScore=maxScore[1L]), .(userID, lvl.)]
user.level.first[, norm.score := totalScore/maxScore]

user.level.first.agg <- user.level.first[, .(user.count=.N, mean.norm.score = mean(totalScore / maxScore)), by=.(lvl.)]

## Do generation parameters predict actual performance (sawtooth graph)
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
challenge.data[100]
unspace <- function(s) unlist(strsplit(as.character(s), ' '))
challenge.data.words <-
  challenge.data[, .(word = unspace(allPossibleWords),
                     splitDistance = as.integer(unspace(splitDistanceBetweenLetters)),
                     maxSequence = as.integer(unspace(maxSequenceLetters))
                     ), by=.(lvl., ch.)]

user.challenge.words <- merge(user.challenge.data, challenge.data.words, by=c('lvl.', 'ch.'), allow.cartesian = TRUE)

user.challenge.words[, selected := selectedWord == word]

user.challenge.words[, str.len := nchar(word)]


selected.glm <- glm(selected ~ splitDistance + maxSequence + str.len, "binomial", user.challenge.words)
#selected.lm <- lm(selected ~ splitDistance + maxSequence, user.challenge.words)

selected.glm$aic
# 252683.2
summary(selected.glm)
plot(selected.glm)

library(tree)
tree.totalScore <- tree(totalScore ~ totalTime + numSolvedChallenges + maxScore, user.level.data.full)
plot(tree.totalScore)
text(tree.totalScore, pretty = 0)

user.challenge.full <- challenge.data[user.challenge.data, on=c('lvl.', 'ch.')]

all.shown.words <- unlist(strsplit(as.character(user.challenge.full$allPossibleWords), ' '))
all.shown.word.counts <- plyr::count(all.shown.words)
all.shown.word.counts <- with(all.shown.word.counts,
                                 data.table(word = as.character(x), freq = freq))

all.selected.word.counts <- plyr::count(as.character(user.challenge.full$selectedWord))
all.selected.word.counts <- with(all.selected.word.counts,
                                 data.table(word = as.character(x), freq = freq))

#all.selected.word.counts[, word := as.character(word)]
#all.shown.word.counts[, word := as.character(word)]
all.words <- merge(all.selected.word.counts, all.shown.word.counts, by='word', all=T)
colnames(all.words) <- c('word', 'n.selected', 'n.shown')
  
all.words[n.selected > 0]
all.words[is.na(n.selected), n.selected := 0]
all.words[, selected.rate := n.selected / n.shown]

all.words[word.count.data, english.count := count, on='word']  
all.words[is.na(english.count), english.count := 0]

all.words[, str.len := nchar(word)]

all.words[bad.word.data, bad.word := TRUE, on='word']  
all.words[is.na(bad.word), bad.word := FALSE]

trigram.entropy.data[, word := tolower(word)]
all.words[trigram.entropy.data, entropy := entropy, on='word']
all.words[is.na(entropy), entropy := 0]

all.words

selected.rate.lm <- lm(selected.rate ~ n.shown + english.count + str.len + bad.word + entropy, all.words[n.shown > 500])
all.words[n.shown > 20]

summary(selected.rate.lm)

user.challenge.full[selectedWord == 'zygoma']









