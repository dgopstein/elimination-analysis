library(data.table)
library(ggplot2)
library(viridis)
library(relaimpo)
library(cowplot)

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

challenge.data[, challenge.time := (30 / (1 + (ch. / 5)))]

unspace.lst <- function(s) strsplit(as.character(s), ' ')
unspace <- function(s) unlist(unspace.lst(s))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
user.challenge.data[, selectedWord := trim(selectedWord)]

user.level.data.full <- level.data[user.level.data, on='lvl.']
user.level.data.full[, scoreRate := totalScore/maxScore]

#user.level.data.agg <- user.level.data.full[, .(user.count=.N, mean.norm.score = mean(totalScore / maxScore)), by=.(lvl.)]
user.level.data.agg <- user.level.data.full[,.(lvl.,totalScore,maxScore),by=.(userID, lvl.)]

# only keep the first time a player reaches a specific level
user.level.first <- user.level.data.full[, .(totalScore=totalScore[1L], maxScore=maxScore[1L]), .(userID, lvl.)]
user.level.first[, norm.score := totalScore/maxScore]

user.level.first.agg <- user.level.first[, .(user.count=.N, mean.norm.score = mean(totalScore / maxScore)), by=.(lvl.)]

user.level.first.agg

## Do generation parameters predict actual performance (sawtooth graph)
average.inferred.difficulty.per.level.plot <-
ggplot(user.level.first.agg) +
  #geom_smooth(aes(lvl., mean.norm.score, size=user.count / 5), span=.2) +
  geom_line(aes(lvl.+1, 1-mean.norm.score, size=user.count / 5)) +
  scale_size_continuous(range=c(.2,3)) +
  #geom_point(aes(x=lvl., y=1-mean.norm.score, size=user.count)) +
  #geom_text(aes(x=lvl., y=1-mean.norm.score, label=user.count, color='w')) +
  geom_vline(xintercept=c(5, 10, 15, 20, 25), color='#888888', linetype='longdash') +
  theme(legend.position = "none") +
  labs(x='Level', y='Inferred Difficulty\n[1 - mean(Score)]') + 
  ggtitle('Average Inferred Difficulty per Level')
average.inferred.difficulty.per.level.plot

ggsave("img/average_infrerred_difficulty_per_level.pdf", average.inferred.difficulty.per.level.plot, width=(width<-140), height=width*0.7, units = "mm")

ggplot(user.level.first, aes(lvl., 1 - norm.score)) +
  #geom_density_2d() +
  #stat_density_2d(aes(fill=..level..), geom='polygon') +
  #geom_tile() +
  geom_bin2d() +
  scale_fill_viridis_c(direction = -1)

user.generation.data <- generation.data[user.level.first, on='lvl.']
table(user.generation.data[, .(mixedWordsLengths, targetLength)])

user.generation.data[, minSourceWord := as.integer(unlist(lapply(unspace.lst(mixedWordsLengths), `[[`, 2)))]
user.generation.data[, nSourceWords := unlist(lapply(unspace.lst(mixedWordsLengths), length))]

generation.user.level.lm <- lm(norm.score ~ minWordFrequency + targetLength + 
                                 maxConseqLetter + minSourceWord,
                               user.generation.data)

summary(generation.user.level.lm)
relImportance <- calc.relimp(generation.user.level.lm, type = "lmg", rela = TRUE)
sort(relImportance$lmg)
plot(relImportance)

data.table(relImportance$lmg)
score.features.rel.importance.plot <-
ggplot(data.table(relImportance$lmg), aes(as.factor(1:4), V1*100)) +
  geom_col(fill='#dddddd', color='black') +
  scale_x_discrete(labels=c("Min Word Freq", "Target Len", "Max Consec Char", "Min Src Word")) +
  theme(axis.text.x = element_text(angle=360, vjust=0)) +
  labs(y=bquote('% of '~R^2), x = 'Predictive Feature') +
  ggtitle('Relative Importance of Features\nPredicting Normalized Score')
score.features.rel.importance.plot

ggsave("img/score_features_rel_importance.pdf", score.features.rel.importance.plot, width=(width<-150), height=width*0.7, units = "mm")


## What factors predict which words are selected
# unravel the challenge potential word list
challenge.data[100]

challenge.data.words <-
  challenge.data[, .(word = unspace(allPossibleWords),
                     splitDistance = as.integer(unspace(splitDistanceBetweenLetters)),
                     maxSequence = as.integer(unspace(maxSequenceLetters)),
                     has2X = as.logical(unspace(has2X)),
                     firstIndex = as.integer(unspace(firstIndex)),
                     challengeWord,
                     X2XLetter,
                     challenge.time
                     ), by=.(lvl., ch.)]

user.challenge.words <- merge(user.challenge.data, challenge.data.words, by=c('lvl.', 'ch.'), allow.cartesian = TRUE)

user.challenge.words[, selected := selectedWord == word]

user.challenge.words[, str.len := nchar(word)]


selected.glm <- glm(selected ~ splitDistance + maxSequence + str.len, "binomial", user.challenge.words)
#selected.lm <- lm(selected ~ splitDistance + maxSequence, user.challenge.words)

selected.glm$aic
# 252683.2
summary(selected.glm)
#plot(selected.glm)

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

#selected.rate.lm <- lm(selected.rate ~ n.shown + english.count + str.len + bad.word, all.words[n.shown > 500])
selected.rate.lm <- lm(selected.rate ~ n.shown + str.len + bad.word + entropy, all.words[n.shown > 10])

user.challenge.full[selectedWord == 'zygoma']

# What changes when users replay the same challenge

user.complete.replays <- user.challenge.full[ch.==0, .N, by=.(userID, lvl., ch.)][N>1]

user.level.replays <- user.complete.replays[, -c('ch.', 'N')][user.level.data.full, on=.(userID, lvl.)]

user.level.replays <- user.level.replays[, attempt := 1:.N, by=.(userID, lvl.)]

# Score
ggplot(user.level.replays[lvl.==1]) +
  geom_line(aes(attempt, totalScore/maxScore, group=userID, color=as.character(userID))) +
  xlim(1, 10) +
  theme(legend.position = "none")

ggplot(user.level.replays[lvl.==1]) +
  geom_line(aes(attempt, totalTime / maxTime, group=userID, color=as.character(userID))) +
  xlim(1, 10) +
  theme(legend.position = "none")


#### How often are words selected in a challenge

user.challenge.selected.rate <- user.challenge.words[str.len > 0, .(selected.rate = sum(selected) / .N) , by=.(lvl., ch., word, str.len, splitDistance, maxSequence, firstIndex, has2X, challengeWord, X2XLetter, challenge.time)]
#user.challenge.selected.rate[, word.idx := gregexpr(pattern=word,challengeWord)[[1]][1]]

user.challenge.selected.rate[bad.word.data, bad.word := TRUE, on='word']  
user.challenge.selected.rate[is.na(bad.word), bad.word := FALSE]
#user.challenge.selected.rate[challenge.data[, .(lvl., ch., challengeWord, X2XLetter)], ]

user.challenge.selected.rate$challenge.time

#divide word size by maxSeqLenght
user.challenge.selected.rate[, wordLen := nchar(word)]
user.challenge.selected.rate[, maxSeqRate := maxSequence / wordLen]

user.challenge.selected.rate.lm <- lm(selected.rate ~ str.len + maxSequence + has2X + splitDistance + firstIndex + bad.word, user.challenge.selected.rate)
summary(user.challenge.selected.rate.lm)

relImportance <- calc.relimp(user.challenge.selected.rate.lm, type = "lmg", rela = TRUE)
sort(relImportance$lmg)
plot(relImportance)



# plot of players over each level

user.level.data.all.lvls <- user.level.data.full[user.level.data.full[lvl.==9, userID, by=userID],,on=.(userID)]
user.level.data.all.lvls[, userID := as.factor(userID)]

ggplot(user.level.data.all.lvls) +
  geom_line(aes(lvl., totalScore/maxScore, group = userID, color=userID)) +
  theme(legend.position = "none")

# Correlation between first level score and levels completed

user.last.level <- user.level.data.full[, .(max.lvl. = max(lvl.)), by=.(userID)]
user.last.level <- user.level.data.full[lvl.== 0, .(userID, firstLevelScoreRate = scoreRate)][user.last.level, on=.(userID)]

ggplot(user.last.level, aes(x = firstLevelScoreRate, y = max.lvl.)) +
  geom_density_2d() +
  stat_density_2d(aes(fill = stat(density)), geom = "raster", contour = FALSE, n=30, h=.01*c(1,30)) +
  #xlim(0,1) +
  scale_fill_viridis()

# how many users make it to each level
user.last.level.sum <- user.last.level[, .N, by=.(max.lvl.)][order(max.lvl.)]
user.last.level.sum[, quit.rate := N / (1225 - cumsum(N) + N)]
ggplot(user.last.level.sum) + geom_col(aes(max.lvl., quit.rate))

# Average score per level
user.level.quantiles <- 
  user.level.data.full[, .(quantiles = quantile(.SD$scoreRate, probs = c(.5, .75, .25)),
                           score = c(mean(scoreRate), max(scoreRate), min(scoreRate)),
                           score.type=c('mean', 'high', 'low')), by=.(lvl.)]

user.last.level.sum[, max.lvl.inc := max.lvl.+1]

ggplot(user.level.quantiles) +
  geom_line(aes(lvl., 1-quantiles, group=score.type, color=score.type)) +
  geom_col(aes(max.lvl.inc, quit.rate), data=user.last.level.sum)

user.level.quantiles <- merge(user.level.quantiles, user.last.level.sum, by.x='lvl.', by.y='max.lvl.')

with(user.level.quantiles[score.type=='mean'], cor(quantiles, quit.rate))

ggplot(user.level.quantiles[score.type=='low'], aes(quantiles, quit.rate)) +
  geom_point()

# How many users played the game
length(unique(user.level.data.full$userID))

# Distribution of # of challenges played

user.level.data.full[, .(sumSolvedChallenges = sum(numSolvedChallenges)), by=userID][order(sumSolvedChallenges)]
user.level.data.full[userID == 3891308997][, .N, by=.(lvl.)]
?hist
hist(user.level.data.full[, .(sumSolvedChallenges = sum(numSolvedChallenges)), by=userID][sumSolvedChallenges <= 300]$sumSolvedChallenges,
     breaks = seq(0, 300, 10),
     main = "Number of challenges solved by players",
     xlab = "# of challenges solved")

n.challenges.per.user.hist.plot <- ggplot(user.level.data.full[, .(sumSolvedChallenges = sum(numSolvedChallenges)), by=userID][sumSolvedChallenges <= 300]) +
  labs(x='# of challenges solved', y='Frequency') + 
  ggtitle('Histogram of challenges solved by players') +
  geom_histogram(aes(sumSolvedChallenges), colour = '#99aacc', fill = '#cccccc')
n.challenges.per.user.hist.plot
ggsave("img/n_challenges_per_user_hist.pdf", n.challenges.per.user.hist.plot, width=(width<-140), height=width*0.7, units = "mm")

user.challenge.full[, .N, by=userID][order(N)][, .(mean(N), median(N))]

# Challenge time
mean(user.level.data.full$totalTime)
unique(user.level.data.full$maxTime)

ggplot(user.level.data.full, aes(totalTime, scoreRate)) +
  geom_density_2d() +
  stat_density_2d(aes(fill = stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_viridis()

# Length of selected word
user.challenge.full[, challengeWord := trim(challengeWord)]
user.challenge.full[, selectedWord := trim(selectedWord)]

user.challenge.full[, selectedLen := nchar(selectedWord)]
user.challenge.full[, challengeLen := nchar(as.character(challengeWord))]
user.challenge.full[, selectedLenRate := selectedLen/challengeLen]

ggplot(user.challenge.full[challengeLen==11], aes(selectedLen)) +
  geom_histogram()

user.challenge.full[, .(challengeWord, challengeLen, selectedWord, selectedLen)]

# Which level has sex
user.challenge.full[selectedWord=='sex', .N, by=.(lvl., ch.)]

user.challenge.full[lvl. == 0 & ch. == 2, .N, by=selectedWord][, .(selectedWord, N =N / sum(N))]

# How many users reached each level

level.users <- user.level.data.full[, .(N = length(unique(userID))), by=.(lvl.)]

n.users.per.level.plot <- ggplot(level.users) +
  geom_col(aes(lvl., N), colour = '#99aacc', fill = '#cccccc') +
  labs(x='Level', y='# Players') + 
  ggtitle('How Many Players Reached Each Level')

n.users.per.level.plot
  
ggsave("img/n_users_per_level_plot.pdf", n.users.per.level.plot, width=(width<-140), height=width*0.7, units = "mm")



