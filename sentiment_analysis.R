library("tm")
library("SnowballC")
library("caTools")
library("rpart")
library("rpart.plot")
library("ROCR")
library("randomForest")

# authorisation
if (!require('pacman')) install.packages('pacman')
pacman::p_load(twitteR, ROAuth, RCurl)

api_key = ''
api_secret = ''
access_token = ''
access_token_secret = ''

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

# set up the URLs
reqURL = 'https://api.twitter.com/oauth/request_token'
accessURL = 'https://api.twitter.com/oauth/access_token'
authURL = 'https://api.twitter.com/oauth/authorize'

twitCred = OAuthFactory$new(consumerKey = api_key, consumerSecret = api_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)

twitCred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))

if (!require('pacman')) install.packages('pacman&')
pacman::p_load(devtools, installr)
install.Rtools()
install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')
install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')

if (!require('pacman')) install.packages('pacman')
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

api_key = ''
api_secret = ''
access_token = ''
access_token_secret = ''

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# harvest some tweets
some_tweets = searchTwitter("keyword", n=100, lang='en')

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

# remove retweet entities
some_txt = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', some_txt)
# remove at people
some_txt = gsub('@\\w+', '', some_txt)
# remove punctuation
some_txt = gsub('[[:punct:]]', '', some_txt)
# remove numbers
some_txt = gsub('[[:digit:]]', '', some_txt)
# remove html links
some_txt = gsub('http\\w+', '', some_txt)
# remove unnecessary spaces
some_txt = gsub('[ \t]{2,}', '', some_txt)
some_txt = gsub('^\\s+|\\s+$', '', some_txt)

# define 'tolower error handling' function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

#save the tweets in sentiment file
write.csv(some_txt, "sentiment.csv",row.names = F)

# Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(some_txt, algorithm='bayes', prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by 'unknown'
emotion[is.na(emotion)] = 'unknown'

# classify polarity
class_pol = classify_polarity(some_txt, algorithm='bayes')
#write.csv(class_pol, "score.csv", row.names = T)
# get polarity best fit
polarity = class_pol[,4]

# Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# save the tweets with polarity in polarity_result.csv file
write.csv(sent_df, "polarity_result.csv", row.names = F)

# Let's do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette='Dark2') +
  labs(x='emotion categories', y='number of tweets') +
  ggtitle('classification by emotion') +
  theme(plot.title = element_text(size=12, face='bold'))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette='RdGy') +
  labs(x='polarity categories', y='number of tweets') +
  ggtitle('classification by polarity') +
  theme(plot.title = element_text(size=12, face='bold'))

# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep('', nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=' ')
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords('english'))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, 'Dark2'),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

tweets = read.csv("read the polarity_result.csv file", stringsAsFactors=FALSE) 
str(tweets)

tweets$Negative = as.factor(tweets$polarity == "negative")
table(tweets$Negative)
tweets$Positive = as.factor(tweets$polarity == "positive")
table(tweets$Positive)

corpus <- Corpus(VectorSource(tweets$text))
corpus
corpus[[1]]

corpus <- tm_map(corpus, tolower)
corpus[[1]]
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
stopwords("english")[1:10]
corpus <- tm_map(corpus, removeWords, c("bjp", stopwords("english")))
corpus[[1]]
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]

DTM <- DocumentTermMatrix(corpus)
DTM
DTM$ncol

#inspect(DTM[1000:1005, 505:515])
lfreq <- findFreqTerms(DTM, lowfreq = 20)
write.csv(lfreq, "low_freq.txt", row.names = F)
hfreq <- findFreqTerms(DTM, highfreq = 20)
write.csv(hfreq, "hfreq.txt", row.names = F)

length(lfreq)
length(hfreq)

sparse_DTM <- removeSparseTerms(DTM, 0.995)
sparse_DTM
tweetsSparse <- as.data.frame(as.matrix(sparse_DTM))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$Negative <- tweets$Negative
tweetsSparse$Positive <- tweets$Positive

set.seed(123)

splitNegative <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparseNegative <- subset(tweetsSparse, splitNegative == TRUE)
testSparseNegative <- subset(tweetsSparse, splitNegative == FALSE)

splitPositive <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparsePositive <- subset(tweetsSparse, splitPositive == TRUE)
testSparsePositive <- subset(tweetsSparse, splitPositive == FALSE)

#Cart Model
tweetCARTNegative <- rpart(Negative ~ . , data = trainSparseNegative, method = "class")
prp(tweetCARTNegative)

tweetCARTPositive <- rpart(Positive ~ . , data = trainSparsePositive, method = "class")
prp(tweetCARTPositive)

predictCARTNegative <- predict(tweetCARTNegative, newdata = testSparseNegative, type = "class")
predictCARTPositive <- predict(tweetCARTPositive, newdata = testSparsePositive, type = "class")

cmat_CARTNegative <- table(testSparseNegative$Negative, predictCARTNegative)
cmat_CARTNegative 

accu_CART <- (cmat_CARTNegative[1,1] + cmat_CARTNegative[2,2])/sum(cmat_CARTNegative)

round(accu_CART,4) # Overall Accuracy
round(cmat_CARTNegative[2,2]/sum(cmat_CARTNegative[2,]),4) # Sensitivity 
round(cmat_CARTNegative[1,1]/sum(cmat_CARTNegative[1,]),4) # Specificity 
round(cmat_CARTNegative[1,2]/sum(cmat_CARTNegative[1,]),4) # FP Rate

cmat_CARTPositive <- table(testSparsePositive$Positive, predictCARTPositive)
cmat_CARTPositive
accu_CARTP <- (cmat_CARTPositive[1,1] + cmat_CARTPositive[2,2])/sum(cmat_CARTPositive)

round(accu_CARTP,4) #Overall Accuracy
round(cmat_CARTPositive[2,2]/sum(cmat_CARTPositive[2,]),4) #Sensitivity
round(cmat_CARTPositive[1,1]/sum(cmat_CARTPositive[1,]),4) #Specificity
round(cmat_CARTPositive[1,2]/sum(cmat_CARTPositive[1,]),4) #FP Rate

#Base Line Model
cmat_baseline <- table(testSparseNegative$Negative)
cmat_baseline
accu_baseline <- max(cmat_baseline)/sum(cmat_baseline)

cmat_baselineP <- table(testSparsePositive$Positive)
cmat_baselineP
accu_baselineP <- max(cmat_baselineP)/sum(cmat_baselineP)

round(accu_baseline,4)
round(accu_baselineP,4)

set.seed(123)

#Random Forest
tweetRFN <- randomForest(Negative ~ . , data = trainSparseNegative)
tweetRFN

set.seed(123)
tweetRFP <- randomForest(Positive ~ . , data = trainSparsePositive)
tweetRFP

predictRFN <- predict(tweetRFN, newdata = testSparseNegative)
predictRFP <- predict(tweetRFP, newdata = testSparsePositive)

cmat_RFN <- table(testSparseNegative$Negative, predictRFN)
cmat_RFN 
accu_RFN <- (cmat_RFN[1,1] + cmat_RFN[2,2])/sum(cmat_RFN)

cmat_RFP <- table(testSparsePositive$Positive, predictRFP)
cmat_RFP 
accu_RFP <- (cmat_RFP[1,1] + cmat_RFP[2,2])/sum(cmat_RFP)

round(accu_RFN,4) #Overall Accuracy for Negative
round(accu_RFP,4) #Overall Accuracy for Positive


# Logistic Regression Model
tweetLogN <- glm(Negative ~ . , data = trainSparseNegative, family = "binomial")
tweetLogP <- glm(Positive ~ . , data = trainSparsePositive, family = "binomial")

tweetLog_predict_testN <- predict(tweetLogN, type = "response", newdata = testSparseNegative)
tweetLog_predict_testP <- predict(tweetLogP, type = "response", newdata = testSparsePositive)

cmat_logRegrN <- table(testSparseNegative$Negative, tweetLog_predict_testN > 0.5)
cmat_logRegrN
accu_logRegrN <- (cmat_logRegrN[1,1] + cmat_logRegrN[2,2])/sum(cmat_logRegrN)

cmat_logRegrP <- table(testSparsePositive$Positive, tweetLog_predict_testP > 0.5)
cmat_logRegrP
accu_logRegrP <- (cmat_logRegrP[1,1] + cmat_logRegrP[2,2])/sum(cmat_logRegrP)

round(accu_logRegrN,4) #Accuracy for Negative
round(accu_logRegrP,4) #Accuracy for Positive
