library(streamR)
library(plyr)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(Rstem)
library(tm)
library(wordcloud)
library(stringr)

gpclibPermit()

#Load the parsed Tweets
setwd("C:/Users/FuserLimon/Documents/Semester 2/Data Visualization/ps4")
load("ParsedTweets.rdata")
save(all_tweets.df, file="ps4.Rdata")

#Bind all the tweets into one dataframe
all_tweets.df <- rbind (t02092016_df, t02202016_df, t02232016_df, t02272016_df, 
                        t03012016_df, t03052016_df, t03062016_df, t03082016_df,
                        t03152016_df, t03152016_df)
save(all_tweets.df, file="ps4.Rdata")

#Split the tweets into 4 nominees
HC <- subset (all_tweets.df, grepl(pattern =  "Clinton | clinton | Hillary | hillary | iamwithher | Iamwithher", 
                                   all_tweets.df$text))
BS <- subset (all_tweets.df, grepl(pattern =  "Bernie | bernie | Sanders | sanders | feelthebern", 
                                   all_tweets.df$text))
DT <- subset (all_tweets.df, grepl(pattern =  "Donald | Trump | donald | trump | realddonaldtrump", 
                                   all_tweets.df$text))
TC <- subset (all_tweets.df, grepl(pattern =  "Ted | Cruz | ted | cruz", 
                                   all_tweets.df$text))

#Create Democrat and Republican subsets 
Dem_tweets <- rbind (HC, BS)
Rep_tweets <- rbind (TC, DT)

#Create a list of unnecessary words that I have encountered during previous altrecations
bad_words <- c("https", 
               "https...",
               "via",
               "use",
               "just",
               "think",
               "more",
               "turn", 
               "hothandsome",
               "watch",
               "get",
               "bad",
               "dude",
               "exatriz",
               "when",
               "you",
               "want",
               "retweet",
               "how",
               "never",
               "make",
               "with",
               "will",
               "say",
               "hes",
               "new",
               "nadelpari",
               "like",
               "need",
               "dont",
               "must",
               "porno",
               "lt3",
               "can",
               "call",
               "kissing",
               "retira",
               "and",
               "good",
               "now",
               "for",
               "can",
               "pjnet",
               "career",
               "macro",
               "love",
               "danscavino",
               "polit",
               "join",
               "htt.",
               "know",
               "cant",
               "aparecia",
               "enter",
               "realli",
               "not",
               "time",
               "that",
               "amp",
               "the",
               "thing",
               "what",
               "presid",
               "beli",
               "back",
               "tcot",
               "porn",     
               "httpstcoyev2vfwxkp",
               "one",
               "yes",
               "record",
               "whi",
               "tell",
               "this",
               "macro",
               "htt...",
               "http.",
               "httpstcongvjylzmet",
               "way",
               "htt.",
               "nadelparis",
               "movie",
               "belies",
               "see",
               "video",
               "macro",
               "retweeted",
               "why",
               "tampa",
               "please",
               "enters",
               "look",
               "best",
               "fuck",
               "tonight",
               "recommend",
               "someone",
               "opening",
               "thank",
               "valentines",
               "night",
               "going",
               "state",
               "happy",
               "stop",
               "man",
               "click",
               "home",
               "apply",
               "doesnt",
               "always",
               "birthday",
               "retail",
               "thanks",
               "show",
               "said",
               "old",
               "still",
               "shit",
               "even",
               "ever",
               "read",
               "better",
               "well",
               "take",
               "latest",
               "ive",
               "fit",
               "team",
               "last",
               "who",
               "getting",
               "didnt",
               "come",
               "hospitality",
               "game",
               "youre",
               "really",
               "real",
               "made",
               "whats",
               "sales",
               "gonna",
               "girl",
               "many",
               "much",
               "first",
               "were",
               "thats",
               "ill",
               "its",
               "feel",
               "got",
               "big",
               "lol",
               "next",
               "might",
               "que",
               "anyone",
               "today",
               "wanna",
               "done",
               "check",
               "year",
               "years",
               "wait",
               "ass",
               "store",
               "nursing",
               "qual")

#Create a lexicon from words
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
religion.words <- lexicon$word[lexicon$polarity=="religion"]
china.words <- lexicon$word[lexicon$polarity=="china"]
economy.words <- lexicon$word[lexicon$polarity=="economy"]
healthcare.words <- lexicon$word[lexicon$polarity=="health_care"]
guncontrol.words <- lexicon$word[lexicon$polarity=="gun_control"]
race.words <- lexicon$word[lexicon$polarity=="race"]
climatechange.words <- lexicon$word[lexicon$polarity=="climate_change"]
immigration.words <- lexicon$word[lexicon$polarity=="immigration"]
military.words <- lexicon$word[lexicon$polarity=="military"]
trade.words <- lexicon$word[lexicon$polarity=="trade"]


#Test Corpus for Hillary Clinton (we will might need to adjust the count for topics
#by the amount of tweets)
HC$text <- sapply(HC$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpusHC <- paste(unlist(HC$text), collapse =" ")
TweetCorpusHC <- Corpus(VectorSource(TweetCorpusHC))
TweetCorpusHC <- tm_map(TweetCorpusHC, removePunctuation)
TweetCorpusHC <- tm_map(TweetCorpusHC, removeWords, stopwords("english"))
TweetCorpusHC <- tm_map(TweetCorpusHC, content_transformer(tolower),lazy=TRUE)
TweetCorpusHC <- tm_map(TweetCorpusHC, PlainTextDocument)
TweetCorpusHC <- tm_map(TweetCorpusHC, removeWords, bad_words)

wordcloud(TweetCorpusHC, max.words = 100, random.order = FALSE)
save(HC, file="ps4HC.Rdata")

#Hillary Counts
HC_religion <- sum(str_count(TweetCorpusHC, religion.words))
HC_china <- sum(str_count(TweetCorpusHC, china.words))
HC_economy <- sum(str_count(TweetCorpusHC, economy.words))
HC_healthcare <- sum(str_count(TweetCorpusHC, healthcare.words))
HC_guncontrol <- sum(str_count(TweetCorpusHC, guncontrol.words))
HC_race <- sum(str_count(TweetCorpusHC, race.words))
HC_climatechange <- sum(str_count(TweetCorpusHC, climatechange.words))
HC_immigration <- sum(str_count(TweetCorpusHC, immigration.words))
HC_military <- sum(str_count(TweetCorpusHC, military.words))
HC_trade <- sum(str_count(TweetCorpusHC, trade.words))

#Test Corpus for Bernie Sanders
BS$text <- sapply(BS$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpusBS <- paste(unlist(BS$text), collapse =" ")
TweetCorpusBS <- Corpus(VectorSource(TweetCorpusBS))
TweetCorpusBS <- tm_map(TweetCorpusBS, removePunctuation)
TweetCorpusBS <- tm_map(TweetCorpusBS, removeWords, stopwords("english"))
TweetCorpusBS <- tm_map(TweetCorpusBS, content_transformer(tolower),lazy=TRUE)
TweetCorpusBS <- tm_map(TweetCorpusBS, PlainTextDocument)
TweetCorpusBS <- tm_map(TweetCorpusBS, removeWords, bad_words)

save(BS, file="ps4BS.Rdata")
rm(BS)

#Bernie Counts
BS_religion <- sum(str_count(TweetCorpusBS, religion.words))
BS_china <- sum(str_count(TweetCorpusBS, china.words))
BS_economy <- sum(str_count(TweetCorpusBS, economy.words))
BS_healthcare <- sum(str_count(TweetCorpusBS, healthcare.words))
BS_guncontrol <- sum(str_count(TweetCorpusBS, guncontrol.words))
BS_race <- sum(str_count(TweetCorpusBS, race.words))
BS_climatechange <- sum(str_count(TweetCorpusBS, climatechange.words))
BS_immigration <- sum(str_count(TweetCorpusBS, immigration.words))
BS_military <- sum(str_count(TweetCorpusBS, military.words))
BS_trade <- sum(str_count(TweetCorpusBS, trade.words))


#Test Corpus for Donald Trump
DT$text <- sapply(DT$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpusDT <- paste(unlist(DT$text), collapse =" ")
TweetCorpusDT <- Corpus(VectorSource(TweetCorpusDT))
TweetCorpusDT <- tm_map(TweetCorpusDT, removePunctuation)
TweetCorpusDT <- tm_map(TweetCorpusDT, removeWords, stopwords("english"))
TweetCorpusDT <- tm_map(TweetCorpusDT, content_transformer(tolower),lazy=TRUE)
TweetCorpusDT <- tm_map(TweetCorpusDT, PlainTextDocument)
TweetCorpusDT <- tm_map(TweetCorpusDT, removeWords, bad_words)

save(DT, file="ps4DT.Rdata")
rm(DT)

#Donald Trump Counts
DT_religion <- sum(str_count(TweetCorpusDT, religion.words))
DT_china <- sum(str_count(TweetCorpusDT, china.words))
DT_economy <- sum(str_count(TweetCorpusDT, economy.words))
DT_healthcare <- sum(str_count(TweetCorpusDT, healthcare.words))
DT_guncontrol <- sum(str_count(TweetCorpusDT, guncontrol.words))
DT_race <- sum(str_count(TweetCorpusDT, race.words))
DT_climatechange <- sum(str_count(TweetCorpusDT, climatechange.words))
DT_immigration <- sum(str_count(TweetCorpusDT, immigration.words))
DT_military <- sum(str_count(TweetCorpusDT, military.words))
DT_trade <- sum(str_count(TweetCorpusDT, trade.words))

#Test Corpus for Ted Cruz
TC$text <- sapply(TC$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpusTC <- paste(unlist(TC$text), collapse =" ")
TweetCorpusTC <- Corpus(VectorSource(TweetCorpusTC))
TweetCorpusTC <- tm_map(TweetCorpusTC, removePunctuation)
TweetCorpusTC <- tm_map(TweetCorpusTC, removeWords, stopwords("english"))
TweetCorpusTC <- tm_map(TweetCorpusTC, content_transformer(tolower),lazy=TRUE)
TweetCorpusTC <- tm_map(TweetCorpusTC, PlainTextDocument)
TweetCorpusTC <- tm_map(TweetCorpusTC, removeWords, bad_words)

save(TC, file="ps4TC.Rdata")
rm(TC)

#Ted Cruz Counts
TC_religion <- sum(str_count(TweetCorpusTC, religion.words))
TC_china <- sum(str_count(TweetCorpusTC, china.words))
TC_economy <- sum(str_count(TweetCorpusTC, economy.words))
TC_healthcare <- sum(str_count(TweetCorpusTC, healthcare.words))
TC_guncontrol <- sum(str_count(TweetCorpusTC, guncontrol.words))
TC_race <- sum(str_count(TweetCorpusTC, race.words))
TC_climatechange <- sum(str_count(TweetCorpusTC, climatechange.words))
TC_immigration <- sum(str_count(TweetCorpusTC, immigration.words))
TC_military <- sum(str_count(TweetCorpusTC, military.words))
TC_trade <- sum(str_count(TweetCorpusTC, trade.words))

#Corpus for Democrats 
Dem_tweets$text <- sapply(Dem_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpusDem <- paste(unlist(Dem_tweets$text), collapse =" ")
TweetCorpusDem <- Corpus(VectorSource(TweetCorpusDem))
TweetCorpusDem <- tm_map(TweetCorpusDem, removePunctuation)
TweetCorpusDem <- tm_map(TweetCorpusDem, removeWords, stopwords("english"))
TweetCorpusDem <- tm_map(TweetCorpusDem, content_transformer(tolower),lazy=TRUE)
TweetCorpusDem <- tm_map(TweetCorpusDem, PlainTextDocument)
TweetCorpusDem <- tm_map(TweetCorpusDem, removeWords, bad_words)

save(Dem_tweets, file="ps4Dem.Rdata")
rm(Dem_tweets)

#Democrat Counts
Dem_religion <- sum(str_count(TweetCorpusDem, religion.words))
Dem_china <- sum(str_count(TweetCorpusDem, china.words))
Dem_economy <- sum(str_count(TweetCorpusDem, economy.words))
Dem_healthcare <- sum(str_count(TweetCorpusDem, healthcare.words))
Dem_guncontrol <- sum(str_count(TweetCorpusDem, guncontrol.words))
Dem_race <- sum(str_count(TweetCorpusDem, race.words))
Dem_climatechange <- sum(str_count(TweetCorpusDem, climatechange.words))
Dem_immigration <- sum(str_count(TweetCorpusDem, immigration.words))
Dem_military <- sum(str_count(TweetCorpusDem, military.words))
Dem_trade <- sum(str_count(TweetCorpusDem, trade.words))

#Corpus for Republicans 
Rep_tweets$text <- sapply(Rep_tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpusRep <- paste(unlist(Rep_tweets$text), collapse =" ")
TweetCorpusRep <- Corpus(VectorSource(TweetCorpusRep))
TweetCorpusRep <- tm_map(TweetCorpusRep, removePunctuation)
TweetCorpusRep <- tm_map(TweetCorpusRep, removeWords, stopwords("english"))
TweetCorpusRep <- tm_map(TweetCorpusRep, content_transformer(tolower),lazy=TRUE)
TweetCorpusRep <- tm_map(TweetCorpusRep, PlainTextDocument)
TweetCorpusRep <- tm_map(TweetCorpusRep, removeWords, bad_words)

save(Rep_tweets, file="ps4Rep.Rdata")
rm(Rep_tweets)

#Republican Counts
Rep_religion <- sum(str_count(TweetCorpusRep, religion.words))
Rep_china <- sum(str_count(TweetCorpusRep, china.words))
Rep_economy <- sum(str_count(TweetCorpusRep, economy.words))
Rep_healthcare <- sum(str_count(TweetCorpusRep, healthcare.words))
Rep_guncontrol <- sum(str_count(TweetCorpusRep, guncontrol.words))
Rep_race <- sum(str_count(TweetCorpusRep, race.words))
Rep_climatechange <- sum(str_count(TweetCorpusRep, climatechange.words))
Rep_immigration <- sum(str_count(TweetCorpusRep, immigration.words))
Rep_military <- sum(str_count(TweetCorpusRep, military.words))
Rep_trade <- sum(str_count(TweetCorpusRep, trade.words))

#Manually created csv file of the first Visualization comparing Hillary and Bernie
HCvsBS <-read.csv("HCvsBS.csv", header = TRUE, sep = ",", quote = "\"")
ggplot(data=HCvsBS, aes(x=topic, y=count, fill=names)) +
  geom_bar(stat="identity", position=position_dodge())

#Manually created csv file of the second Visualization comparing Cruz and Trump
TCvsDT <-read.csv("TCvsDT.csv", header = TRUE, sep = ",", quote = "\"")
ggplot(data=TCvsDT, aes(x=topic, y=count, fill=names)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=TCvsDT, aes(x=topic, y=count, fill=names)) +
  geom_bar(stat="identity", position=position_dodge())

#Manually created csv file of the third Visualization comparing Democrats and Republicans
DEMvsREP <-read.csv("DEMvsREP.csv", header = TRUE, sep = ",", quote = "\"")
ggplot(data=DEMvsREP, aes(x=topic, y=count, fill=names)) +
  geom_bar(stat="identity", position=position_dodge())
