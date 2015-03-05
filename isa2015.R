library("scales")
library("Cairo")
library("ROAuth")
library("twitteR")
library("ggplot2")
library("RCurl")

# read data
# the 'download.file' command might only work on Windows platforms.
download.file("https://raw.githubusercontent.com/felixhaass/isa-2015-twitter/master/isa2015_tweets.csv", destfile = "isa2015_tweets.csv")isa2015df <- read.csv(text = tweets_csv)
isa2015df <- read.csv("isa2015_tweets.csv")

################
# Clean tweets #
################


# Filter international spirit awards & thai band tweets
isa2015df <- isa2015df %>% filter(screenName != "newzcard" & 
                                    screenName != "The_OnlyAgency" & 
                                    screenName != "B93AllHits" &
                                    screenName != "Minah_GSD_BTH" & 
                                    screenName != "Bonus_bbb")

isa2015df <- isa2015df[!grepl("#SpiritAward", isa2015df$text), ]
isa2015df <- isa2015df[!grepl("#indipendentspiritawards", isa2015df$text), ]
isa2015df <- isa2015df[!grepl("#PinkShirtDay", isa2015df$text), ]
isa2015df <- isa2015df[!grepl("#KARA", isa2015df$text), ]
isa2015df <- isa2015df[!grepl("@i2PMCNFT7", isa2015df$text), ]
isa2015df <- isa2015df[!grepl("@Minah_GSD_BTH", isa2015df$text), ]

###########################
# Plot tweeps' join dates #
###########################

# this requires a functioning Twitter authentication; follow these instructions:
# http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
# 
# # necessary step for Windows
# download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
# 
# #to get your consumerKey and consumerSecret see the twitteR documentation for instructions
# cred <- OAuthFactory$new(consumerKey='YOURKEY',
#                          consumerSecret='YOURSECRET',
#                          requestURL='https://api.twitter.com/oauth/request_token',
#                          accessURL='https://api.twitter.com/oauth/access_token',
#                          authURL='https://api.twitter.com/oauth/authorize')
# 
# # necessary step for Windows
# cred$handshake(cainfo="cacert.pem")
# 
# # save for later use for Windows
# save(cred, file="twitter authentication.Rdata")
# registerTwitterOAuth(cred)

names <- as.character(unique(sort(isa2015df$screenName)))
user_info <- lookupUsers(names, cainfo = "cacert.pem")

start_dates <- data.frame(lapply(user_info, "[[", "created"))
start_dates <- data.frame(name = names(start_dates), start_dates = t(start_dates)[ ,1])

start_dates$start_dates <- as.POSIXct(start_dates$start_dates)

minDate <- min(start_dates$start_dates)
maxDate <- max(start_dates$start_dates) 
dateBreaks <- seq(minDate, maxDate, by=7889231.49)
dateBreaks <- c(dateBreaks, maxDate)

tweetCount <- hist(start_dates$start_dates, breaks=dateBreaks, plot=FALSE)                             
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]

plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count))

CairoPNG("ISA_Twitter_join_dates.png", width=16, height=9, units = "in", dpi=200)
ggplot(plotData) +
  geom_line(aes(x=dates, y=tweets), stat="identity") +
  scale_y_continuous("Number of quarter-yearly joins") +
  scale_x_datetime(breaks="1 year", labels = date_format("%d Jan %Y")) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="#isa2015 Twitter users join date \n") 
dev.off()

##############################
# plot twenty busiest tweeps #
##############################

df <- data.frame(table(isa2015df$screenName))
df <- tail(df[order(df$Freq), ], 20)

# get real names
for(i in 1:nrow(df)) {
  df[i, "realname"] <- getUser(df[i, "Var1"],  cainfo="cacert.pem")$name  
}

# create display
df$disp_name <- paste0(df$realname, " \n(@", df$Var1, ")")

# save image
CairoPNG("busiest_isa2015_tweeps_NEW.png", height=1600, width = 1000, pointsize=30)
par(mar=c(5, 8, 4, 2))
barplot(df$Freq, 
        names.arg=df$disp_name, 
        horiz=T, 
        las=1,
        main="20 Busiest #ISA2015 Tweeps",
        xlab="Tweet count",
        space=0.6, 
        col="navy", 
        border = FALSE, cex.names = .6)

dev.off()

#######################
# Most Popular Tweets #
#######################

# add popular count
isa2015df$popular <- (isa2015df$favoriteCount + isa2015df$retweetCount)

# order & subset top 10
ordered <- isa2015df[order(isa2015df$popular, decreasing = TRUE), ]
top20tweets <- head(ordered[ordered$isRetweet == FALSE, ], 20)

# generate full twitter link
top20tweets$link <- paste0("https://twitter.com/", top20tweets$screenName, "/status/", top20tweets$id)

# write list of tweet links for c & p inclusion to wordpress
write.table(top20tweets[, "link"], row.names=FALSE, file="isa_twitter.txt", quote=F, col.names=FALSE)

##########################
# When do ISAlers tweet? #
##########################

# some code taken from here:
# http://bommaritollc.com/2012/05/21/charting-twitter-time-series-data-with-tweet-and-unique-user-counts/

isa2015df$date <- as.POSIXct(strptime(isa2015df$created, "%Y-%m-%d %H:%M:%S", tz="GMT"))

# fix dates / adjust to NOLA timezone
isa2015df$date <- isa2015df$date - (6*60*60)

minDate <- round(min(isa2015df$date), "hours")
maxDate <- round(max(isa2015df$date), "hours")
dateBreaks <- seq(minDate, maxDate, by=60 * 60)
dateBreaks <- c(dateBreaks, maxDate + 60 * 60)

tweetCount <- hist(isa2015df$date, breaks=dateBreaks, plot=FALSE)                             
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]

# prepare plot data
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count))

# save plot
CairoPNG("ISA_Twitter_trend.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(plotData) +
  geom_bar(aes(x=dates, y=tweets), stat="identity") +
  scale_y_continuous("Number of tweets") +
  scale_x_datetime(breaks="1 day", labels = date_format("%d February")) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="#isa2015 Twitter usage over time \n") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2015-02-18 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2015-02-19 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2015-02-20 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2015-02-21 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2015-02-22 00:00:00 UTC"))), color = "firebrick") +
  geom_text(label = "Day 1", aes(x = as.POSIXct("2015-02-18 13:00:00 UTC"), y = 300), size = 4) +
  geom_text(label = "Day 2", aes(x = as.POSIXct("2015-02-19 13:00:00 UTC"), y = 300), size = 4) +
  geom_text(label = "Day 3", aes(x = as.POSIXct("2015-02-20 13:00:00 UTC"), y = 300), size = 4) +
  geom_text(label = "Day 4", aes(x = as.POSIXct("2015-02-21 13:00:00 UTC"), y = 300), size = 4)

dev.off()

#######################
# Language processing #
#######################

# get text body
isa_text <- gettext(isa2015df$text)

# clean text

# remove non-ascii characters
isa_text <- gsub("[^\x20-\x7E]", "", isa_text)
# remove html links, h/t http://stackoverflow.com/questions/25352448/remove-urls-from-string-in-r
isa_text <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", isa_text)
# remove @people
isa_text <- gsub("@\\w+", "", isa_text)
# remove punctuation
isa_text <- gsub("[[:punct:]]", " ", isa_text)
# remove numbers
isa_text <- gsub("[[:digit:]]", "", isa_text)
# remove "RT"
isa_text <- gsub("RT", "", isa_text)
# remove "amp" 
isa_text <- gsub(" amp ", "", isa_text)
# remove "amp" 
isa_text <- gsub(" http ", "", isa_text)

# create corpus
isa_corpus <- Corpus(VectorSource(isa_text))

tdm <- TermDocumentMatrix(isa_corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("#isa2015", "isa", "isa2015", 
                                                       "ISA", "amp", "http", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m <- as.matrix(tdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(m), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm <- data.frame(word=names(word_freqs), freq = word_freqs)

# termcorr() function taken from Jay Ulfelder:
# https://github.com/ulfelder/national-security-strategy/blob/master/nss.explore.R

termcorr <- function(term2, corr = 0.2) {
  require(Hmisc)
  # Get a vector of correlation coefficients for terms above 0.5
  z <- findAssocs(tdm, term2, corr)
  if(is.matrix(z)) {
    # Make a dot plot of the results, limited to top 10
    dotchart2(z[1:length(z)],
              labels = dimnames(z)[[1]],
              lines = TRUE, lwd = 0.05, lty = 3,
              sort = FALSE,
              dotsize = 1.25, pch = 20, col = "firebrick2",
              cex.labels = 1,
              xlim = c(0,max(z+0.1)))
    title(main = list(term2, cex = 1.25))
  } else {
    stop("No or not enough correlation results!")
  }
}

# correlation plot of "coffee"
CairoPNG("termcorr_coffee.png", height=900, width = 1600, pointsize=30)
termcorr("coffee")
dev.off()

