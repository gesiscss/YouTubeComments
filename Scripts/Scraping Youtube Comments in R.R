################################################################################
#                                                                              #
#       Scraping and Analysing YouTube Comments with the Tuber Package         #
#                                                                              #
################################################################################


# This script gives a detailed walktrough of how to use the tuber package (https://cran.r-project.org/web/packages/tuber/tuber.pdf)
# to extract YouTube comments, format them, and give some basic examples for analysis of text and contained emojis. It was tested on Mac,
# Linux and Windows and should work on all systems. However, display of text containing Emojis in the console is only supported on Mac
# and Linux. In Windows, you will see the byte-code sequences for the respective Emojis in the console and in the View() tab in RStudio instead.

# The script is part of an ongoing research project:
# https://www.researchgate.net/project/Methods-and-Tools-for-Automatic-Sampling-and-Analysis-of-YouTube-Comments
# and will be subject to change. If you use substantive parts of this script as part of your own research, please cite it in the following way:

# Kohne, J., Breuer, J., & Mohseni, M. R. (2019). Methods and Tools for Automatic Sampling and Analysis of YouTube User Comments:https://doi.org/10.17605/OSF.IO/HQSXE



#### Setting up local environment ####

#### Working Directory

# You should set your working directory to the same location that this script file is located in.
getwd() # display current working directory
setwd(choose.dir()) # assign working directory path via a graphical user interface (GUI) -> should be the directory where this script is stored
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # alternatively: directly set working directory to directory that contains this script (RSTUDIO only)

#### Setting options

# Because we are working with textual data, we need to prevent R from recognizing all text strings as factors
options(stringsAsFactors = FALSE) # set automatic conversion of strings to factors to false

#### Installing & loading CRAN packages

# to extract, format, and explore the YouTube comment data, we need the following packages
packages <- c("devtools", "tm", "quanteda", "tuber","qdapRegex","rlang","purrr","ggplot2", "syuzhet", "lexicon") # create list of required packages
for (package in packages) { # check if those packages are already installed
  if (!require(package, character.only=T)) { # if not, install & load them
    install.packages(package)
    library(package, character.only=T)
  }
  else {library(package, character.only=T)} # if they are already installed, only load them
}
rm(packages, package) # remove the list-variable with the package names from the global environment

#### Installing & loading GitHub packages

# Two packages that we need to work with Emojis are not on CRAN yet, so we install them from GitHub
install_github("hadley/emo")
library(emo)

install_github("dill/emoGG")
library(emoGG)


#### Creating Google Account and authenticating for Youtube API #####

# To get access to data from Youtube, we have to use the Youtube API (https://developers.google.com/youtube/v3/).
# To do this, we need a token that identifies us when using the API, so Youtube can be sure
# that their Terms and Conditions are respected (e.g., there is a certain limit how much data you can access in
# a given timeframe). We´ll go through this step by step below:

# 1) If you do not have a Google account (or do not want to use your personal account for this project),
#    you have to create a new one here https://accounts.google.com/signup/v2/webcreateaccount?hl=en-GB&flowName=GlifWebSignIn&flowEntry=SignUp

# 2) With your Google account, you have to create a "Google Project" and configure it correctly, so you have the right credentials to get access to the data
#    with the tuber package. In case you need help, we have created a short video showing the process. You can find it here:
#    https://www.youtube.com/watch?v=qLrNq0jWH84

# 3) Use the credentials of the account associated with the project to authenticate your R-session. By doing this, you allow R access to all data on YouTube,that you would be able
#    to see if you went to the site logged in with this account. Be carefull to **NOT** share these credentials with anyone else you don´t want to be able to log
#    into this account.

# 4) Run the authentification for the R session using the credentials from the Google project:

#### Authentification in R ####

appID <- "12345abcde.apps.googleusercontent.com" # Insert your own app Id here
appSecret <- "4pp53Cr3T" # insert your own app Secret here


# When running this line, there will be a prompt in the console asking you to save the access token in a file
# select "No" by entering 2 in the console and hitting enter.
# Afterwards, a browser window should open, prompting you to log in with your Google account
# After logging in, you can close the browser and return to R
yt_oauth(app_id = appID,app_secret = appSecret)



#### Extracting comments ####

# First, we need the video ID(s) of the YouTube video(s) whose comments we want to scrape & analyze
# We can find the video ID by navigating to the video in our browser, and simply
# copying the last part of the URL that comes after "?v="
# For the example in this script, we use the video with the URL https://www.youtube.com/watch?v=DcJFdCmN98s
# Hence, the video ID we need is "DcJFdCmN98s"

# We can use functions from the tuber package to extract a sample of or all comments
Comments_sample <- get_comment_threads(c(video_id="DcJFdCmN98s"), max_results = 100) # set max_results value between 20 and 100 to get the x latest comments
# NB1: If you set it below 20 none will be fetched, above 100 all comments will be fetched.
# NB2: get_comment_threads does not collect replies to comments

Comments <- get_all_comments(c(video_id="DcJFdCmN98s")) # to extract all comments (this might take a while if there are many comments)

# Notice that your data set might contain fewer comments than are displayed as total comments on YouTube. This is because the
# tuber package only scrapes up to five replies for each comment. If a comment has more than five replies, all subsequent replies
# will not be extracted by the tuber package. This might be changed in the future, see: https://github.com/soodoku/tuber/issues/52

# If you want to save some time, you can also just load the comment data for the example video if you downloaded the whole GitHub repo by uncommenting and running the following line
# load("../Data/UnparsedCommentsUTF8.RData")

# Run the following line only if you only want to use comments that are not replies to other comments
# you probably want to do this if you are only interested in the reactions to the video (and not those to other comments)
Comments <- subset(Comments, is.na(parentId))


#### Formatting the data ####

# The extracted data is still in a rather raw format.
# To create a more detailed dataframe, we need to parse the data and extract relevant information.
# We included a script containing the function for parsing comments in the GitHub repository.
# Make sure that you have this script (yt_parse.R) in the working directory.

source("yt_parse.R")

# NB: This function was not written to be computationally efficient. Parsing large amounts of comments (+50.000) may take a while.
#     Nevertheless it might be useful for your own research if your dataset of interest is reasonably small (for reference: parsing ~ 50.000 comments took ~ 5 minutes on our laptops)

# This function will create a dataframe with one row per comment or comment reply and 11 columns:
#       1) YouTube username of the author
#       2) Original comment text
#       3) Text where hex codes for emojis are replaced with textual descriptions of emojis (e.g. EMOJI_grinningface)
#       4) Text where emojis have been deleted
#       5) A column including only the emojis that have been used in the comments (e.g. EMOJI_grinningface)
#       6) Amount of likes that the comment has received
#       7) List of URLs that are contained in the comment
#       8) Timestamp when comment was published
#       9) Timestamp when comment was last edited
#       10) YouTube moderation status flags (e.g. "likely spam")
#       11) Unique comment ID

# Now we can use the custom function defined above to format the "Comments" dataframe
FormattedComments <- yt_parse(Comments) # will take a while if the number of comments is high (> 1000)

# Next, we can see what the data looks like
View(FormattedComments) # display dataframe to see if everything worked (you can close the View window/tab after inspecting the dataframe)

# We can also create a plot to explore the development of the number of comments over time

# sort comments by date
FormattedComments <- FormattedComments[order(FormattedComments$Published),]

# create comment counter variable
CommentsCounter <- rep(1,dim(FormattedComments)[1])

# create dataframe for plotting
CounterFrame <- data.frame(CommentsCounter,unlist(FormattedComments[,8]))

# bin by week
colnames(CounterFrame) <- c("CommentCounter","DateTime")
CounterFrame$DateTime <- as.Date(cut(CounterFrame$DateTime, breaks = "week"))

# compute percentiles
PercTimes <- round(quantile(cumsum(CounterFrame$CommentCounter), probs = c(0.5, 0.75, 0.9, 0.99)))
CounterFrame$DateTime[PercTimes]

# plot
ggplot(CounterFrame,aes(x = DateTime,y = CommentCounter)) +
  stat_summary(fun.y = sum,geom="bar") +
  scale_x_date()  +
  labs(title = "Number of comments over time", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  geom_vline(xintercept = CounterFrame$DateTime[PercTimes],linetype = "dashed", colour = "red") +
  geom_text(aes(x = as.Date(CounterFrame$DateTime[PercTimes][1]) , label = "50%", y = 3500), colour = "red", angle=90, vjust = 1.2) +
  geom_text(aes(x = as.Date(CounterFrame$DateTime[PercTimes][2]) , label = "75%", y = 3500), colour = "red", angle=90, vjust = 1.2) +
  geom_text(aes(x = as.Date(CounterFrame$DateTime[PercTimes][3]) , label = "90%", y = 3500), colour = "red", angle=90, vjust = 1.2) +
  geom_text(aes(x = as.Date(CounterFrame$DateTime[PercTimes][4]) , label = "99%", y = 3500), colour = "red", angle=90, vjust = 1.2)

#### Basic frequency analysis for text ####

# In this section, we give a brief outline of text analysis for YouTube comments.
# This exemplary part is largely based on this tutorial: https://docs.quanteda.io/articles/pkgdown/examples/plotting.html

# We use the dataframe column without the emojis for the textual analysis here
# First of all, we need to remove new line commands from comment texts
FormattedComments$TextEmojiDeleted <- gsub(FormattedComments$TextEmojiDeleted, pattern = "\\\n", replacement = " ")

# display first 50 values of the variable we will use for these analyses
head(FormattedComments$TextEmojiDeleted, n = 50)

# tokenize the comments (i.e., split them up into individual words)
# This step also simplifies the text by:
#       removing all numbers
#       removing all punctuation
#       removing all non-character symbols
#       removing all hyphens
#       removing all URLs

# for more information and options check: https://www.rdocumentation.org/packages/quanteda/versions/1.4.0/topics/tokens
toks <- tokens(char_tolower(FormattedComments$TextEmojiDeleted),
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_separators = TRUE,
               remove_symbols = TRUE,
               remove_hyphens = TRUE,
               remove_url = TRUE)

# Next, we build a document frequency matrix and remove stopwords (for more information see:
# https://en.wikipedia.org/wiki/Document-term_matrix and https://en.wikipedia.org/wiki/Stop_words)
# Stopwords are very frequent words that appear in almost all texts (e.g. "a","but","it")
commentsDfm <- dfm(toks, remove = quanteda::stopwords("english"))

# We can display the most frequent words from the comments
TermFreq <- textstat_frequency(commentsDfm)
head(TermFreq, n = 50) # you can pick a different value for n to choose the length of the most frequent words table
# in the printed table, docfreq indicates in how many comments the word appears

# After inspecting the most frequent terms, we might want to exclude certain terms that are not indicative for the comments (e.g. the word "video")
CustomStops <- c("video","oh","d","now","get","go","xd", "youtube") # This is just an example, you can (and should) create your own list for each video

# We can create another document-frequency matrix that excludes the custom stopwords that we just defined
commentsDfm <- dfm(toks, remove = c(quanteda::stopwords("english"),CustomStops))

# Updating Term frequency with removed custom stopwords
TermFreq <- textstat_frequency(commentsDfm)

#### Visualizing the overall frequency

# Sort by reverse frequency order (i.e., from most to least frequent)
TermFreq$feature <- with(TermFreq, reorder(feature, -frequency))

# Plot frequency of 50 most common words
ggplot(head(TermFreq, n = 50), aes(x = feature, y = frequency)) + # you can change n to choose how many words are plotted
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Most frequent words in comments", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")

#### Visualizing the number of comments containing the term at least once

# sort by reverse document frequency order (i.e., from most to least frequent)
TermFreq$feature <- with(TermFreq, reorder(feature, -docfreq))

# plot terms that appear in the highest number of comments
ggplot(head(TermFreq, n = 50), aes(x = feature, y = docfreq)) + # you can change n to choose how many words are plotted
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of comments that each token is contained in", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")


# We can create a Wordcloud with the most frequently used terms
set.seed(12345) # We need to set a random seed first (can be any number). This is necessary if we want the wordcloud to be reproducible.
textplot_wordcloud(dfm_select(commentsDfm, min_nchar=1),
                   random_order=FALSE,
                   max_words=100)

#### Sentiment analysis of comment text ####

# We want to compute sentiment scores per comment. This is done by matching the text strings with a dictionary of word sentiments.
# Depending on the type of video you want to analyze, a different sentiment dictionary might be suitable.
# For this example, we decided to use the AFINN dictionary (http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010)
# For more options, check: https://www.rdocumentation.org/packages/syuzhet/versions/1.0.4/topics/get_sentiment
CommentSentiment <- get_sentiment(FormattedComments$TextEmojiDeleted, method = "afinn")

# summary statistics for sentiment scores per comment
summary(CommentSentiment)

# display comments with a sentiment score below x
x  <- -10
FormattedComments$TextEmojiDeleted[CommentSentiment < x]

# disyplay comments with a sentiment score above x
x <- 10
FormattedComments$TextEmojiDeleted[CommentSentiment > x]

# display most negative/positive comment
FormattedComments$TextEmojiDeleted[CommentSentiment == min(CommentSentiment)]
FormattedComments$TextEmojiDeleted[CommentSentiment == max(CommentSentiment)]


#### Visualizing comment sentiments

# build helper dataframe to distinguish between positive, negative and neutral comments
Desc <- CommentSentiment
Desc[Desc > 0] <- "positive"
Desc[Desc < 0] <- "negative"
Desc[Desc == 0] <- "neutral"
df <- data.frame(FormattedComments$TextEmojiDeleted,CommentSentiment,Desc)
colnames(df) <- c("Comment","Sentiment","Valence")

# display amount of positive, negative, and neutral comments
ggplot(data=df, aes(x=Valence, fill = Valence)) +
  geom_bar(stat='count') +
  labs(title = "Comment Sentiment", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")

# distribution of comment sentiments (dotted line represents mean sentiment of all comments)
ggplot(df, aes(x=Sentiment)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept=mean(Sentiment)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Distribution of Comment Sentiment Scores", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  scale_x_continuous(limits=c(-10,10))


#### Emoji frequency analysis ####

# So far, most research has simply ignored emojis in text as noise that should be removed
# However, emojis do confer emotions and meaning, and give more context to the textual data.
# In the following, we will keep the emojis, analyze their frequency,
# their sentiment, and compare them to the textual context of the comment they appear in.

# First, we need to correctly define missing values in the emoji variable
FormattedComments$Emoji[FormattedComments$Emoji == "NA"] <- NA

# next, we remove spaces at the end of the string
FormattedComments$Emoji <- substr(FormattedComments$Emoji, 1, nchar(FormattedComments$Emoji)-1)

# then we tokenize emoji descriptions (important for comments that contain more than one Emoji)
EmojiToks <- tokens(FormattedComments$Emoji)

# afterwards, we create an emoji frequency matrix, excluding "NA" as a term
EmojiDfm <- dfm(EmojiToks, remove = "NA")

# next, we list the most frequent emojis in the comments
EmojiFreq <- textstat_frequency(EmojiDfm)
head(EmojiFreq, n = 10) # you can pick a different value for n to choose the length of the most frequent Emojis table
# the variable docfreq indicates in how many comments the emoji appears

#### Visualizing by overall frequencies

# Sort by reverse frequency order (i.e., from most to least frequent)
EmojiFreq$feature <- with(EmojiFreq, reorder(feature, -frequency))

# plot
ggplot(head(EmojiFreq, n = 50), aes(x = feature, y = frequency)) + # you can change n to choose how many Emojis are plotted
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Most frequent emojis", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")

# To make the plot prettier, we create mappings to display scatterplot points as the respective emojis
mapping1 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_facewithtearsofjoy",], aes(feature,frequency), emoji = "1f602")
mapping2 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_hamburger",], aes(feature,frequency), emoji = "1f354")
mapping3 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_frenchfries",], aes(feature,frequency), emoji = "1f35f")
mapping4 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_smilingfacewithsunglasses",], aes(feature,frequency), emoji = "1f60e")
mapping5 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_smilingface",], aes(feature,frequency), emoji = "263a")
mapping6 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_fire",], aes(feature,frequency), emoji = "1f525")
mapping7 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_loudlycryingface",], aes(feature,frequency), emoji = "1f62d")
mapping8 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_smilingfacewithheart-eyes",], aes(feature,frequency), emoji = "1f60d")
mapping9 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_rollingonthefloorlaughing",], aes(feature,frequency), emoji = "1f923")
mapping10 <- geom_emoji(data = EmojiFreq[EmojiFreq$feature == "emoji_redheart",], aes(feature,frequency), emoji = "2764")

# Sort by reverse frequency order
EmojiFreq$feature <- with(EmojiFreq, reorder(feature, -frequency))

# Plot 10 most common Emojis using their graphical representation as points in the scatterplot
ggplot(EmojiFreq[1:10], aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "10 most frequent emojis", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  mapping1 +
  mapping2 +
  mapping3 +
  mapping4 +
  mapping5 +
  mapping6 +
  mapping7 +
  mapping8 +
  mapping9 +
  mapping10


#### Visualizing by number of comments containing the emoji at least once

# sort by reverse document frequency order (i.e., from most to least frequent)
EmojiFreq$feature <- with(EmojiFreq, reorder(feature, -docfreq))

# plot
ggplot(head(EmojiFreq,n = 50), aes(x = feature, y = docfreq)) + # you can change n to choose how many Emojis are plotted
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Emojis contained in most comments", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")

# Creating a new frame order by document occurance frequenc rather than overall frequency
NewOrder <- EmojiFreq[order(-EmojiFreq$docfreq),]

# To make the plot prettier, we create mappings to display scatterplot points as the respective emojis
mapping1 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_facewithtearsofjoy",], aes(feature,docfreq), emoji = "1f602")
mapping2 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_hamburger",], aes(feature,docfreq), emoji = "1f354")
mapping3 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_loudlycryingface",], aes(feature,docfreq), emoji = "1f62d")
mapping4 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_fire",], aes(feature,docfreq), emoji = "1f525")
mapping5 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_redheart",], aes(feature,docfreq), emoji = "2764")
mapping6 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_heartsuit",], aes(feature,docfreq), emoji = "2665")
mapping7 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_frenchfries",], aes(feature,docfreq), emoji = "1f35f")
mapping8 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_rollingonthefloorlaughing",], aes(feature,docfreq), emoji = "1f923")
mapping9 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_thumbsup",], aes(feature,docfreq), emoji = "1f44d")
mapping10 <- geom_emoji(data = NewOrder[NewOrder$feature == "emoji_smilingfacewithheart-eyes",], aes(feature,docfreq), emoji = "1f60d")

# Plot 10 Emojis that most comments mention at least once
ggplot(NewOrder[1:10], aes(x = feature, y = docfreq)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 10 emojis contained in most comments", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  mapping1 +
  mapping2 +
  mapping3 +
  mapping4 +
  mapping5 +
  mapping6 +
  mapping7 +
  mapping8 +
  mapping9 +
  mapping10


#### Emoji sentiment analysis ####

# Emojis are often used to confer emotions (hence the name), so they might be a valuable addition
# to assess the sentiment of a comment. The following part of the script is a suggestion how to do this.

# First, we need a dictionary that maps emojis to a specific sentiment score

# import Emoji dictionary (from the lexicon package)
EmojiSentiments <- emojis_sentiment
?emojis_sentiment # view a short description of this dictionary from the documentation for the lexicon package

# Unfortunately, the dictionary only contains 734 different Emojis.
# Those were the most frequently used ones when the study on which the dictionars is based was conducted.
# You can view the emoji sentiment scores online here: http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html

# we have to match the sentiment scores to our descriptions of the emojis, and create a quanteda dictionary object
EmojiNames <- paste0("emoji_",gsub(" ","",EmojiSentiments$name))
EmojiSentiment <- cbind.data.frame(EmojiNames,EmojiSentiments$sentiment,EmojiSentiments$polarity)
names(EmojiSentiment) <- c("word","sentiment","valence")
EmojiSentDict <- as.dictionary(EmojiSentiment[,1:2])

# we then tokenize the emoji-only column in our formatted dataframe
EmojiToks <- tokens(tolower(FormattedComments$Emoji))

# We can now replace the emojis that appear in the dictionary with the corresponding sentiment scores
EmojiToksSent <- tokens_lookup(x = EmojiToks, dictionary = EmojiSentDict)

#### Check how many emojis we can assign sentiment scores to

# total number of emojis in the dataframe
AllEmoji <- unlist(EmojiToksSent)
names(AllEmoji) <- NULL
AllNonNAEmoji <- AllEmoji[AllEmoji!="NA"]
length(AllNonNAEmoji)

# Number of emojis that could not be assigned a sentiment score
length(grep("emoji_",AllNonNAEmoji))

# number of emojis that could be assigned a sentiment score
length(grep("0.",AllNonNAEmoji))

# Percentage of emojis that could not be assigned a sentiment score
(length(grep("emoji_",AllNonNAEmoji))/length(AllNonNAEmoji))*100

# Percentage of emojis that could be assigned a sentiment score
(length(grep("0.",AllNonNAEmoji))/length(AllNonNAEmoji))*100

#### Compute sentiment scores for comments based on emojis used in them

# only keep the assigned sentiment scores for the emoji vector
AllEmojiSentiments <- tokens_select(EmojiToksSent,EmojiSentiment$sentiment,"keep")
AllEmojiSentiments <- as.list(AllEmojiSentiments)

# define custom function to add up sentiment scores of emojis per comment (you have to highlight the whole function and run it as a whole)

AddEmojiSentiments <- function(x){

  x <- sum(as.numeric(as.character(x)))
  return(x)

}

# Apply the function to every comment that contains emojis (only those emojis that have a sentiment score will be used)
AdditiveEmojiSentiment <- lapply(AllEmojiSentiments,AddEmojiSentiments)
AdditiveEmojiSentiment[AdditiveEmojiSentiment == 0] <- NA
AdditiveEmojiSentiment <- unlist(AdditiveEmojiSentiment)

# plot histogram to check distribution of emoji sentiment scores
AES_df <- data.frame(AdditiveEmojiSentiment)
ggplot(AES_df, aes(x = AES_df[,1])) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of summed emoji sentiment scores by comment", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  xlab("Emoji sentiment summed per comment")

# show comments with negative emoji sum scores
EmojiNegComments <- FormattedComments[AdditiveEmojiSentiment < 0,2]
EmojiNegComments[is.na(EmojiNegComments) == F]

EmojiNegEmojis <- FormattedComments[AdditiveEmojiSentiment < 0,5]
EmojiNegEmojis[is.na(EmojiNegEmojis) == F]

# show comments with overly positive emoji sum scores
EmojiPosComments <- FormattedComments[AdditiveEmojiSentiment > 20,2]
EmojiPosComments[is.na(EmojiPosComments) == F]

EmojiPosEmojis <- FormattedComments[AdditiveEmojiSentiment > 20,5]
EmojiPosEmojis[is.na(EmojiPosEmojis) == F]

# correlation between additive emoji sentiment score and text sentiment score
cor(CommentSentiment,AdditiveEmojiSentiment,use="complete.obs")

# plot the relationship
TextEmojiRel <- data.frame(CommentSentiment,AdditiveEmojiSentiment)
ggplot(TextEmojiRel, aes(x = CommentSentiment, y = AdditiveEmojiSentiment)) + geom_point(shape = 1) +
  labs(title = "Scatterplot of comment sentiment and emoji sentiment", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")
  scale_x_continuous(limits=c(-15,15))

# As we can see, there seems to be no relationship between the sentiment scores of the text and the sentiment
# of the used emojis. This can have multiple reasons:
#  - Comments that score very high (positive) on emoji sentiment typically contain very little text
#  - Comments that score very low  (negative) on emoji sentiment typically contain very little text
#  - dictionary based bag-of-words/-emojis sentiment analysis is not perfect - there is a lot of room for error in both metrics
#  - most comment text and emoji sentiments are neutral
#  - emojis are very much context dependent, bur we only consider a single sentiment score for each emoji

# We can try to make our metrics less dependent on the amount of emojis or words in the comments by comparing average sentiment
# per used word and per used emoji for each comment
WordsInComments <- sapply(FormattedComments$TextEmojiDeleted,function(x){A <- strsplit(x," ");return(length(A[[1]]))})
names(WordsInComments) <- NULL

# Compute average sentiment score per word instead of using the overall sum
AverageSentimentPerWord <- CommentSentiment/WordsInComments

# Save a copy of the full vector for later use
FullAverageSentimentPerWord <- AverageSentimentPerWord

# We exclude comments that do not have any words in them
AverageSentimentPerWord <- AverageSentimentPerWord[is.nan(AverageSentimentPerWord) == FALSE]

#### Visualize average comment sentiments per word

# build helper dataframe to distinguish between positive, negative and neutral comments
Desc <- AverageSentimentPerWord
Desc[Desc > 0] <- "positive"
Desc[Desc < 0] <- "negative"
Desc[Desc == 0] <- "neutral"
df <- data.frame(FormattedComments$TextEmojiDeleted[is.nan(FullAverageSentimentPerWord) == FALSE],AverageSentimentPerWord,Desc)
colnames(df) <- c("Comment","Sentiment","Valence")

# display amount of positive, negative, and neutral comments
ggplot(data=df, aes(x=Valence, fill = Valence)) +
  geom_bar(stat='count') +
  labs(title = "Average comment sentiment per word", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")

# distribution of comment sentiments (dotted line representd mean sentiment of all comments)
ggplot(df, aes(x=Sentiment)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept=mean(Sentiment)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Distribution of average comment sentiment per word", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s")
  scale_x_continuous(limits=c(-5,5))

# display most negative/positive comment (by average sentiment score per word)
df$Comment[AverageSentimentPerWord == min(AverageSentimentPerWord)]
df$Comment[AverageSentimentPerWord == max(AverageSentimentPerWord)]

## Compute average emoji sentiment per comment

# define custom function to add up sentiment scores of emojis per comment (you have to highlight the whole function and run it as a whole)

AverageEmojiSentiments <- function(x){

  x <- mean(as.numeric(unlist(x)))
  return(x)

}

# Apply the function to every comment that contains emojis (only those emojis that have a sentiment score will be used)
AverageEmojiSentiment <- lapply(AllEmojiSentiments,AverageEmojiSentiments)

# save a full copy of the vector for later use
FullAverageEmojiSentiment <- unlist(AverageEmojiSentiment)

AverageEmojiSentiment[AverageEmojiSentiment == 0] <- NA
AverageEmojiSentiment <- unlist(AverageEmojiSentiment)

# We exclude comments that do not contain emojis
AverageEmojiSentiment <- AverageEmojiSentiment[is.nan(AverageEmojiSentiment) == FALSE]

# plot histogram to check distribution of emoji sentiment scores
AvES_df <- data.frame(AverageEmojiSentiment)
ggplot(AvES_df, aes(x = AvES_df[,1])) +
  geom_histogram(binwidth = 0.2) +
  labs(title = "Distribution of averaged emoji sentiment scores by comment", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  xlab("Emoji sentiment averaged per comment")

## Correlation

# correlation between averaged emoji sentiment score and averaged text sentiment score
cor(FullAverageSentimentPerWord,FullAverageEmojiSentiment,use="complete.obs")

# plot the relationship
TextEmojiRel <- data.frame(FullAverageSentimentPerWord,FullAverageEmojiSentiment)
ggplot(TextEmojiRel, aes(x = FullAverageSentimentPerWord, y = FullAverageEmojiSentiment)) + geom_point(shape = 1) +
  labs(title = "Averaged sentiment scores for text and emojis", subtitle = "Schmoyoho - OH MY DAYUM ft. Daym Drops \nhttps://www.youtube.com/watch?v=DcJFdCmN98s") +
  scale_x_continuous(limits = c(-5,5)) +
  scale_y_continuous(limits = c(-1,1))

# We do obtain a larger positive correlation with the averaged measures, however visual inspection reveals
# that there is no meaningful linear relationships. The data are clustered around one vertical line and multiple
# horizontal lines. This is likely in large parts due to:

# - skewed distribution of number of emojis per comment and types of emojis used (e.g., using the ROFL emoji exactly once is by far
#   the most common case for this particular video)
# - most common average sentiment per word is zero