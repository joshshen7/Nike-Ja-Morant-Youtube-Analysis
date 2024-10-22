#Collect data and comments from Nike commercial: "I Get Up | Ja Morant | Nike"

library(tuber)

client_id = ""
client_secret = ""
yt_oauth(client_id, client_secret, token = "")

get_stats(video_id = "1-04z2uxxQ0")
get_video_details(video_id = "1-04z2uxxQ0")

df <- yt_search("Ja Morant Nike")
view(df)
comments <- get_all_comments(c(video_id = "1-04z2uxxQ0"))
view(comments)
write.csv(comments, file = "jamorant.csv")

#Create a word cloud of the comments
library(tm)
library(wordcloud)
library(SnowballC)

comments_corp <- Corpus(VectorSource(comments$textOriginal))
View(comments_corp)

comments_DTM <- DocumentTermMatrix(comments_corp, control = list(removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE))
View(comments_DTM)

as.matrix(comments_DTM[, 1:5])

comments_terms <- colSums(as.matrix(comments_DTM))
comments_terms_matrix <- as.matrix(comments_terms)
View(comments_terms_matrix)

wordcloud(words = names(comments_terms),
          freq = comments_terms,
          vfont = c("serif", "bold italic"),
          colors = brewer.pal(8, "Dark2"))

#Create a barplot of emotions in the comments
install.packages('syuzhet')
library(syuzhet)

video_sentiment <- get_nrc_sentiment(as.character(comments$textOriginal))
video_sentimentDF <- t(data.frame(video_sentiment))
View(video_sentimentDF)

video_comments_emotion <- data.frame(rownames(video_sentimentDF), rowSums(video_sentimentDF > 0))
View(video_comments_emotion)
rownames(video_comments_emotion) = NULL
colnames(video_comments_emotion) = c('Emotion', 'Frequency')
bar_emotion <- barplot(video_comments_emotion$Frequency, 
                           names.arg = video_comments_emotion$Emotion, 
                           main = "Emotion Frequency of Ja Morant Nike Ad", 
                           xlab = "Emotion", 
                           ylab = "Frequency", 
                           ylim = c(0, max(video_comments_emotion$Frequency) * 1.1))  

text(x = bar_emotion, 
     y = video_comments_emotion$Frequency, 
     labels = video_comments_emotion$Frequency, 
     pos = 3, # Position above the bars
     cex = 0.8, # Size of the text
     col = "black") # Text color

#Calculate the polarity score of each comment (240 entries)
video_comments_pol <- data.frame(as.character(comments$textOriginal), get_sentiment(as.character(comments$textOriginal)))
colnames(video_comments_pol) <- c('Comments', 'Polarity')
View(video_comments_pol)

# Count comments with polarity > 0 (positive)
positive_count <- sum(video_comments_pol$Polarity > 0)
View(positive_count)

# Count comments with polarity < 0 (negative)
negative_count <- sum(video_comments_pol$Polarity < 0)

# Count comments with polarity == 0 (neutral)
neutral_count <- sum(video_comments_pol$Polarity == 0)
