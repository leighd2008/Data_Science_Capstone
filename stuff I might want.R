## Clean the data sets

Use TM package to read text in as a list and then clean the text.
A google search for "profanity list" led me to the web site http://www.cs.cmu.edu/~biglou/resources/ from Luis von Ahn's research group at CMU. The web site contained a link to an Offensive/Profane Word List, which I have used here.
```{r clean_corpus}
if(!exists(cordata)){
cordata <- readLines("./datafiles/cordata.txt", warn = FALSE)
}
mycorpus <- Corpus(VectorSource(cordata))
mycorpus <- tm_map(mycorpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
mycorpus <- tm_map(mycorpus, content_transformer(tolower)) # converting to lowercase
mycorpus <- tm_map(mycorpus, content_transformer(removePunctuation),
preserve_intra_word_dashes=TRUE) # removing ponctuation

fileUrl<- "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
temp <- "profanity.txt"
download.file(fileUrl, temp)
profanity <- readLines("profanity.txt")
mycorpus <- tm_map(mycorpus,removeWords, profanity)
mycorpus <- tm_map(mycorpus, content_transformer(removeNumbers)) # removing numbers

## removing URLs 
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(removeURL))

mycorpus <- tm_map(mycorpus, removeWords, stopwords("english")) # removing stop words
mycorpus <- tm_map(mycorpus, stripWhitespace) ## Stripping unnecessary whitespace 

saveRDS(mycorpus, file = "./datafiles/mycorpus.RData")

```

## Tokenization

```{r unigram}

mycorpusmem <- readRDS("mycorpus.RData")
finalcorpus <-data.frame(text=unlist(mycorpus2),stringsAsFactors = FALSE)

fctoken1 <- NGramTokenizer(finalcorpus, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
fctoken1 <- data.frame(table(fctoken1))
fctoken1 <- fctoken1[order(fctoken1$Freq,decreasing = TRUE),]
names(fctoken1) <- c("word1", "freq")
head(fctoken1)
fctoken1$word1 <- as.character(fctoken1$word1)

unigram <- ggplot(data=fctoken1[1:10,], aes(x = word1, y = freq))
unigram <- unigram + geom_bar(stat="identity") + coord_flip() + ggtitle("Unigram Frequency")
unigram <- unigram + geom_text(data = fctoken1[1:10,], aes(x = word1, y = freq, label = freq), hjust=-1, position = "identity")
unigram

#write.csv(fctoken1[fctoken1$freq > 1,],"fctoken1.csv",row.names=F)
#fctoken1 <- read.csv("fctoken1.csv",stringsAsFactors = F)
#saveRDS(fctoken1, file = "fctoken1.RData")

```


```{r bigram}
mycorpusmem <- readRDS("mycorpus.RData")
finalcorpus <-data.frame(text=unlist(mycorpus2),stringsAsFactors = FALSE)

fctoken2 <- NGramTokenizer(finalcorpus, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
fctoken2 <- data.frame(table(fctoken2))
fctoken2 <- fctoken2[order(fctoken2$Freq,decreasing = TRUE),]
names(fctoken2) <- c("word2", "freq")
head(fctoken2)
fctoken2$word2 <- as.character(fctoken2$word2)

bigram <- ggplot(data=fctoken2[1:10,], aes(x = word2, y = freq))
bigram <- bigram + geom_bar(stat="identity") + coord_flip() + ggtitle("Bigram Frequency")
Bigram <- Bigram + geom_text(data = fctoken2[1:10,], aes(x = word1, y = freq, label = freq), hjust=-1, position = "identity")
Bigram

#write.csv(fctoken1[fctoken1$freq > 1,],"fctoken1.csv",row.names=F)
#fctoken1 <- read.csv("fctoken1.csv",stringsAsFactors = F)
#saveRDS(fctoken1, file = "fctoken1.RData")

```