removeCommonTerms <- function (x, pct) 
{
    stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
        is.numeric(pct), pct > 0, pct < 1)
    m <- if (inherits(x, "DocumentTermMatrix")) 
        t(x)
    else x
    t <- table(m$i) < m$ncol * (pct)
    termIndex <- as.numeric(names(t[t]))
    if (inherits(x, "DocumentTermMatrix")) 
        x[, termIndex]
    else x[termIndex, ]
}

vBlogs <- readLines(paste("./en_US/en_US.blogs.txt", sep=""), skipNul = TRUE)
vTwitters <- readLines(paste("./en_US/en_US.twitter.txt", sep=""), skipNul = TRUE)
vNews <- readLines(paste("./en_US/en_US.news.txt", sep=""), skipNul = TRUE)
sampleBlogs <- sample(vBlogs, 3000)
sampleTwitters <- sample(vTwitters, 3000)
sampleNews <- sample(vNews, 3000)
superSample <- c(sampleBlogs,sampleTwitters,sampleNews)
remove(vBlogs,vTwitters,vNews,sampleBlogs,sampleNews,sampleTwitters)
suppressWarnings(library(qdap))
enTxt <- sent_detect(superSample)

suppressWarnings(library(tm))
enTxt <- removeNumbers(enTxt)
enTxt <- removePunctuation(enTxt)

stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
enTxt = stringr::str_replace_all(enTxt, stopwords_regex, '')

enTxt <- stripWhitespace(enTxt)
enTxt <- tolower(enTxt)
enTxt <- iconv(enTxt, "latin1", "ASCII", sub=" ");
enTxt <- gsub("[^[:alpha:][:space:][:punct:]]", "", enTxt);
dfTxt <- data.frame(enTxt,stringsAsFactors = FALSE)

suppressWarnings(library(NLP))
suppressWarnings(library(RWeka))

onegrams <- NGramTokenizer(dfTxt, Weka_control(min = 1, max = 1))
onegrams <- data.frame(table(onegrams))
onegrams <- onegrams[order(onegrams$Freq, decreasing = TRUE),]

bigrams <- NGramTokenizer(dfTxt, Weka_control(min = 2, max = 2))
bigrams <- data.frame(table(bigrams))
bigrams <- bigrams[order(bigrams$Freq, decreasing = TRUE),]

trigrams <- NGramTokenizer(dfTxt, Weka_control(min = 3, max = 3))
trigrams <- data.frame(table(trigrams))
trigrams <- trigrams[order(trigrams$Freq, decreasing = TRUE),]

quadgrams <- NGramTokenizer(dfTxt, Weka_control(min = 4, max = 4))
quadgrams <- data.frame(table(quadgrams))
quadgrams <- quadgrams[order(quadgrams$Freq, decreasing = TRUE),]

setwd("/Users/debsubhraroy/R/Capstone/final/WordPredictor")

colnames(onegrams) <- c("terms","Freq")
colnames(bigrams) <- c("terms", "Freq")
colnames(trigrams) <- c("terms", "Freq")
colnames(quadgrams) <- c("terms", "Freq")

save(onegrams, file="onegrams.RData")
save(bigrams, file="bigrams.RData")
save(trigrams, file="trigrams.RData")
save(quadgrams, file="quadgrams.RData")

load("onegrams.RData")
load("bigrams.RData")
load("trigrams.RData")
load("quadgrams.RData")
