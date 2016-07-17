
# 1. Required libraries
    library(tm)
    library(dplyr)
    library(stringi)
    library(shiny)

# 2. Adquiring data and initial processing
    
    # 2.1 Get data files
        biUrl <- "https://github.com/Quam-Diu/Capstone/blob/master/bigram_top_freq.csv?raw=true"
        bigram_top <- read.table(biUrl, header = TRUE, sep = ",")
        
        triUrl <-"https://github.com/Quam-Diu/Capstone/blob/master/trigram_top_freq.csv?raw=true"
        trigram_top <- read.table(triUrl, header = TRUE, sep = ",")
        
        wcUrl <- "https://github.com/Quam-Diu/Capstone/blob/master/wordCount.csv?raw=true"
        wordCount <- read.table(wcUrl, header = TRUE, sep = ",")

#3. Server
    shinyServer(function(input, output, session) {
    
        #3.1 Function to make the prediction 
        nextWord <- function(firstWord, secondWord="", onlytheBest=FALSE) {
            # Defines a unigram with the first of the given words
            # unigram1 <- paste("^", firstWord, " ", sep = "")
            
            # Step# 1: Defines a bigram with the two given words and searches for it on trigrams
            bigramCount <- wordCount[1,2]
            trigramCount <- wordCount[2,2]
            SWt1 <- data.frame(word="ukn", freq=1, prob=1/trigramCount)
            SWb1 <- data.frame(word="ukn", freq=1, prob=1/bigramCount)
            SWb2 <- data.frame(word="ukn", freq=1, prob=1/bigramCount)
            SWb3 <- data.frame(word="ukn", freq=1, prob=1/bigramCount)
            
            if (firstWord != "" & secondWord == "") {
                unigram <- paste("^", firstWord, " ", sep = "")
                ftUnigram <- bigram_top[grepl(unigram, bigram_top$word), ]
                if (nrow(ftUnigram)>0) {
                    SWb1 <- ftUnigram[which(ftUnigram$freq == max(ftUnigram$freq)), ]
                    SWb1$prob <- 0.4 * SWb1$freq / sum(ftUnigram$freq)
                    SWb1$word <- as.character(SWb1$word)
                    SWb1$word <- tail(strsplit(SWb1$word,split=" ")[[1]],1)
                }
                result <- SWb1
            } else {
                if (firstWord != "" & secondWord != "") {
                    trigram <- paste("^", firstWord, " ", secondWord, " ", sep = "")
                    ftTrigram <- trigram_top[grepl(trigram, trigram_top$word), ]
                    
                    bigram <- paste("\\<", firstWord, " ", secondWord, "\\>", sep = "")
                    ftBigram <- bigram_top[grepl(bigram, bigram_top$word), ]
                    
                    if (nrow(ftBigram) > 0 & nrow(ftTrigram) > 0) {
                        SWt1 <- ftTrigram[which(ftTrigram$freq == max(ftTrigram$freq)), ]
                        SWt1$prob <- SWt1$freq / ftBigram$freq
                        SWt1$word <- as.character(SWt1$word)
                        SWt1$word <- tail(strsplit(SWt1$word, split=" ")[[1]],1)
                    } else {
                        unigram <- paste("^", firstWord, " ", sep = "")
                        ftUnigram <- bigram_top[grepl(unigram, bigram_top$word), ]
                        if (nrow(ftBigram) > 0 & nrow(ftUnigram)>0) {
                            SWb2 <- ftBigram[which(ftBigram$freq == max(ftBigram$freq)), ]
                            SWb2$prob <- 0.4 * SWb2$freq / sum(ftUnigram$freq)
                            SWb2$word <- as.character(SWb2$word)
                            SWb2$word <- tail(strsplit(SWb2$word,split=" ")[[1]],1)
                        }  
                    }
                }
                if (secondWord != "") {
                    unigram <- paste("^", secondWord, " ", sep = "")
                    ftUnigram <- bigram_top[grepl(unigram, bigram_top$word), ]
                    if (nrow(ftUnigram)>0) {
                        SWb3 <- ftUnigram[which(ftUnigram$freq == max(ftUnigram$freq)), ]
                        SWb3$prob <- 0.1 * SWb3$freq / sum(ftUnigram$freq)
                        SWb3$word <- as.character(SWb3$word)
                        SWb3$word <- tail(strsplit(SWb3$word,split=" ")[[1]],1)
                    }
                }
                result <- rbind(SWt1, SWb2, SWb3)
                if (onlytheBest) {
                    result <- result[which(result$prob == max(result$prob)), ][1,]  
                }
            }
            
            return(result)
            
        }
        
        #3.2 Function to get input from the user
        recWord <- function() {
            # Always take the last two words
            num <- length(strsplit(input$words,split=" ")[[1]])
            if (num>1) {
                w1 <- strsplit(input$words,split=" ")[[1]][num-1]
                w2 <- strsplit(input$words,split=" ")[[1]][num]                
            } else {
                w1 <- strsplit(input$words,split=" ")[[1]][1]
                w2 <- ""
            }
            # Check for nulls
            if (!is.na(w1)) {
                    if (is.na(w2)) {w2<- ""}
                    prediction <- nextWord(w1, w2)
                    bestPrediction <- as.character(prediction[which(prediction$prob == max(prediction$prob)), ][1,]$word)
                } else {
                    bestPrediction <- "Enter some text..."     
                }
            return(bestPrediction)  
        }
        
        #3.3 Output result
        output$recWords <- renderText(recWord()) 
            
})