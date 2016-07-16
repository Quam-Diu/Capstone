
# 1. Required libraries
    library(tm)
    library(dplyr)
    library(stringi)
    library(shiny)

# 2. Adquiring data and initial processing
    
    # 2.1 Download and unzip the data file

        biUrl <- "https://github.com/Quam-Diu/Capstone/blob/master/bigram_top_freq.csv?raw=true"
        bigram.df <- read.table(biUrl, header = TRUE, sep = ",")
        
        triUrl <-"https://github.com/Quam-Diu/Capstone/blob/master/trigram_top_freq.Rda?raw=true"
        bigram.df <- load(biUrl)
        
    
    #2.2 Data tyding 
    
    #2.3 Load data as dataframe
        #2.3.1 Initial loading
        bigram.df <- load(biCon)
        trigram.df <- load(triCon)

#3. Server
    shinyServer(function(input, output, session) {
    
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
                    SWb1$word <- tail(strsplit(SWb1$word,split=" ")[[1]],1)
                }
                result <- SWb1
            } else {
                if (firstWord != "" & secondWord != "") {
                    trigram <- paste("^", firstWord, " ", secondWord, " ", sep = "")
                    ftTrigram <- trigram_top[grepl(trigram, trigram_top$word), ]
                    
                    bigram <- paste("\\<", firstWord, " ", secondWord, "\\>", sep = "")
                    ftBigram <- bigram_top[grepl(bigram, bigram_top$word), ]
                    #ftBigram <- ftBigram[which(ftBigram$freq == max(ftBigram$freq)), ]
                    if (nrow(ftBigram) > 0 & nrow(ftTrigram) > 0) {
                        SWt1 <- ftTrigram[which(ftTrigram$freq == max(ftTrigram$freq)), ]
                        SWt1$prob <- SWt1$freq / ftBigram$freq
                        SWt1$word <- tail(strsplit(SWt1$word, split=" ")[[1]],1)
                    } else {
                        unigram <- paste("^", firstWord, " ", sep = "")
                        ftUnigram <- bigram_top[grepl(unigram, bigram_top$word), ]
                        if (nrow(ftBigram) > 0 & nrow(ftUnigram)>0) {
                            SWb2 <- ftBigram[which(ftBigram$freq == max(ftBigram$freq)), ]
                            SWb2$prob <- 0.4 * SWb2$freq / sum(ftUnigram$freq)
                            SWb2$word <- tail(strsplit(SWb2$word,split=" ")[[1]],1)
                        }  
                    }
                }
                if (secondWord != "") {
                    unigram <- paste("^", secondWord, " ", sep = "")
                    ftUnigram <- bigram_top[grepl(unigram, bigram_top$word), ]
                    if (nrow(ftUnigram)>0) {
                        SWb3 <- ftUnigram[which(ftUnigram$freq == max(ftUnigram$freq)), ]
                        SWb3$prob <- 0.4 * SWb3$freq / sum(ftUnigram$freq)
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
        
        inputText <- reactive({input$textInput})
        
    #3.1 User interfaces
        #3.1.1 Countries (Central America and Mexico pre-selected)
            output$words <- renderUI({
                textInput("text", label = h3("Type one or two words..."), 
                          value = "Enter text...")
            })
    
    #3.2 Outputs
        
        #3.2.2 Text explaining the selected indicator
            text <- reactive(output$words)
            
            output$wordRec <- renderText({

            })
            
    

})