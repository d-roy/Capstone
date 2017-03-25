#--------------------------------------------------
# R Server Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# load One-Gram, Two-Gram, Three-Gram and Four-Gram Data frame files
# This data is already cleansed with N-Grams frequency in decending order
# The data was convert to lower case, punctuations removed, numbers removed, 
# white spaces removed, non print characters removed

load("onegrams.RData");
load("bigrams.RData");
load("trigrams.RData");
load("quadgrams.RData");
mesg <- as.character(NULL);

#------------------------------------------------------
# This function is used to clean up the input string of
# any foreign character, punctuation or white space
# before it is used to predict the next term
#------------------------------------------------------
CleanInputString <- function(inStr)
{
   # Test sentence
   #inStr <- "This is. the; -  .   use's 12"

   # First remove the non-alphabatical characters
   inStr <- iconv(inStr, "latin1", "ASCII", sub=" ");
   inStr <- gsub("[^[:alpha:][:space:][:punct:]]", "", inStr);

   # Then convert to a Corpus
   inStrCrps <- VCorpus(VectorSource(inStr))

   # Convert the input sentence to lower case
   # Remove punctuations, numbers, white spaces
   # non alphabets characters
   inStrCrps <- tm_map(inStrCrps, content_transformer(tolower))
   inStrCrps <- tm_map(inStrCrps, removePunctuation)
   inStrCrps <- tm_map(inStrCrps, removeNumbers)
   inStrCrps <- tm_map(inStrCrps, stripWhitespace)
   inStr <- as.character(inStrCrps[[1]])
   inStr <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inStr)

   # Return the cleaned resulting input string
   # If the resulting string is empty return empty and string.
   if (nchar(inStr) > 0) {
       return(inStr); 
   } else {
       return("");
   }
}

#---------------------------------------
# Description of the Back Off Algorithm
#---------------------------------------
# To predict the next term of the user specified sentence
# 1. first we use a FourGram; the first three words of which are the last three words of the user provided sentence
#    for which we are trying to predict the next word. The FourGram is already sorted from highest to lowest frequency
# 2. If no FourGram is found, we back off to ThreeGram (first two words of ThreeGram last two words of the sentence)
# 3. If no FourGram is found, we back off to TwoGram (first word of TwoGram last word of the sentence)
# 4. If no TwoGram is found, we back off to OneGram (the most common word with highest frequency)
#
PredNextTerm <- function(inStr)
{
    assign("mesg", "in PredNextTerm", envir = .GlobalEnv)
  
    # Clean up the input string and extract only the words with no leading and trailing white spaces
    inStr <- CleanInputString(inStr);

    # Split the input string across white spaces and then extract the length
    inStr <- unlist(strsplit(inStr, split=" "));
    inStrLen <- length(inStr);

    nxtTermFound <- FALSE;
    predNxtTerm <- as.character(NULL);
    #mesg <<- as.character(NULL);
    # 1. First test the Four Gram using the four gram data frame
    if (inStrLen >= 3 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");

        # Subset the Four Gram data frame 
        searchStr <- paste("^",inStr1, sep = "");
        quadgramsTemp <- quadgrams[grep (searchStr, quadgrams$terms), ];
        
        # Check to see if any matching record returned
        if ( length(quadgramsTemp[, 1]) > 1 )
        {
            predNxtTerm <- quadgramsTemp[1,1];
            nxtTermFound <- TRUE;
            mesg <<- "Next word is predicted using 4-gram."
        }
        quadgramsTemp <- NULL;
    }

    # 2. Next test the Three Gram using the three gram data frame
    if (inStrLen >= 2 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");

        # Subset the Three Gram data frame 
        searchStr <- paste("^",inStr1, sep = "");
        trigramsTemp <- trigrams[grep (searchStr, trigrams$terms), ];
        
        # Check to see if any matching record returned
        if ( length(trigramsTemp[, 1]) > 1 )
        {
            predNxtTerm <- trigramsTemp[1,1];
            nxtTermFound <- TRUE;
            mesg <<- "Next word is predicted using 3-gram."
        }
        trigramsTemp <- NULL;
    }

    # 3. Next test the Two Gram using the three gram data frame
    if (inStrLen >= 1 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- inStr[inStrLen];

        # Subset the Two Gram data frame 
        searchStr <- paste("^",inStr1, sep = "");
        bigramsTemp <- bigrams[grep (searchStr, bigrams$terms), ];
        
        # Check to see if any matching record returned
        if ( length(bigramsTemp[, 1]) > 1 )
        {
            predNxtTerm <- bigramsTemp[1,1];
            nxtTermFound <- TRUE;
            mesg <<- "Next word is predicted using 2-gram.";
        }
        bigramsTemp <- NULL;
    }

    # 4. If no next term found in Four, Three and Two Grams return the most
    #    frequently used term from the One Gram using the one gram data frame
    if (!nxtTermFound & inStrLen > 0)
    {
        predNxtTerm <- onegrams$terms[1];
        mesg <- "No next word found, the most frequent word is selected as next word."
    }

    nextTerm <- word(predNxtTerm, -1);
       
    if (inStrLen > 0){
        dfTemp1 <- data.frame(nextTerm, mesg);
        return(dfTemp1);
    } 

    nextTerm <- "";
    mesg <-"";
    dfTemp1 <- data.frame(nextTerm, mesg);
    return(dfTemp1);
}

msg <- ""
shinyServer(function(input, output) {
        output$prediction <- renderPrint({
            str2 <- CleanInputString(input$inputString);
            strDF <- PredNextTerm(str2);
            input$action;
            msg <<- as.character(strDF[1,2]);
            cat("", as.character(strDF[1,1]))
            cat("\n\t");
            cat("\n\t");
            cat("Note: ", as.character(strDF[1,2]));
            })

        output$text1 <- renderText({
          paste("Input Sentence: ", input$inputString)});
        
        output$text2 <- renderText({
          input$action;
          #paste("Note: ", msg);
        })
    }
)
