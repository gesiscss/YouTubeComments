yt_parse <- function(x){

  ### First, We need to check if the dataframe has 15 columns (videos with replies to comments) or fewer (videos without replies to comments)

  if (dim(x)[2] > 13) {

    # only keep the relevant columns
    x <- x[,c(1,7,10,11,12,13,14)]

    # convert dataframe columns to proper types
    x[,1] <- as.factor(x[,1])
    x[,2] <- as.character(x[,2])
    x[,3] <- as.numeric(x[,3])
    x[,6] <- as.character(x[,6])
    x[,7] <- as.character(x[,7])

    # convert timestamps into proper date-time objects
    Published <- unlist(lapply(as.character(x[,4]),function(x){paste(substr(x,1,10),substr(x,12,19),sep = "-")}))
    x[,4] <- as.POSIXct(Published, format ="%Y-%m-%d-%H:%M:%S ", tz = "UTC")

    Updated <- unlist(lapply(as.character(x[,5]),function(x){paste(substr(x,1,10),substr(x,12,19),sep = "-")}))
    x[,5] <- as.POSIXct(Updated, format ="%Y-%m-%d-%H:%M:%S ", tz = "UTC")


    #### Emojis

    ## convert emoji names to CamelCase (if you want to learn (more) about CamelCase: https://en.wikipedia.org/wiki/Camel_case)
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }

    ## detect and replace emojis in the comments

    ReplaceEM <- function(x) {


      # import emoji list
      EmoticonList <- jis

      ListedEmojis <- as.list(jis[,4])
      CamelCaseEmojis <- lapply(jis$name,simpleCap)
      CollapsedEmojis <- lapply(CamelCaseEmojis,function(x){gsub(" ","",x,fixed=TRUE)})
      EmoticonList[,4]$name <- unlist(CollapsedEmojis)

      # order the list by the length of the string to avoid partial matching of shorter strings
      EmoticonList <- EmoticonList[rev(order(nchar(jis$emoji))),]

      # assign x to a new variable so we can save the progress in the for-loop (see below)
      New <- x

      # rm_default throws a useless warning on each iteration that we can ignore
      oldw <- getOption("warn")
      options(warn = -1)

      # cycle through the list and replace everything
      # we have to add clean = FALSE and trim = FALSE to avoid deleting whitespaces that are part of the pattern

      for (i in 1:dim(EmoticonList)[1]){

        New <- rm_default(New, pattern=EmoticonList[i,3],replacement= paste0("EMOJI_", EmoticonList[i,4]$name, " "), fixed = TRUE, clean = FALSE, trim = FALSE)

      }

      # turn warning messages back on
      options(warn = oldw)

      # output result
      return(New)

    }

    # Create a text column in which emojis are replaced by their textual descriptions

    TextEmoRep <- ReplaceEM(x[,2])

    # Create a text column in which emojis are deleted

    TextEmoDel <- emo::ji_replace_all(x[,2],"")

    # Create a column with only the textual descriptions of emojis for each comment

    ExtractEM <- function(x){

      SpacerInsert <- gsub(" ","[{[SpAC0R]}]", x)
      ExtractEmoji <- rm_between(SpacerInsert,"EMOJI_","[{[SpAC0R]}]",fixed=TRUE,extract = TRUE, clean= FALSE,trim=FALSE,include.markers = TRUE)
      UnlistEmoji <- unlist(ExtractEmoji)
      DeleteSpacer <- sapply(UnlistEmoji,function(x){gsub("[{[SpAC0R]}]"," ",x,fixed=T)})
      names(DeleteSpacer) <- NULL

      Emoji <-paste0(DeleteSpacer,collapse="")
      return(Emoji)

    }

    # Extract and rename emojis
    Emoji <- sapply(TextEmoRep,ExtractEM)

    #### URLs

    # Extract URLs from comments

    Links <- qdapRegex::rm_url(x[,2], extract = TRUE)
    Links <- I(Links)

    #### Combine everything into one dataframe

    a <- cbind.data.frame(x[,1],Emoji)

    df <- cbind.data.frame(x[,1],x[,2],TextEmoRep,TextEmoDel,Emoji,x[,3],Links,x[,4],x[,5],x[,6],x[,7])
    names(df) <- c("Author","Text","TextEmojiReplaced","TextEmojiDeleted","Emoji","LikeCount","URL","Published","Updated","ModerationStatus","CommentID")
    row.names(df) <- NULL


  }

  else {

    # only keep relevant columns
    x <- x[,c(1,7,10,11,12)]

    # convert dataframe columns to proper types
    x[,1] <- as.factor(x[,1])
    x[,2] <- as.character(x[,2])
    x[,3] <- as.numeric(x[,3])

    # convert timestamps into proper date-time objects
    Published <- unlist(lapply(as.character(x[,4]),function(x){paste(substr(x,1,10),substr(x,12,19),sep = "-")}))
    x[,4] <- as.POSIXct(Published, format ="%Y-%m-%d-%H:%M:%S ", tz = "UTC")

    Updated <- unlist(lapply(as.character(x[,5]),function(x){paste(substr(x,1,10),substr(x,12,19),sep = "-")}))
    x[,5] <- as.POSIXct(Updated, format ="%Y-%m-%d-%H:%M:%S ", tz = "UTC")


    #### Emojis

    ## convert emoji names to CamelCase (What is CamelCase? https://en.wikipedia.org/wiki/Camel_case)
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }

    ## detect and replace Emojis in the comments

    ReplaceEM <- function(x) {


      # import emoji List
      EmoticonList <- jis

      ListedEmojis <- as.list(jis[,4])
      CamelCaseEmojis <- lapply(jis$name,simpleCap)
      CollapsedEmojis <- lapply(CamelCaseEmojis,function(x){gsub(" ","",x,fixed=TRUE)})
      EmoticonList[,4]$name <- unlist(CollapsedEmojis)

      # order the list by the length of the string to avoid partial matching of shorter strings
      EmoticonList <- EmoticonList[rev(order(nchar(jis$emoji))),]

      # assign x to a new variable so we can save the progress in the for-loop (see below)
      New <- x

      # rm_default throws a useless warning on each iteration that we can ignore
      oldw <- getOption("warn")
      options(warn = -1)

      # cycle through the list and replace everything
      # we have to add clean = FALSE and trim = FALSE to avoid deleting whitespaces that are part of the pattern

      for (i in 1:dim(EmoticonList)[1]){

        New <- rm_default(New, pattern=EmoticonList[i,3],replacement= paste0("EMOJI_", EmoticonList[i,4]$name, " "), fixed = TRUE, clean = FALSE, trim = FALSE)

      }


      # turn warnings back on
      options(warn = oldw)

      # output result
      return(New)

    }

    # create a text column in which emojis are replaced by their textual descriptions

    TextEmoRep <- ReplaceEM(x[,2])

    # create a text column in which emojis are deleted

    TextEmoDel <- emo::ji_replace_all(x[,2],"")

    # create a column with only the textual descriptions of emojis for each comment

    ExtractEM <- function(x){

      SpacerInsert <- gsub(" ","[{[SpAC0R]}]", x)
      ExtractEmoji <- rm_between(SpacerInsert,"EMOJI_","[{[SpAC0R]}]",fixed=TRUE,extract = TRUE, clean= FALSE,trim=FALSE,include.markers = TRUE)
      UnlistEmoji <- unlist(ExtractEmoji)
      DeleteSpacer <- sapply(UnlistEmoji,function(x){gsub("[{[SpAC0R]}]"," ",x,fixed=T)})
      names(DeleteSpacer) <- NULL

      Emoji <-paste0(DeleteSpacer,collapse="")
      return(Emoji)

    }

    # extract and rename emojis
    Emoji <- sapply(TextEmoRep,ExtractEM)


    #### URLs

    # Extract URLs from comments

    Links <- qdapRegex::rm_url(x[,2], extract = TRUE)
    Links <- I(Links)

    #### Combine everything into one dataframe

    df <- cbind.data.frame(x[,1],x[,2],TextEmoRep,TextEmoDel,Emoji,x[,3],Links,x[,4],x[,5])
    names(df) <- c("Author","Text","TextEmojiReplaced","TextEmojiDeleted","Emoji","LikeCount","URL","Published","Updated")
    row.names(df) <- NULL


  }


  #### Return dataframe

  return(df)

}