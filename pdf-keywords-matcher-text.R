##script takes PDF documents in "docs" subfolder and searches them for keywords specified in criteria.csv file

setwd('/temp/') ## set wd to the correct folder: replace this with the location on your system

## based loosely on https://www.tidytextmining.com/tidytext.html

##install necessary packages (only needed the first time this is run)
install.packages(c("pdftools","dplyr","tidytext","stringr"))
install.packages("knitr") #to export to html, if you want to do that last step

##examples for debugging, commented out

#pdf_file <- "Sri Lanka 6th National Report.pdf"
#pdf_file <- "docs/Ireland-nr-06-en.pdf"
#based on https://masalmon.eu/2019/02/11/trump-schedule/
# no plotical statement made or implied
# also https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html

#load libraries
library(pdftools) #pdf operations
library(dplyr)  #data wrangling
library(tidytext) #dealing with multiple word search and tokening

library(knitr)
library(stringr)


#custom function for finding words 
findWords <- function(unnest,wordList,ignorecase,fullwordmatch) {
  ret <- rep(1,length(unnest$word))
  start.char <- ifelse(fullwordmatch ==TRUE,"^","")
  end.char <- ifelse(fullwordmatch ==TRUE,"$","")

  for (i in 1:length(wordList)) { #loop through search terms
    tempList <- append(unnest$word[i:length(unnest$word)],rep("",i-1))  

    ret <- ret *  str_detect(tempList ,regex(paste(start.char,wordList[i],end.char,sep=""),ignore_case=ignorecase))
  }
  return(ret)
}



## when not image based, can parse by text directly:
results <- NULL
all.files<- list.files(path = "docs", pattern = "*.pdf")

#get list of words to search for 
##format of csv file with header and two example rows.  Fuzzy match allows partial word matching, FALSE means the entire word has to match:
#words,caseSensitive,fuzzyMatch
#cloth,FALSE,TRUE
#garden,FALSE,TRUE
wordList.df <- read.csv("criteria.csv")

#tell the user the basic stats for the search

cat(paste(length(all.files),"files to be inspected for",nrow(wordList.df),"keywords\n"))

##iterate through docs
for (f in 1:length(all.files)) {
  pdf_file<-paste("docs/",all.files[f],sep="")
  p2.all <- pdf_text(pdf_file)

  ##text mining with dplyr:
  ## see also https://www.tidytextmining.com/tidytext.html
  
  
  text_df <- tibble(page = 1:length(p2.all), text = p2.all)
  p2.all.tok <- text_df %>% unnest_tokens(word, text,to_lower = FALSE)



	for (w in 1:nrow(wordList.df)) {
	  cat(paste("starting word list ",wordList.df$words[w],"on file",pdf_file,'\n'))
	  #get word set to search for this time around
	  word.list<-strsplit(as.character(wordList.df$words[w])," ")[[1]]
	  ##find the words in this scope
	  oneMatch<-findWords(p2.all.tok,word.list,!wordList.df$caseSensitive[w],!wordList.df$fuzzyMatch[w])
	  
	 if (sum(oneMatch)>0) {
	   #found something
	  p2.all.tok.oneMatch<-cbind(p2.all.tok,oneMatch)
	  matches<-   subset(p2.all.tok.oneMatch,oneMatch==1)
	  matches$search <- as.character(wordList.df$words[w])
	  matches$context <- ""
	  matches$file <- pdf_file
	  for (p in 1:nrow(matches)) {
		 row.id<-as.numeric(row.names(matches)[p])
		 ##get context so user can see something about the match
		 matches[p,]$context <- paste(p2.all.tok$word[(row.id-10):(row.id + 10 + length(word.list))],collapse = ' ')
	  } 
	  if (length(results) ==0) {
		##results is the current set of matches
		results <- matches
	   ## cat(paste("init matches of row count ",nrow(results)))
	  } else {
		##cat(paste("binding ", nrow(results), " to " , nrow(matches)))
		temp<-results
		##add to current set of results
		results <- bind_rows(temp,matches)
	  }
	 } #at least one match of this word list
	} ## loop through each word list
	##results

	} #loop through each file
##results

##sort the results
results.to.write <- select(results,-oneMatch,-word) %>% arrange(file,page,search)
## add html link to pdf
results.to.write$link <-paste("<a href='",results.to.write$file,"#page=",results.to.write$page,"'>link</a>",sep="")

##write as csv
write.csv(results.to.write,"results.csv")


## loop and gsub each one, an apply function would probably be more elegant, but this works
results.to.write$newContext <- "TO FILL IN"
for (r in 1:nrow(results.to.write)) {
  results.to.write$newContext[r] <- gsub(paste("(",results.to.write$search[r],")",sep=""),"<b style='background-color:yellow'>\\1</b>",results.to.write$context[r],TRUE)
}

##write html
write(kable(results.to.write %>% select(-context),"html",escape=FALSE),file="results.html")

##summarise results
results.to.write.summ.pre<- results.to.write
results.to.write.summ.pre$fileOnly<- substring(results.to.write.summ.pre$file,6)

results.to.write.summ <- results.to.write.summ.pre %>% group_by(fileOnly) %>% summarise(countWords=n())

all.files.df <- as.data.frame(as.character(all.files))
names(all.files.df) <- "fileOnly"

results.to.write.summ2 <- left_join( all.files.df, results.to.write.summ)

write.csv(results.to.write.summ2,"results_summary.csv")

