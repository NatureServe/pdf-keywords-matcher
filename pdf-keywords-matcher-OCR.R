## see also https://www.tidytextmining.com/tidytext.html

setwd('/temp/')

install.packages(c("magrittr","tesseract","magick"))
install.packages(c("pdftools","dplyr","tidytext","stringr"))

install.packages("knitr") #to export to html

#pdf_file <- "Sri Lanka 6th National Report.pdf"
#pdf_file <- "docs/Ireland-nr-06-en.pdf"
#based on https://masalmon.eu/2019/02/11/trump-schedule/
# no political statement made or implied
# also https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html

library(magrittr)
library(pdftools) #pdf operations
library(magick) # imagery
library(tesseract) # ocr
library(dplyr)
library(tidytext)
#library(xlsx)
library(knitr)
library(stringr)

findWords <- function(unnest,wordList,ignorecase,fullwordmatch) {
  ret <- rep(1,length(unnest$word))
  start.char <- ifelse(fullwordmatch ==TRUE,"^","")
  end.char <- ifelse(fullwordmatch ==TRUE,"$","")

  for (i in 1:length(wordList)) { #loop through search terms
    tempList <- append(unnest$word[i:length(unnest$word)],rep("",i-1))  
   ## cat(i)
   ## cat("\n")
   
    ret <- ret *  str_detect(tempList ,regex(paste(start.char,wordList[i],end.char,sep=""),ignore_case=ignorecase))
   ## cat(sum(ret))
   ## cat("\n")

  }
  return(ret)
}






## when not image based, can do it this way:
results <- NULL
all.files<- list.files(path = "ocrdocs", pattern = "*.pdf")
## get all files that didn't get results
## if you are using text matching and didn't find anything, you can use this 
##all.files.df<-read.csv("../results_summary.csv")  %>% filter(is.na(countWords))
##all.files <-  lapply(as.list(all.files.df$fileOnly), as.character)


wordList.df <- read.csv("criteria.csv")


cat(paste(length(all.files),"files to be inspected for",nrow(wordList.df),"keywords\n"))

#get a page of the PDF rendered to an image
#show the image, to see if it worked
## plot(p1)
#set up OCR in english
eng <- tesseract("eng")

## trim test case: 
## all.files.trim <- c(all.files[1:7],all.files[9:12])
## all.files <- all.files.trim
##iterate through docs
for (f in 1:length(all.files)) {
  ## f<-8 test
  cat(paste('starting file ',all.files[f],'\n'))
  pdf_file<-paste("ocrdocs/",as.character(all.files[f]),sep="")
  
 
  p2.all.getpages <- pdf_text(pdf_file)


  for (pg in 1:length(p2.all.getpages)) {
    cat('ocr',pg,'\n')
    p1 <- pdftools::pdf_render_page(pdf_file, page = pg, dpi=600) %>%
     magick::image_read()
    #convert image to text:
    text.ocr <- tesseract::ocr(p1, engine = eng) ##other languages can't be downloaded! 2/12/2020
    # show the text

    text_df_one <- tibble(page=pg,text=text.ocr)
    if (pg==1) {
       text_df <- text_df_one
    } else { ##add it
      text_df <- bind_rows(text_df,text_df_one)
    }
  
  }
cat('have rows in text_df',nrow(text_df),'\n')

p2.all.tok <- text_df %>% unnest_tokens(word, text,to_lower = FALSE)


wordList.use <-wordList.df


for (w in 1:nrow(wordList.use)) {
  cat(paste("starting word list ",wordList.use$words[w],"on file",pdf_file,'\n'))
  word.list<-strsplit(as.character(wordList.use$words[w])," ")[[1]]
  oneMatch<-findWords(p2.all.tok,word.list,!wordList.use$caseSensitive[w],!wordList.use$fuzzyMatch[w])
  
 if (sum(oneMatch)>0) {
  p2.all.tok.oneMatch<-cbind(p2.all.tok,oneMatch)
  matches<-   subset(p2.all.tok.oneMatch,oneMatch==1)
  matches$search <- as.character(wordList.use$words[w])
  matches$context <- ""
  matches$file <- pdf_file
  for (p in 1:nrow(matches)) {
     row.id<-as.numeric(row.names(matches)[p])
     matches[p,]$context <- paste(p2.all.tok$word[(row.id-10):(row.id + 10 + length(word.list))],collapse = ' ')
  } 
  if (length(results) ==0) {
    results <- matches
   ## cat(paste("init matches of row count ",nrow(results)))
  } else {
    ##cat(paste("binding ", nrow(results), " to " , nrow(matches)))
    temp<-results
    results <- bind_rows(temp,matches)
  }
 } #at least one match of this word list
} ## loop through each word list
cat(paste('after',all.files[f],'result count',nrow(results),'\n'))

} #loop through each file
cat(paste('after ALL FILES','result count',nrow(results),'\n'))

##save.image()

#library(xlsx)
#write.xlsx(results, "results.xlsx", sheetName = "MAIN", 
#  col.names = TRUE, row.names = TRUE, append = FALSE)

results.to.write <- select(results,-oneMatch,-word) %>% arrange(file,page,search)
results.to.write$link <-paste("<a href='",results.to.write$file,"#page=",results.to.write$page,"'>link</a>",sep="")


write.csv(results.to.write,"ocr_results.csv")

##just loop and gsub each one!
results.to.write$newContext <- "TO FILL IN"
for (r in 1:nrow(results.to.write)) {
  results.to.write$newContext[r] <- gsub(paste("(",results.to.write$search[r],")",sep=""),"<b style='background-color:yellow'>\\1</b>",results.to.write$context[r],TRUE)
}

write(kable(results.to.write %>% select(-context),"html",escape=FALSE),file="ocr_results.html")
##?kable

results.to.write.summ.pre<- results.to.write
results.to.write.summ.pre$fileOnly<- substring(results.to.write.summ.pre$file,6)

results.to.write.summ <- results.to.write.summ.pre %>% group_by(fileOnly) %>% summarise(countWords=n())

all.files.df <- as.data.frame(as.character(all.files))
names(all.files.df) <- "fileOnly"

results.to.write.summ2 <- left_join( all.files.df, results.to.write.summ)
#results.to.write.summ2

write.csv(results.to.write.summ2,"ocr_results_summary.csv")

