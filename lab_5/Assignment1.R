library(tm)
library(wordcloud)
library(RColorBrewer)
library(viridisLite)
library(plotrix)
library(tidyverse)

five<-read.table("Five.txt",header=F, sep='\n')#Read file
five$doc_id=1:nrow(five)
colnames(five)[1]<-"text"


#Here we interpret each line in the document as separate document
mycorpus <- Corpus(DataframeSource(five)) #Creating corpus (collection of text data)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus,function(x) removeWords(x,stopwords("english")))
   return(corpus)
}


mycorpus <- clean_corpus(mycorpus)
tdm <- TermDocumentMatrix(mycorpus) #Creating term-document matrix
m <- as.matrix(tdm)

#here we merge all rows
v <- sort(rowSums(m),decreasing=TRUE) #Sum up the frequencies of each word
d <- data.frame(word = names(v),freq=v) #Create one column=names, second=frequences
pal <- brewer.pal(6,"Paired")
pal <- pal[-(1:2)] #Create palette of colors
color_pal <- cividis(n = 15)
purple_orange<-brewer.pal(8,"Set2")
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=F, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))

wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=F, rot.per=.15, colors=purple_orange, vfont=c("sans serif","plain"))

ggplot(d[1:6,],aes(x=word,y=freq))+geom_bar(stat="identity",aes(colour=word,fill=word))

commonality.cloud(v,colors="steelblue1",max.words=100)
###############

oneTwo<-read.table("oneTwo.txt",header=F,sep="\n")
oneTwo$doc_id=1:nrow(oneTwo)
colnames(oneTwo)[1]<-"text"

mycor <- Corpus(DataframeSource(oneTwo))

mycor <- clean_corpus(mycor)
tdm1<- TermDocumentMatrix(mycor) #Creating term-document matrix
m1 <- as.matrix(tdm1)

#here we merge all rows
v1 <- sort(rowSums(m1),decreasing=TRUE) #Sum up the frequencies of each word
d1<- data.frame(word = names(v1),freq=v1)



j<-left_join(d,d1,by="word")

top25_df <- j %>%
  # Convert to data frame
  as_data_frame(rownames = "word") %>% 
  # Keep rows where word appears everywhere
  filter_all(all_vars(. > 0)) %>% 
  # Get difference in counts
  mutate(difference = chardonnay - coffee) %>% 
  # Keep rows with biggest difference
  top_n(25, wt = difference) %>% 
  # Arrange by descending difference
  arrange(desc(difference))


col<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)

pyramid.plot(
  j$freq.x[1:10],j$freq.y[1:10],
  # Words
  labels = j$word[1:10], 
  top.labels = c("Words in File", "Common Words", "Words in oneTwo"), 
  main = "Pyramid Plot",gap=17,unit="counts")




########################
gg <- ggplot(data = j[1:10,], aes(x=word))
gg.1 <- gg + 
  geom_bar(aes( y =freq.x , fill = word)) +
  scale_y_continuous('', labels = 'percent') +
  coord_flip() 