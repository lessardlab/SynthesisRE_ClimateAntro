library(udpipe)
WoS <- read.csv("DATA/AbstractsRangeExpansions.csv", header = T)

dim(WoS)
selectedJournals <- unique(WoS$`Source Title`)[c(2,6,10,11,12,14,16,18,20,21,25,31,32,33,
  34,35,36,37,38,39,40,41,
  51,55,71,78,88,102,103,104,106,109,111,116,130,133,145,148,149,155,163,164,167,184,191,208,212,220)]

dim(WoS)
subWoS <- WoS[WoS$`Source Title` %in% selectedJournals,]
dim(subWoS)


# function to annotate text
annotText <- function(WoS, ud_model){
  ud_model <- udpipe_load_model(ud_model$file)
  AnnotatedTitles <- udpipe_annotate(ud_model, x = WoS$`Article Title`)
  AnnotatedAbstract <- udpipe_annotate(ud_model, x = WoS$Abstract)
  
  return(list("Titles" = as.data.frame(AnnotatedTitles),
              "Abstract" = as.data.frame(AnnotatedAbstract)))
  
}


# assign elements to groups of similar size
groupBy <-data.frame(table(subWoS$`Publication Year`),
           "group" = as.numeric(cut(cumsum(table(subWoS$`Publication Year`)),5)))
# add info to dataset
subWoS$group <- groupBy$group[match(subWoS$`Publication Year`, groupBy$Var1)]

# Load a udpipe model in English
ud_model <- udpipe_download_model(language = "english")

At1 <- lapply(1:5, 
              function(i) 
                annotText(subWoS[which(subWoS$group == i),],
                          ud_model))



## Collocation of word titles (words following one another)
#====
statsTitles <- lapply(1:5, function(i)
  keywords_collocation(x = subset(At1[[i]]$Titles,
                                  upos %in% c("NOUN")), 
                              term = "lemma",
                              group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 2))

Kwrd <- lapply(1:5, function(i)
  statsTitles[[i]][order(statsTitles[[i]]$freq, decreasing = T),])

Kwrd <- lapply(1:5, function(i)
  Kwrd[[i]][!Kwrd[[i]]$keyword %in% c("range expansion", "Range expansion"),])

period <- aggregate(as.numeric(as.character(groupBy$Var1)), by = list(groupBy$group), range)
period <- data.frame(period$x)
colseq <- RColorBrewer::brewer.pal(5, "Set2")
for(i in 1:5){
  png(paste0("FIGS/Wc",i,".png"),
      1000,
      1000,pointsize = 20)
  par(mar = c(0,0,0,0))

  wordcloud::wordcloud(Kwrd[[i]]$keyword,
                       freq = Kwrd[[i]]$freq,
                       color = colseq[i],
                       min.freq = 3)
  
  dev.off()
  
}

png("FIGS/legWd.png", 
    500,500, pointsize = 20)
plot(0, col = "white", frame = F, axes=F, xlab = "", ylab = "")
legend("top",legend = paste(period$X1,"-", period$X2), fill = colseq )
dev.off()
library(magick)

image_write(
  image_append(c(image_append(c(
  image_scale(image_trim(image_read("FIGS/Wc1.png")), geometry = "500x500"),
  image_scale(image_trim(image_read("FIGS/Wc2.png")),geometry="500x500"),
  image_scale(image_trim(image_read("FIGS/Wc3.png")),geometry="500x500"))),
  image_append(
    c(image_scale(image_trim(image_read("FIGS/Wc4.png")),geometry= "500x500"),
      image_scale(image_trim(image_read("FIGS/Wc5.png")),geometry= "500x500"),
      image_read("FIGS/legWd.png")))), T), "FIGS/WordCloud1.png")
#====
## Co-occurrences: How frequent do words occur in the same abstract sentence, 
#====
# in this case only nouns or adjectives
# make a function to compute coocurrences 
GetWDNet <- function(At1){
  
  statsAbstract <- cooccurrence(x = subset(At1$Abstract,
                                           upos %in% c("NOUN")), 
                                term = "lemma", 
                                group = c("doc_id", "paragraph_id", "sentence_id" ),
                                ngram_max = 6)
  
  MostCoorc <- statsAbstract[1:100,]
  
  
  # prune the cooccurrences
  MostCoorc <- MostCoorc[!paste0(MostCoorc$term1,MostCoorc$term2) 
                         %in% paste0(MostCoorc$term2,MostCoorc$term1),]
  
  # create a matrix of words
  WNET <- xtabs(cooc~term1+term2, MostCoorc)
  # create a network of terms 
  WNET <- igraph::graph_from_edgelist(
    as.matrix.data.frame(MostCoorc[,1:2]),
    directed = T)
  
  return(list("net" = WNET,
              "cooc" =MostCoorc ))
  
}

NetYearWd <- lapply(1:5, function(x) GetWDNet(At1[[x]]))


# plot the networks

colseq1 <- RColorBrewer::brewer.pal(5, "Set1")

for(i in 1:5){
  png(paste0("FIGS/WordNet",i,".png"),
      2000,2000, pointsize = 10)
  
  
  l <- layout.fruchterman.reingold(NetYearWd[[i]]$net)
  plot(NetYearWd[[i]]$net, 
       layout = l,
       edge.label.font=1,
       edge.curved = T,
       vertex.label.color = colseq1[i],
       vertex.size = 0, 
       edge.arrow.size=0.5,
       vertex.label.cex = 4)
  
  dev.off()
  
}
png("FIGS/legWd1.png", 
    500,500, pointsize = 20)
plot(0, col = "white", frame = F, axes=F, xlab = "", ylab = "")
legend("top",legend = paste(period$X1,"-", period$X2), fill = colseq1)
dev.off()

image_write(
image_append(c(image_append(c(
  image_scale(image_trim(image_read("FIGS/WordNet1.png")), geometry = "500x500"),
  image_scale(image_trim(image_read("FIGS/WordNet2.png")),geometry="500x500"),
  image_scale(image_trim(image_read("FIGS/WordNet3.png")),geometry="500x500"))),
  image_append(
    c(image_scale(image_trim(image_read("FIGS/WordNet4.png")),geometry= "500x500"),
      image_scale(image_trim(image_read("FIGS/WordNet5.png")),geometry= "500x500"),
      image_read("FIGS/legWd1.png")))), T),
"FIGS/WordNetParti.png")
#====


AbstAll <-rbind(At1[[1]]$Abstract,
                At1[[2]]$Abstract,
                At1[[3]]$Abstract,
                At1[[4]]$Abstract,
                At1[[5]]$Abstract)

length(subWoS$Abstract)

# Text mining
#====
library("tm")
library("SentimentAnalysis")
library(RWeka)    

# remove numbers
preAbs <- removeNumbers(subWoS$Abstract)
# prepare corpus
corpus <- tm::VCorpus(tm::VectorSource(preAbs))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, PlainTextDocument)
# function to tokenize ngrams (3)
BigramTokenizer <- function(x) NGramTokenizer(x,
                                              Weka_control(min = 3, max = 3))
# name corpus
names(corpus) <- subWoS$DOI
# create document term matrix
tdm <- tm::TermDocumentMatrix(corpus, 
                              control=list(
                                tokenize=BigramTokenizer,
                                wordLengths = c(1, Inf)))
# create lsa space from dtm
library(lsa)
lsaSpace <- lsa(tdm)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
#hard save
#saveRDS(dist.mat.lsa, "distMat.RDS")

kmClus <- vegan::cascadeKM(dist.mat.lsa, inf.gr = 3,sup.gr = 10)

plot(kmClus$results[2,])

subWoS$Partition <- kmClus$partition[,7]
unique( kmClus$partition[,7])
#====

# Distribution of articles by partition 

colSums(table(subWoS$DOI,subWoS$Partition))

subWoS$`Article Title`[subWoS$Partition == 1]
# Compute the word clouds by partitions
#====
At2 <- lapply(1:9, 
              function(i) 
                annotText(subWoS[which(subWoS$Partition == i),],
                          ud_model))


## Co-occurrences: How frequent do words occur in the same abstract sentence, 
#====
# in this case only nouns or adjectives
# make a function to compute coocurrences 


NetPartWd <- lapply(1:9, function(x) GetWDNet(At2[[x]]))


# plot the networks

colseq1 <- c(RColorBrewer::brewer.pal(8, "Set1"),"darkgreen")

for(i in 1:9){
  png(paste0("FIGS/WordNetA",i,".png"),
      2000,2000, pointsize = 10)
  
  
  l <- layout.fruchterman.reingold(NetPartWd[[i]]$net)
  plot(NetPartWd[[i]]$net, 
       layout = l,
       edge.label.font=1,
       edge.curved = T,
       edge.color = f(NetPartWd[[i]]$cooc$cooc, 5, "Spectral",T),
       vertex.label.color = "black",
       vertex.size = 0, 
       edge.arrow.size=0.5,
       vertex.label.cex = 4)
  
  dev.off()
  
}


image_write(image_append(
  c(image_append(c(
  image_scale(image_trim(image_read("FIGS/WordNetA1.png")), geometry = "500x500"),
  image_scale(image_trim(image_read("FIGS/WordNetA2.png")),geometry="500x500"),
  image_scale(image_trim(image_read("FIGS/WordNetA3.png")),geometry="500x500"))),
  image_append(c(
    image_scale(image_trim(image_read("FIGS/WordNetA4.png")), geometry = "500x500"),
    image_scale(image_trim(image_read("FIGS/WordNetA5.png")),geometry="500x500"),
    image_scale(image_trim(image_read("FIGS/WordNetA6.png")),geometry="500x500"))),
  image_append(
    c(image_append(c(
      image_scale(image_trim(image_read("FIGS/WordNetA7.png")), geometry = "500x500"),
      image_scale(image_trim(image_read("FIGS/WordNetA8.png")),geometry="500x500"),
      image_scale(image_trim(image_read("FIGS/WordNetA9.png")),geometry="500x500"))))
    )), T),"FIGS/WordNetCluster.png")
#====









