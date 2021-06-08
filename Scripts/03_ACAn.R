

# Initial WoS search 

# Broad literature search in Web of Science. Literature search was done on June 2021 using the term "range expansion" as a search term. Search term was set to match Article Titles, Article Abstracts, and Article smartKeyWords (WoS). 

# The search is done manually but exported the results as a csv file. 


readWoS <- lapply(1:7, function(x) 
  readxl::read_xls(paste0("DATA/savedrecs",x,".xls"),
                   sheet = 1))

WoS<- plyr::rbind.fill(readWoS)
dim(WoS) # number of records 

## General bibliometrics


# What has been the frequency of publication? 

png("FIGS/YearPublic.png",
    1000, 
    1000,pointsize = 20)  
par(mar = c(5,6,3,3))
hist(WoS$`Publication Year`, 
     main = "", breaks = 15)
dev.off()
# What are the most journals with the most information 
mostread <-head(sort(colSums(table(WoS$DOI, WoS$`Source Title`)), T), 15)

png("FIGS/MostRead.png",
    1000, 
    1000,pointsize = 20) 
barplot(mostread, las = 2,
        xaxt  = "n", ylab = "# Articles" )
legend("topright",
       bty = "n", fill = "grey",
       legend = names(mostread), 
       cex = 0.7)
dev.off()

# What are the most journals with the most information 
yearTab <-table(WoS$`Source Title`,WoS$`Publication Year`)



png("FIGS/YearDist.png",
    1000, 
    1000,pointsize = 20) 
heatmap(log1p(yearTab), col = c("white", "white","white","white", "white",
                                topo.colors(100)),cexRow = 0.3,scale = "column",
        Colv = NA, Rowv = order(rowSums(yearTab),decreasing = T))
dev.off()

# Automated Content Analysis 

## Annotating text 

# Load a udpipe model in English
library(udpipe)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# annotate titles
AnnotatedTitles <- udpipe_annotate(ud_model, x = WoS$`Article Title`)


AnnotatedTitles <- as.data.frame(AnnotatedTitles)

# annotate abstract
AnnotatedAbstract <- udpipe_annotate(ud_model, x = WoS$Abstract)

AnnotatedAbstract <- as.data.frame(AnnotatedAbstract)

# hard-save cause of time consuming step
#write.csv(AnnotatedTitles, file ="AnnotatedTitles.csv", row.names = F)
#write.csv(AnnotatedAbstract, file ="AnnotatedAbstract.csv", row.names = F)

```

```{r, eval = T, echo = F}
# load hard saved object
AnnotatedTitles <- read.csv("AnnotatedTitles.csv", header = T)
AnnotatedAbstract <- read.csv("AnnotatedAbstract.csv", header = T)
```

Once annotation has been completed, we can start finding patterns in the words.
Let's start with the titles

```{r}
statsTitles <- keywords_collocation(x = AnnotatedTitles, 
                              term = "token",
                              group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 2)



```



