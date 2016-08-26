require('wordcloud')
require('biclust')
require('cluster')
require('igraph') 
require('dplyr')
require('scales')
require('SnowballC')
require('RColorBrewer')
require('ggplot2')
require('tm')
require('fpc')


clean_docs=function(docs) {
  docs = tm_map(docs, content_transformer(tolower))
  docs = tm_map(docs, removeNumbers)
  docs = tm_map(docs, removePunctuation)
  docs = tm_map(docs, removeWords, stopwords("english"))
  docs = tm_map(docs, stemDocument)
  docs = tm_map(docs, stripWhitespace)
  docs = tm_map(docs, PlainTextDocument)
  docs
}


cos.sim=function(ma, mb){
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}

setwd("/Users/vishaljuneja/Desktop/EDAV/Project5/textViz/")
mypwd = "/Users/vishaljuneja/Desktop/EDAV/Project5/textViz/data"

docs = Corpus(DirSource(mypwd))
docs = Corpus(VectorSource(strsplit(paste(docs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))

speeches = docs[[1]]$content
speeches = strsplit(speeches, "\n")
speeches = speeches[[1]]
speeches = speeches[speeches!=""]

df = data.frame(speeches)
df = data.frame(do.call('rbind', strsplit(as.character(df$speeches), "," ,fixed=TRUE)))

colnames(df) = c("president","speech","month","year")

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

df = apply(df, 2, trim)
df = df[-1,]

ident = paste(df[,1], df[,4], sep="_")
df = cbind(df, ident)

docs = docs[-1]
docs = clean_docs(docs)

dtm <- DocumentTermMatrix(docs)
dtm

tdm <- TermDocumentMatrix(docs)
tdm

names(docs) = df[,5]
tdmss = removeSparseTerms(tdm, 0.75)


## read hitler's speeches

hitd = "/Users/vishaljuneja/Desktop/EDAV/Project5/textViz/hitler"
hdocs = Corpus(DirSource(hitd))
hdocs = Corpus(VectorSource(strsplit(paste(hdocs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))

hdocs = clean_docs(hdocs)

combined = c(docs, hdocs)
sp = c(df[,5], "Hitler")
names(combined) = sp


htdm = TermDocumentMatrix(combined)
htdmss = removeSparseTerms(htdm, 0.75)
htdmss$dimnames

m = as.matrix(htdmss)

m = m[,180:225]
c = cos.sim(t(m), t(m))
scores = c[46,]
scores = scores[order(scores)]

plot(scores)

out = data.frame(cbind(names(scores), scores))
out$scores = as.numeric(as.character(out$scores))
o = out[36:46,]
ggplot(o) + 
  geom_bar(aes(x=reorder(V1, -scores), y=scores, fill="blue", alpha=0.5), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## compare speeches

compare_speeches = function(dir_name, title) {
  hitd = paste("/Users/vishaljuneja/Desktop/EDAV/Project5/textViz/", dir_name, sep="")
  print(hitd)
  
  hdocs = Corpus(DirSource(hitd))
  hdocs = Corpus(VectorSource(strsplit(paste(hdocs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))
  
  hdocs = clean_docs(hdocs)
  
  combined = c(docs, hdocs)
  sp = c(df[,5], title)
  names(combined) = sp
  
  
  htdm = TermDocumentMatrix(combined)
  htdmss = removeSparseTerms(htdm, 0.75)
  htdmss$dimnames
  
  m = as.matrix(htdmss)
  
  m = m[,180:225]
  c = cos.sim(t(m), t(m))
  scores = c[46,]
  scores = scores[order(scores)]
  scores
  
  plot(scores)
  
  out = data.frame(cbind(names(scores), scores))
  out$scores = as.numeric(as.character(out$scores))
  o = out[36:46,]
  ggplot(o) + 
    geom_bar(aes(x=reorder(V1, -scores), y=scores, alpha=0.5), stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

compare_speeches("gandhi", "Gandhi")

