---
layout: post
title: Detecting speech similarities
date: 2016-08-24 23:15:45 -0400
categories: jekyll update
---

Disclaimer: In this post I am comparing State of the Union (SOTU) addresses with speeches of Hitler, Gandhi and the recently elected Canadian president Justin Trudeau. This is a surface level textual analysis and I am not drawing any personality based comparisons in this post.


This exploratory analysis compares SOTU addresses given by presidents over the years with speeches of other famous/in-famous personalities. Although the overall idea of this and future analysis is to draw comparisons in both speech content and styles; the current analysis is restricted to term frequency based cosine similarity measures. A deeper content analysis will probably require parsing, identifying topics/policies etc. Associating sentiment with those topics could be a dummy/indicator for policy standpoints. Speech style comparisons will require analysis of speech signals and prosody data.

We will do the following:

a) parse the SOTU speech dataset

b) obtain term document frequency matrix

c) remove sparse features

d) draw cosine similarities with speeches of other personalities

All of the analysis is done in R. Full code is [here][code] and the datasets used are [here][data].


Read SOTU data:

{% highlight ruby %}

require('ggplot2')
require('tm')

mypwd = "set path to the datasets ... "

docs = Corpus(DirSource(mypwd))
docs = Corpus(VectorSource(strsplit(paste(docs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))

{% endhighlight %}


Parse meta information at top of the document. Combine president name and speech year.
{% highlight ruby %}

speeches = docs[[1]]$content
speeches = strsplit(speeches, "\n")
speeches = speeches[[1]]
speeches = speeches[speeches!=""]

df = data.frame(speeches)
df = data.frame(do.call('rbind', strsplit(as.character(df$speeches), "," ,fixed=TRUE)))

colnames(df) = c("president","speech","month","year")

# returns string w/o leading or trailing whitespace
trim = function (x) gsub("^\\s+|\\s+$", "", x)

df = apply(df, 2, trim)
df = df[-1,]

ident = paste(df[,1], df[,4], sep="_")
df = cbind(df, ident)
{% endhighlight %}


Remove the meta doc and clean the remaining corpus (stemming, stop word removal etc.)
{% highlight ruby %}
docs = docs[-1]
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

docs = clean_docs(docs)
{% endhighlight %}

<h3>Comparing speeches</h3>

The following R code reads in the speeches of the personality with which the comparison is drawn. It concatenates the read-in corpus with the previously built corpus and calculates cosine similarities with each years address. Finally highest 10 matches are picked and actual similarity measures are plotted.

Put the speeches of a famous personality in the directory "dir_name" as a text document and enter the name of the personality as "pname".

{% highlight ruby %}
compare_speeches = function(dir_name, pname) {
  hitd = paste("path to data directory here ...", dir_name, sep="/")
  print(hitd)
  
  hdocs = Corpus(DirSource(hitd))
  hdocs = Corpus(VectorSource(strsplit(paste(hdocs[[1]]$content,collapse='\n'),"***",fixed=TRUE)[[1]]))
  
  hdocs = clean_docs(hdocs)
  
  combined = c(docs, hdocs)
  sp = c(df[,5], pname)
  names(combined) = sp
  
  
  htdm = TermDocumentMatrix(combined)
  htdmss = removeSparseTerms(htdm, 0.75)
  htdmss$dimnames
  
  m = as.matrix(htdmss)
  
  m = m[,180:225]

  cos.sim=function(ma, mb){
	  mat=tcrossprod(ma, mb)
	  t1=sqrt(apply(ma, 1, crossprod))
	  t2=sqrt(apply(mb, 1, crossprod))
	  mat / outer(t1,t2)
  }
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

{% endhighlight %}

<h3>Comparing with Hitler</h3>
The directory "hitler" in the data folder has the translated version of Hitler’s speeches over the years: starting from his ascendency to the highest office; to his speeches during the world war.

{% highlight ruby %}
compare_speeches("hitler", "Adolf Hitler")
{% endhighlight %}
![Adolf Hitler](/images/hitler.png)

The above graph shows top 10 most similar speeches with those of Hitler’s. As a sanity check Hitler’s speech has a cosine similarity measure of 1 (completely identical) with his own speeches.

We find that most matching speeches were made during war periods e.g. Nixon’s address in 1972 during the Vietnam war, George W. Bush’s speeches in 2003 and 2005 during Afghanistan invasion, George H.W. Bush’s speech in 1991 was again made during the turmoil period in Afghanistan.

<h3>Comparing with Gandhi</h3>
I took Gandhi’s speeches during freedom struggle and after independence and compared them with other presidents.

{% highlight ruby %}
compare_speeches("gandhi", "Mahatma Gandhi")
{% endhighlight %}
![Mahatma Gandhi](/images/gandhi.png)

First thing we note is that general similarity measure is very small compared to similarity with Hitler’s speeches (~ 0.30). From the above plot we find that as in Hitler’s case Gandhi’s speeches are similar to George W. Bush, George H.W. Bush’s speeches as well. Barrack Obama also figures in the top 5. 

The conjecture is that although Bush and Gandhi might both be talking about war, post-war situations and effects of war; their sentiment towards the topics could be different. The similarity measure takes into account only frequency of occurance of words. This means that even though Gandhi and Bush might be talking about the same topics we would not know if there sentiment and opinions about the topics differ.

<h3>Comparing with Justin Trudeau</h3>
We further took recently elected Canadian president’s address to the nation. Justin Trudeau has been applauded for choosing a very diverse, secular and representative cabinet. His address was seen as being progressive and discussed roadmap for advancing Canada on frontiers of technology and innovation. 

Justin Trudeau’s speech seem to be similar in spirit with Barrack Obama’s addresses to the nation.

{% highlight ruby %}
compare_speeches("jt", "Justin Trudeau")
{% endhighlight %}

![Justin Trudeau](/images/jt.png)



PS: Please contact me if anyone is interested in taking this analysis further!!


[data]: https://github.com/viishaal/viishaal.github.io/tree/master/data/SOTU
[code]: https://github.com/viishaal/viishaal.github.io/blob/master/code/topics.R

