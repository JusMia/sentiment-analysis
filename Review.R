#######################################
library(dplyr)
library(SnowballC)
library(tokenizers)
library(tm)
library(tidytext)
library(pacman)
library(VennDiagram)
library(ngramrr)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(textstem, dplyr)

review=read.csv("Womens Clothing E-Commerce Reviews.csv", stringsAsFactors = FALSE)
all.tokenized.words <- c()

for (a in 1:nrow(review))
{
  tokenized.words <- unlist(tokenize_words(review$Review.Text[a]))
  tokenized.words <- gsub("[^A-Za-z1-9'-]","",tokenized.words)
  # tokenized.words<-gsub("['http'+]","",tokenized.words)
  #tokenized.words<-gsub("[@+]","",tokenized.words)
  # tokenized.words<-gsub("[<>]","",tokenized.words)
  #tokenized.words<-gsub("[…+]","",tokenized.words)
  #tokenized.words<-gsub("['\'+]","",tokenized.words)
  # tokenized.words<-gsub("['t.co'+]","",tokenized.words)
  # tokenized.words<-gsub("['+/'+]","",tokenized.words)
  # tokenized.words <- gsub("[0-9'-/#:_+?!;]","",tokenized.words)
  tokenized.words.help <- list()
  for (a in 1:length(tokenized.words))
 {
    if (nchar(tokenized.words[a]) > 0)
      tokenized.words.help[[a]] <- tokenized.words[a]
 }
  tokenized.words <- unlist(tokenized.words.help)
  all.tokenized.words <- c(all.tokenized.words, tokenized.words)
}

sw <- stopwords(kind = "en")
all.tokenized.words.helper <- all.tokenized.words[!all.tokenized.words %in% sw]
all.tokenized.words <- all.tokenized.words.helper
stem <- unique(lemmatize_strings(stem_strings(all.tokenized.words)))

#Get lexicons

bing <- get_sentiments('bing')
afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')

#lemmatizing, stimming and getting unique words from lexicons

bing <- unique(lemmatize_words(stem_words(bing$word)))
afinn <- unique(lemmatize_words(stem_words(afinn$word)))
nrc <- unique(lemmatize_words(stem_words(nrc$word)))

length(intersect(bing, stem)) / length(stem)
length(intersect(afinn, stem)) / length(stem)
length(intersect(nrc, stem)) / length(stem)
length(intersect(intersect(intersect(afinn,nrc),stem),bing))/length(union_all(afinn,nrc,stem,bing))

x<- c()
x<-unique(union_all(afinn,nrc,bing))
y<-setdiff(stem,x)
length(y)/length(stem)


#lexicons common parts 
length(intersect(nrc, bing)) / length(union(nrc, bing))
length(intersect(afinn, bing)) / length(union(afinn, bing))
length(intersect(nrc, afinn)) / length(union(nrc, afinn))
length(intersect(intersect(nrc, afinn), bing)) / length(union(union(nrc, afinn), bing))

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing=bing, Reviews=stem)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", sigdigs = 2, main = "Lexicons and analized text common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing, Reviews=stem)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Lexicons and analized text common parts",fill = "grey")
grid.draw(venn.grid)