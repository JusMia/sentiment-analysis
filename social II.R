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

dane1 <- read.csv("dane_theresamay.csv ", stringsAsFactors = FALSE)
dane2 <- read.csv("dane_oprah.csv ", stringsAsFactors = FALSE)
dane3 <- read.csv("danerdt.csv ", stringsAsFactors = FALSE)
dane4 <- read.csv("dane_joannakrupa.csv ", stringsAsFactors = FALSE)
dane5 <- read.csv("dane_justintrudeau.csv ", stringsAsFactors = FALSE)

dane <-bind_rows(dane1,dane2,dane3,dane4,dane5)


all.tokenized.words <- c()
for (a in 1:nrow(dane))
{
  tokenized.words <- unlist(tokenize_words(dane$text[a]))
  #tokenized.words <- unlist(strsplit(dane$text[a]," "))
  tokenized.words <- gsub("[^A-Za-z1-9'-]","",tokenized.words)
  #tokenized.words <- gsub("[0-9+]","",tokenized.words)
  #tokenized.words<-gsub("[https+]","",tokenized.words)
  #tokenized.words<-gsub("[@+]","",tokenized.words)
  #tokenized.words<-gsub("[<>]","",tokenized.words)
  #tokenized.words<-gsub("[…+]","",tokenized.words)
  #tokenized.words<-gsub("['\'+]","",tokenized.words)
  #tokenized.words<-gsub("['t.co'+]","",tokenized.words)
  #tokenized.words<-gsub("['+/'+]","",tokenized.words)
  #tokenized.words <- gsub("[0-9'-/#:_+?!]","",tokenized.words)
  tokenized.words.help <- list()
  for (a in 1:length(tokenized.words))
  {
    if (nchar(tokenized.words[a]) > 1)
      tokenized.words.help[[a]] <- tokenized.words[a]
  }
  tokenized.words <- unlist(tokenized.words.help)
  all.tokenized.words <- c(all.tokenized.words, tokenized.words)
}
#gathering unique elements

all.tokenized.words<-unique(all.tokenized.words)
#removing stopwords

sw <- stopwords(kind = "en")
all.tokenized.words.helper <- all.tokenized.words[!all.tokenized.words %in% sw]
all.tokenized.words <- all.tokenized.words.helper

stem <- unique(lemmatize_strings(stem_strings(all.tokenized.words)))
length(stem)
#Get lexicons

bing <- get_sentiments('bing')
afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')

#lemmatizing, stimming and getting unique words from lexicons

bing <- unique(lemmatize_words(stem_words(bing$word)))
afinn <- unique(lemmatize_words(stem_words(afinn$word)))
nrc <- unique(lemmatize_words(stem_words(nrc$word)))

#Social_bing<-length(intersect(bing, stem)) / length(stem)
#Social_afinn<-length(intersect(afinn, stem)) / length(stem)
#Social_nrc<-length(intersect(nrc, stem)) / length(stem)

length(intersect(bing, stem)) / length(stem)
length(intersect(afinn, stem)) / length(stem)
length(intersect(nrc, stem)) / length(stem)
length(intersect(intersect(intersect(afinn,nrc),stem),bing))/length(union_all(afinn,nrc,stem,bing))
length(all.tokenized.words)
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
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Lexicons and analized text common parts - percentage results",fill = "grey")
grid.draw(venn.grid)


grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing=bing, Tweets=stem)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", sigdigs = 2, main = "Lexicons and analized text common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing, Tweets=stem)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Lexicons and analized text common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent",fill = "grey")
grid.draw(venn.grid)
grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, fill = "grey")
grid.draw(venn.grid)