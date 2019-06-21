#######################################
library(dplyr)
library(SnowballC)
library(tokenizers)
library(tm)
library(tidytext)
library(pacman)
library(VennDiagram)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(textstem, dplyr)
prince <- read.csv('https://raw.githubusercontent.com/eswaribala/rps_citi_ml_r2018/master/prince_raw_data.csv',
                   stringsAsFactors = FALSE)
all.tokenized.words <- c()

#gsub("[^A-Za-z1-9'-]","","2-fac''ed#$^%")

for (a in 1:nrow(prince))
{
  tokenized.words <- unlist(tokenize_words(prince$text[a]))
  #tokenized.words <- unlist(strsplit(prince$text[a]," "))
  tokenized.words <- gsub("[^A-Za-z1-9'-]","",tokenized.words)
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
    if (nchar(tokenized.words[a]) > 0)
      tokenized.words.help[[a]] <- tokenized.words[a]
  }
  tokenized.words <- unlist(tokenized.words.help)
  all.tokenized.words <- unique(c(all.tokenized.words, tokenized.words))
}

sw <- stopwords(kind = "en")
all.tokenized.words.helper <- all.tokenized.words[!all.tokenized.words %in% sw]
all.tokenized.words <- all.tokenized.words.helper

all.tokenized.words <- unique(lemmatize_strings(stem_strings(all.tokenized.words)))

# sentiments from lexicon
bing <- get_sentiments('bing')
afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')

bing <- unique(lemmatize_words(stem_words(bing$word)))
afinn <- unique(lemmatize_words(stem_words(afinn$word)))
nrc <- unique(lemmatize_words(stem_words(nrc$word)))

length(bing)
length(afinn)
length(nrc)
length(all.tokenized.words)
#common parts analized text and lexicon


length(intersect(bing, all.tokenized.words)) / length(all.tokenized.words)
 length(intersect(afinn, all.tokenized.words)) / length(all.tokenized.words)
 length(intersect(nrc, all.tokenized.words)) / length(all.tokenized.words)
 length(intersect(intersect(intersect(afinn,nrc),all.tokenized.words),bing))/length(unique(union_all(afinn,nrc,all.tokenized.words,bing)))
 x<- c()
 x<-unique(union_all(afinn,nrc,bing))
 length(setdiff(all.tokenized.words,x))/length(all.tokenized.words)
length(unique(union_all(nrc, afinn, bing,all.tokenized.words )))  # count of all words

#lexicons common parts 
length(intersect(nrc, bing)) / length(union(nrc, bing))
length(intersect(afinn, bing)) / length(union(afinn, bing))
length(intersect(nrc, afinn)) / length(union(nrc, afinn))
length(intersect(intersect(nrc, afinn), bing)) / length(union(union(nrc, afinn), bing))

###common parts analized text and lexicon unique words (stimmed and lemmatized)



####PLOTS

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing, Lyrics=all.tokenized.words)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", sigdigs = 2,  main = "Lyrics and lexicons common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing, Lyrics=all.tokenized.words)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Lyrics and lexicons common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Afinn=afinn)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)
grid.newpage()
example.list = list(Nrc=nrc, Afinn=afinn)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, lyrics = all.tokenized.words)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(bing=bing, lyrics = all.tokenized.words)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(afinn=afinn, lyrics = all.tokenized.words)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Results Common parts",fill = "grey")
grid.draw(venn.grid)
