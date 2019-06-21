#######################################
library(dplyr)
library(SnowballC)
library(tokenizers)
library(tm)
library(stringr)
library(tidytext)
library(pacman)
library(VennDiagram)
library(janeaustenr)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(textstem, dplyr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

tidy_books <- unique(tidy_books$word)

sw <- stopwords(kind = "en")
stemming_austen <- tidy_books[!tidy_books %in% sw]
stemming_austen <- unique(lemmatize_strings(stem_strings(tidy_books)))


bing <- get_sentiments('bing')
afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')

bing <- unique(lemmatize_words(stem_words(bing$word)))
afinn <- unique(lemmatize_words(stem_words(afinn$word)))
nrc <- unique(lemmatize_words(stem_words(nrc$word)))

#Austen_bing<-length(intersect(bing, stemming_austen)) / length(stemming_austen)
#Austen_afinn<-length(intersect(afinn, stemming_austen)) / length(stemming_austen)
#Austen_nrc<-length(intersect(nrc, stemming_austen)) / length(stemming_austen)

length(intersect(bing, stemming_austen)) / length(stemming_austen)
length(intersect(afinn, stemming_austen)) / length(stemming_austen)
length(intersect(nrc, stemming_austen)) / length(stemming_austen)
length(intersect(intersect(intersect(afinn,nrc),stemming_austen),bing))/length(unique(union_all(nrc, afinn, bing,stemming_austen )))
x<- c()
x<-unique(union_all(afinn,nrc,bing))
length(setdiff(stemming_austen,x))/length(stemming_austen)
#length(unique(union_all(setdiff(stemming_austen,bing),setdiff(stemming_austen,nrc),setdiff(stemming_austen,afinn))))/length(unique(union_all(nrc, afinn, bing,stemming_austen ))) # austen withou all others


#length(length(setdiff(stemming_austen,bing))+length(setdiff(stemming_austen,nrc))+length(setdiff(stemming_austen,afinn)))/length(unique(union_all(nrc, afinn, bing,stemming_austen )))
#unique((length(intersect(bing, stemming_austen))+length(intersect(afinn, stemming_austen))
#  +length(intersect(nrc, stemming_austen))))/ length(unique(union_all(nrc, afinn, bing,stemming_austen )))
#lexicons common parts 
length(intersect(nrc, bing)) / length(union(nrc, bing))
length(intersect(afinn, bing)) / length(union(afinn, bing))
length(intersect(nrc, afinn)) / length(union(nrc, afinn))
length(intersect(intersect(nrc, afinn), bing)) / length(unique(union_all(nrc, afinn, bing)))

#plots

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing=bing, Austen_texts=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", sigdigs = 2, main = "Lexicons and analized text common parts - percentage results",fill = "grey"
)
grid.draw(venn.grid)

grid.newpage()
example.list = list(NRC=nrc, Afinn=afinn, Bing = bing, Austen_texts=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Lexicons and analized text common parts",fill = "grey")
grid.draw(venn.grid)


grid.newpage()
example.list = list(Bing = bing, Austen_text=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Bing and analized text common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Bing=bing, Austen_text=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main =2, "Bing and analized text common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Austen_text=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Nrc and analized text common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Austen_text=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Nrc and analized text common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Afinn=afinn, Austen_text=stemming_austen)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Afinn and analized text common parts",fill = "grey")
grid.draw(venn.grid)


grid.newpage()
example.list = list(Afinn=afinn, Austen_text=stemming_austen)
venn.grid = venn.diagram(example.list,filename=NULL, print.mode="percent", main = "Afinn and analized text common parts - percentage results", 
                        cat.pos=-10, fill = "grey"
                      )
grid.draw(venn.grid)


#Lexicons common parts plots


grid.newpage()
example.list = list(Nrc=nrc, Afinn=afinn)
venn.grid = venn.diagram(example.list, filename=NULL, main = "NRC and Afinn lexicons common parts",fill = "grey")
grid.draw(venn.grid)
grid.newpage()
example.list = list(Nrc=nrc, Afinn=afinn)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "NRC and Afinn lexicons common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Bing and Afinn lexicons common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Bing and Afinn lexicons common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Bing and NRC lexicons common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Bing and NRC lexicons common parts",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, print.mode="percent", main = "Bing, NRC and Afinn lexicons common parts - percentage results",fill = "grey")
grid.draw(venn.grid)

grid.newpage()
example.list = list(Nrc=nrc, Afinn=afinn, Bing = bing)
venn.grid = venn.diagram(example.list, filename=NULL, main = "Bing, NRC and Afinn lexicons common parts",fill = "grey")
grid.draw(venn.grid)

