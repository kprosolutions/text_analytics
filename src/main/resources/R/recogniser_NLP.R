#Installing the required Packages .
install.packages("rJava")
install.packages("NLP")
install.packages("openNLP")
install.packages("magrittr")
#install.packages("RWeka")
#install.packages("qdap")
install.packages("googleVis")

#Load the required packages 
library(rJava)
library(NLP)
library(openNLP)
library(magrittr)
library(RWeka)
library(qdap)
library (googleVis)
#Read the data
text_data = readLines("C:/Users/hp/Desktop/New folder/Pulwama_attack.txt")

#Removing blank spaces from data
text_data1 = paste(text_data, collapse = "")
print(text_data1)

# converting the text data into string because open NLP can only work with string
text_data1 =as.String(text_data1)

#Tokenization text data using sentennce ,words and POS tokennizer
Sentence_token1 = Maxent_Sent_Token_Annotator()
word_token1 = Maxent_Word_Token_Annotator()
text_annotations = annotate(text_data1,list(Sentence_token1,word_token1))
pos_tag_token = Maxent_POS_Tag_Annotator()
pos_annotation = annotate(text_data1 ,pos_tag_token ,text_annotations)
head(text_annotations)

#Named Entity recognizer
#Person recognition from text
person_ann = Maxent_Entity_Annotator(kind = "person")
location_ann = Maxent_Entity_Annotator(kind = "location")
Organization_ann = Maxent_Entity_Annotator(kind = "organization")
date_ann = Maxent_Entity_Annotator(kind = "date")

Pipeline =list (Sentence_token1,word_token1,pos_tag_token,person_ann,location_ann,
                Organization_ann,date_ann)

Text_ann = annotate(text_data1,Pipeline)
text_doc = AnnotatedPlainTextDocument(text_data1,Text_ann)

sents(text_doc) %>% head(2)

#entities = function(doc,kind){
 # s = doc$content
 # a = annotation(doc)[[1]]
 # if(hasArg(kind)) {
 #   k = sapply(a$features,'[[',"kind")
 #   s[a[k == kind]]
    
 # }else {
 #   s[a[a$type == "entity"]]}
#}

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotation(doc)
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(text_doc, kind = "person")
entities(text_doc, kind = "location")
entities(text_doc, kind = "organization")
entities(text_doc, kind = "date")

#Visualization using googlevision

data = data.frame(table(entities(text_doc,kind = "location")))
loc_vis =gvisColumnChart(data)
plot(loc_vis)

data_date =data.frame(table(entities(text_doc,kind = "date")))
date_vis =gvisColumnChart(data_date)
plot(date_vis)
