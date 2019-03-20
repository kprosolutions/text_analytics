#Installing and loading required packages
install.packages("cleanNLP")
install.packages("dplyr")
install.packages("readr")
install.packages("stringi")
install.packages("ggplot2")
install.packages("tokenizers")
install.packages("ggExtra",dependencies=TRUE)

library(cleanNLP)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
library(tokenizers)
library(rJava)
library(magrittr)
library(ggExtra)


#Read the data
Text = readLines("C:/Users/hp/Desktop/New folder/useR2017_nlp-master/useR2017_nlp-master/data/holmes_stories/14_the_yellow_face.txt")

cnlp_init_tokenizers()
anno <- cnlp_annotate(Text,as_strings = TRUE)
names(anno)
cnlp_get_token(anno)
cnlp_get_entity(anno)

##Sentence boundries

text_tokens %>%
  mutate(sentence_end = word %in% c(".", "?", "!")) %>%
  group_by(id) %>%
  summarize(mean_sentence_len = n() / sum(sentence_end)) %>%
  ggplot(aes(id, mean_sentence_len)) +
  geom_line() +
  geom_point()
stri_wrap(Text1) #use `stri_wrap` just to fit the output on the console plane.

### Splitting with whitespace
stri_split(Text1, fixed = " ")


### Splitting with cleanNLP

cnlp_init_tokenizers(locale = "en")

### Running annotations

Text_anno = cnlp_annotate(Text1, as_strings = TRUE)

##calling the function cnlp_get_token on the annotation object to combine above list in a table

text_tokens = cnlp_get_token(Text_anno)

## cleanNLP tokenization results
text_tokens$word


