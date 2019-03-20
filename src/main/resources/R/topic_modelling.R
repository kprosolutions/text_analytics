#Installing and loading the packages
install.packages("topicmodels")
install.packages("tidytext")


library(topicmodels)

docs <- Corpus(DirSource("C:/Users/hp/Desktop/New folder/Module3/data/holmes_stories"))


# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtmr, k = 2, control = list(seed = 1234))
ap_lda

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics


library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
