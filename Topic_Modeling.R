### Topic Modeling in R

### 1. Quick introduction to the workflow

# Display the column names
colnames(word_topics)

# Display the probability
word_topics[1, which(colnames(word_topics)=="street")]

# If a punctuation mark is followed by a space, it is removed by unnest_tokens

# Specify the input column
word_freq <- chapters %>% 
  unnest_tokens(output=word, 
                input=text, 
                token="words", 
                format="text") %>% 
  # Obtain word frequencies
  count(chapter, word) 

# Test equality
word_freq %>% filter(word == "after")

dtm <- corpus %>% 
    # Specify the input column
    unnest_tokens(input=text, output=word, drop=TRUE) %>% 
    count(id, word) %>% 
    # Specify the token
    cast_dtm(document=id, term=word, value=n)

mod = LDA(x=dtm, k=2, method="Gibbs", control=list(alpha=1, delta=0.1, seed=10005))

posterior(mod)$topics

# Generate the document-term matrix
dtm <- corpus %>% 
   unnest_tokens(input=text, output=word) %>% 
   count(id, word) %>% 
   cast_dtm(document=id, term=word, value=n)

# Run the LDA for two topics
mod <- LDA(x=dtm, k=2, method="Gibbs",control=list(alpha=1, delta=0.1, seed=10005))

# Retrieve the probabilities of word `will` belonging to topics 1 and 2
tidy(mod, matrix="beta") %>%
  filter(term == "will")

# Make a stacked column chart showing the probabilities of documents belonging to topics
tidy(mod, matrix="gamma") %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(aes(x=document, y=gamma)) + 
  geom_col(aes(fill=topic))

#######################

# 2. Wordclouds, stopwords, and control arguments

# Display column names
colnames(dtm)

# Fit an LDA model for 2 topics using Gibbs sampling
mod <- LDA(x=dtm, k=2, method="Gibbs", 
           control=list(alpha=1, seed=10005, thin=1))

# Convert matrix beta into tidy format and filter on topic number and term
tidy(mod, matrix="beta") %>%
  filter(topic==2, term=="opened")

# Fit LDA topic model using Gibbs sampling for 2 topics
mod2 <- LDA(x=dtm, k=2, method="Gibbs",
           control=list(alpha=1, seed=10005, thin=1))

# Display the probabilities of topics in documents side by side
tidy(mod2, matrix = "gamma") %>% spread(topic, gamma)

# Create the document-term matrix
dtm <- corpus %>%
  unnest_tokens(output=word, input=text) %>%
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n)

# Display dtm as a matrix
as.matrix(dtm)

# Create the document-term matrix with stop words removed
dtm <- corpus %>%
  unnest_tokens(output=word, input=text) %>%
  anti_join(stop_words) %>% 
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n)

# Display the matrix
as.matrix(dtm)

# Perform inner_join with the dictionary table
dtm <- corpus %>%
  unnest_tokens(output=word, input=text) %>%
  inner_join(dictionary) %>% 
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n)

# Display the summary of dtm
as.matrix(dtm)

# Generate the counts of words in the corpus
word_frequencies <- corpus %>% 
  unnest_tokens(input=text, output=word) %>%
  count(word)

# Create a wordcloud
wordcloud(words=word_frequencies$word, 
          freq=word_frequencies$n,
          min.freq=1,
          max.words=10,
          colors=c("DarkOrange", "Blue"),
          random.order=F,
          random.color=F)

# Construct a document-term matrix
dtm <- history %>% 
  unnest_tokens(input=text, output=word) %>% 
    anti_join(stop_words) %>% 
    count(chapter, word) %>% 
    cast_dtm(document=chapter, term=word, value=n)

# Insert the missing arguments
mod <- LDA(x=dtm, k=4, method="Gibbs", 
           control=list(alpha=1, seed=10005))

# Display top 15 words of each topic
terms(mod, k=15)

# Display the structure of the verbs dataframe
str(verbs)

# Construct a document-term matrix
dtm <- history %>% 
  unnest_tokens(input=text, output=word) %>% 
    inner_join(verbs, by=c("word"="past")) %>% 
    count(chapter, word) %>% 
    cast_dtm(document=chapter, term=word, value=n)

# Fit LDA for four topics
mod <- LDA(x=dtm, k=4, method="Gibbs",
          control=list(alpha=1, seed=10005))

# Display top 25 words from each topic
terms(mod, k=25)

# Extract matrix gamma and plot it
tidy(mod, "gamma") %>% 
  mutate(document=as.numeric(document)) %>% 
  ggplot(aes(x=document, y=gamma)) + 
  geom_line(aes(color=factor(topic))) + 
    labs(x="Chapter", y="Topic probability") +
  scale_color_manual(values=brewer.pal(n=4, "Set1"), name="Topic")

# Display wordclouds one at a time
for (j in 1:4) {
  # Generate a table with word frequences for topic j
  word_frequencies <- tidy(mod, matrix="beta") %>% 
    mutate(n = trunc(beta * 10000)) %>% 
    filter(topic == j)

  # Display word cloud
  wordcloud(word = word_frequencies$term, 
            freq = word_frequencies$n,
            max.words = 20,
            scale = c(3, 0.5),
            colors = c("DarkOrange", "CornflowerBlue", "DarkRed"), 
            rot.per = 0.3)
}

#######################

### 3. 


