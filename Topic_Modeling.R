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

### 3. Named entity recognition as unsupervised classification

# Fit a topic model using LDA with Gibbs sampling
mod = LDA(x=dtm, k=2, method = "Gibbs", 
          control=list(iter = 500, thin = 1,
                       seed = 12345,
                       alpha = NULL)) # alpha = 50/k

# Display topic prevalance in documents as a table
tidy(mod, "gamma") %>% spread(topic, gamma)  

# Fit the model for delta = 0.1
mod <- LDA(x=dtm, k=2, method="Gibbs",
         control=list(iter=500, seed=12345, alpha=1, delta=.1))

# Define which words we want to examine
my_terms = c("loans", "bank", "opened", "pay", "restaurant", "you")

# Make a tidy table
t <- tidy(mod, "beta") %>% filter(term %in% my_terms)

# Make a stacked column chart of word probabilities
ggplot(t, aes(x=term, y=beta)) + geom_col(aes(fill=factor(topic))) +
  theme(axis.text.x=element_text(angle=90))

# Fit the model for delta = 0.5
mod <- LDA(x=dtm, k=2, method="Gibbs",
         control=list(iter=500, seed=12345, alpha=1, delta=.5))

# Define which words we want to examine
my_terms = c("loans", "bank", "opened", "pay", "restaurant", "you")

# Make a tidy table
t <- tidy(mod, "beta") %>% filter(term %in% my_terms)

# Make a stacked column chart
ggplot(t, aes(x=term, y=beta)) + geom_col(aes(fill=factor(topic))) +
  theme(axis.text.x=element_text(angle=90))

# Regex pattern for an entity and word context
p1 <- "( [a-z]+){2}( (St[.] )?[A-Z][a-z]+)+( [a-z]+){2}"

# Obtain the regex match object from gregexpr
m <- gregexpr(p1, text)

# Get the matches and flatten the list
v <- unlist(regmatches(text, m))

# Find the number of elements in the vector
length(v)

# Regex pattern for an entity and word context
p2 <- "( [a-z]+){2}( (St[.] )?[A-Z][a-z]+( (of|the) [A-Z][a-z]+)?)+( [a-z]+){2}"

# Obtain the regex match object from gregexpr
m <- gregexpr(p2, text)

# Get the matches and flatten the list
v <- unlist(regmatches(text, m))

# Find the number of elements in the vector
length(v)

# Print out contents of the `entity_pattern`
entity_pattern

# Remove the named entity from text
v2 <- gsub(entity_pattern, "", v)

# Display the head of v2
head(v2)

# Remove the named entity
v2 <- gsub(entity_pattern, "", v)

# Pattern for inserting suffixes
p <- "\\1_L1 \\2_L2 \\3_R1 \\4_R2"

# Add suffixes to words
context <- gsub("([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+)", p, v2)

# Extract named entity and use it as document ID
re_match <-  gregexpr(entity_pattern, v)
doc_id <- unlist(regmatches(v, re_match))

# Make a data frame with columns doc_id and text
corpus <- data.frame(doc_id = doc_id, text = context, stringsAsFactors = F)

# Summarize the text to produce a document for each doc_id
corpus2 <- corpus %>% group_by(doc_id) %>% 
  summarise(doc = paste(text, collapse=" "))

# Make a document-term matrix
dtm <- corpus2 %>% unnest_tokens(input=doc, output=word) %>% 
  count(doc_id, word) %>% 
  cast_dtm(document=doc_id, term=word, value=n)

# Fit an LDA model for 3 topics
mod <- LDA(x=dtm, k=3, method="Gibbs", 
          control=list(alpha=1, seed=12345, iter=1000, thin=1))

# Create a table with probabilities of topics in documents
topics <- tidy(mod, matrix="gamma") %>% 
  spread(topic, gamma)

# Set random seed for reproducability
set.seed(12345)

# Take a sample of 20 random integers, without replacement
r <- sample.int(n=nrow(corpus2), size=20, replace=FALSE)

# Generate a document-term matrix
train_dtm <- corpus2[-r, ] %>% unnest_tokens(input=doc, output=word) %>% 
  count(doc_id, word) %>% 
  cast_dtm(document=doc_id, term=word, value=n)

# Fit an LDA topic model for k=3
train_mod <- LDA(x=train_dtm, k=3, method="Gibbs",
                control=list(alpha=1, seed=10001,
                             iter=1000, thin=1))

# Get the test row indices
set.seed(12345)
r <- sample.int(n=nrow(corpus2), size=20, replace=FALSE)

# Extract the vocabulary of the training model
model_vocab <- tidy(train_mod, matrix="beta") %>% 
  select(term) %>% distinct()

# Create a table of counts with aligned vocabularies
test_table <- corpus2[r, ] %>% unnest_tokens(input=doc, output=word) %>% 
  count(doc_id, word) %>%
  right_join(model_vocab, by=c("word"="term"))

# Prepare a document-term matrix
test_dtm <- test_table %>% 
  arrange(desc(doc_id)) %>% 
  mutate(doc_id = ifelse(is.na(doc_id), first(doc_id), doc_id),
      n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=doc_id, term=word, value=n)

# Obtain posterior probabilities for test documents
results <- posterior(object=train_mod, newdata=test_dtm)

# Display the matrix with topic probabilities
results$topics

#######################

### 4. How many topics is enough?

# Split the Abstract column into tokens
dtm <- df %>% unnest_tokens(input=Abstract, output=word) %>% 
   # Remove stopwords
   anti_join(stop_words) %>% 
   # Count the number of occurrences
   count(AwardNumber, word) %>% 
   # Create a document term matrix
   cast_dtm(document=AwardNumber, term=word, value=n)

dtm <- df %>% unnest_tokens(input=Abstract, output=word) %>% 
   anti_join(stop_words) %>% 
   # Count occurences within documents
   count(AwardNumber, word) %>%
   # Group the data
   group_by(word) %>% 
   # Filter for corpus wide frequency
   filter(sum(n) >= 10) %>% 
   # Ungroup the data andreate a document term matrix
   ungroup() %>% 
   cast_dtm(document=AwardNumber, term=word, value=n)

# Create a LDA model
mod <- LDA(x=dtm, method="Gibbs", k=3, 
          control=list(alpha=0.5, seed=1234, iter=500, thin=1))

# Retrieve log-likelihood
logLik(mod)

# Find perplexity
perplexity(object=mod, newdata=dtm)

# Display names of elements in the list
names(models[[1]])

# Retrieve the values of k and perplexity, and plot perplexity vs k
x <- sapply(models, '[[', 'k')
y <- sapply(models, '[[', 'perplexity')
plot(x, y, xlab="number of clusters, k", ylab="perplexity score", type="o")

# Record the new perplexity scores
new_perplexity_score <- numeric(length(models))

# Run each model for 100 iterations
for (i in seq_along(models)) {
  mod2 <- LDA(x=dtm, model=models[[i]]$model,
             control=list(iter=100, seed=12345, thin=1))
  new_perplexity_score[i] <- perplexity(object=mod2, newdata=dtm)
}

# Specify the possible values of k and build the plot
k <- 2:10
plot(x=k, y=new_perplexity_score, xlab="number of clusters, k", 
     ylab="perplexity score", type="o")

t <- history %>% 
        # Unnest the tokens
    unnest_tokens(input=text, output=word) %>% 
        # Create a word index column
    mutate(word_index = 1:n()) %>% 
        # Create a document number column
    mutate(document_number = word_index %/% 1000 + 1)

dtm <- t %>% 
    # Join verbs on "word" and "past"
  inner_join(verbs, by=c("word"="past")) %>% 
    # Count word
  count(document_number, word) %>% 
    # Create a document-term matrix
  cast_dtm(document=document_number, term=word, value=n)

# Store the names of documents in a vector
required_documents <- c(" Africa", " Emperor Heraclius", 
                       " Adrianople", " Daniel", " African")

# Convert table into wide format
tidy(mod, matrix="gamma") %>% 
   spread(key=topic, value=gamma) %>% 
   # Keep only the rows with document names matching the required documents
   filter(document %in% required_documents)

# Set up the column names
colnames(seedwords) <- colnames(dtm)

# Set the weights
seedwords[1, "defeated_l2"] = 1
seedwords[2, "across_l2"] = 1

# Fit the topic model
mod <- LDA(dtm, k=3, method="Gibbs",
         seedwords=seedwords,
         control=list(alpha=1, iter=500, seed=1234))

# Examine topic assignment in the fitted model
tidy(mod, "gamma") %>% spread(topic, gamma) %>% 
  filter(document %in% c(" Daniel", " Adrianople", " African"))
