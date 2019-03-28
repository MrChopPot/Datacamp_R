### Sentiment Analysis in R

### 1. Fast & dirty: Polarity scoring

# Examine the text data
text_df

# Calc overall polarity score
text_df %$% polarity(text)

# Calc polarity score by person
(datacamp_conversation <- text_df %$% polarity(text, person))

# Counts table from datacamp_conversation
counts(datacamp_conversation)

# Plot the conversation polarity
plot(datacamp_conversation)

# clean_corpus(), tm_define are pre-defined
clean_corpus
#function(corpus){
#  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
#  corpus <- tm_map(corpus, removePunctuation)
#  corpus <- tm_map(corpus, removeNumbers)
#  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee"))
#  corpus <- tm_map(corpus, content_transformer(tolower))
#  corpus <- tm_map(corpus, stripWhitespace)
#  return(corpus)
#}
tm_define

# Create a VectorSource
tm_vector <- VectorSource(tm_define)

# Apply VCorpus
tm_corpus <- VCorpus(tm_vector)

# Examine the first document's contents
content(tm_corpus[[1]])

# Clean the text
tm_clean <- clean_corpus(tm_corpus)

# Reexamine the contents of the first doc
content(tm_clean[[1]])

# clean_text is pre-defined
clean_text

# Create tf_dtm
tf_dtm <- DocumentTermMatrix(clean_text)

# Create tf_dtm_m
tf_dtm_m <- as.matrix(tf_dtm)

# Dimensions of DTM matrix
dim(tf_dtm_m)

# Subset part of tf_dtm_m for comparison
tf_dtm_m[16:20, 2975:2985]

# Examine sb_words
head(sb_words)

# Create expectations
sb_words$expectations <- sb_words %$% 
  {freq[1] / rank}

# Create metrics plot
sb_plot <- mjs_plot(sb_words, x = rank, y = freq, show_rollover_text = F)

# Add 1st line
sb_plot <- mjs_line(sb_plot)

# Add 2nd line
sb_plot <- mjs_add_line(sb_plot, expectations)

# Add legend
sb_plot <- mjs_add_legend(sb_plot, legend = c("Frequency", "Expectation"))

# Display plot
sb_plot

# Example statement
positive <- "DataCamp courses are good for learning"

# Calculate polarity of statement
(pos_score <- polarity(positive))

# Examine conversation
conversation

# Polarity - All
polarity(conversation$text)

# Polarity - Grouped
student_pol <- conversation %$%
  polarity(text, student)

# Student results
scores(student_pol)

# Sentence by sentence
counts(student_pol)

# qdap plot
plot(student_pol)

# Examine the key.pol
key.pol

# Negators
negation.words

# Amplifiers
amplification.words

# De-amplifiers
deamplification.words

# Examine
text

# stressed_out has been pre-defined
head(stressed_out)

# Basic lexicon score
polarity(stressed_out)

# Check the subjectivity lexicon
key.pol[grep("stress", x)]

# New lexicon
custom_pol <- sentiment_frame(positive.words, c(negative.words, "stressed", "turn back"))

# Compare new score
polarity(stressed_out, polarity.frame = custom_pol)

#######################

### 2. Sentiment analysis the tidytext way

