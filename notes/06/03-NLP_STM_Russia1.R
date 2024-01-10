# Loading necessary libraries
library(readr)        # For reading CSV files
library(dplyr)        # For data manipulation
library(stm)          # For Structural Topic Models
library(lubridate)    # For date-time manipulation
library(tidyr)        # For tidying data
library(tictoc)       # For measuring execution time
library(tm)           # For text mining

# Setting a seed for reproducibility
set.seed(123)

# Disabling scientific notation for numbers
options(scipen=100)

# Reading CSV files containing reviews
reviews_before <- readr::read_csv("/path/to/reviews_before.csv")
reviews_after <- readr::read_csv("/path/to/reviews_after.csv")

# Adding a new column 'sanc' to differentiate the reviews
reviews_before$sanc <- 0  # For reviews before a certain event
reviews_after$sanc <- 1   # For reviews after a certain event

# Combining both datasets into one
reviews <- rbind(reviews_before, reviews_after)

# Creating a sample from the combined reviews data
rev_sample <- reviews

# Processing the text data: removing stopwords, html tags, and doing other preprocessing
processed <- textProcessor(rev_sample$text, 
                           metadata = rev_sample,
                           customstopwords = c("ukranian", "russian", "will", "can", "nhttps", "putin", "ukraine", "http", "russia", "zelensky"),
                           sparselevel = 0.99,
                           striphtml = TRUE)

# Preparing documents for topic modeling: creating a document-term matrix and filtering extremes
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta,
                     lower.thresh = 0.01 * length(processed$documents),
                     upper.thresh = 0.99 * length(processed$documents))

# Assigning the processed documents, vocabulary, and metadata to variables
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Creating the Structural Topic Model with 10 topics, using Spectral initialization
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence = ~ sanc,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

# Finding and printing the most representative texts for certain topics
findThoughts(First_STM, texts = meta$text,
             n = 2, topics = c(6,1))


# Determining a range of values for the number of topics to test in the topic model
k <- seq(10, 50, by=2)

# Estimating the effect of the 'date' covariate on the topics in the STM model
predict_topics <- estimateEffect(formula = 1:10 ~ date, 
                                 stmobj = First_STM, 
                                 metadata = out$meta, 
                                 uncertainty = "Global")

# Plotting the effect of the 'sanc' variable on selected topics
plot(predict_topics, covariate = "sanc", 
     topics = c(3, 5, 9, 1, 4),
     model = First_STM, method = "continuous", # For categorical, use "difference"
     xlab = "Sanc-pre-after",
     main = "Effect of Follower Count on Topics",
     labeltype = "custom",
     ci.level = 0.95, # Default is 0.95
     nsims = 100, # Default is 100
     custom.labels = c('Topic 3', 'Topic 5', 'Topic 9', 'Topic 1', 'Topic 4'))

# Loading necessary libraries for interactive visualization
library(LDAvis)
library(servr)

# Estimating effects for visualization
prep <- estimateEffect(1:10 ~ sanc, First_STM, meta=out$meta, 
                       uncertainty="Global")

# Saving the plot as a JPEG file
jpeg(file="sanctions_before_after.jpeg")
plot(prep, covariate="sanc", topics=1:10, model=First_STM, 
     method="difference", cov.value1="1", cov.value2="0",
     xlab="Before Sanctions ... After Sanctions", main="Effect of Sanctions",
     xlim=c(-.15,.15), labeltype ="custom", custom.labels=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')) 
dev.off()

# Saving a JPEG file with labels of the top topics
jpeg(file="topics.jpeg")
plot(First_STM, type="labels", topics=1:10)
dev.off()