### 1. Starting To Use Spark With dplyr Syntax

# Load sparklyr
library(sparklyr)

# Connect to your Spark cluster
spark_conn <- spark_connect(master = "local")

# Print the version of Spark
spark_version(sc = spark_conn)

# Disconnect from Spark
spark_disconnect(sc = spark_conn)

# Load dplyr
library(dplyr)

# Explore track_metadata structure
str(track_metadata)

# Connect to your Spark cluster
spark_conn <- spark_connect("local")

# Copy track_metadata to Spark
track_metadata_tbl <- copy_to(spark_conn, track_metadata)

# List the data frames available in Spark
src_tbls(spark_conn)

# Disconnect from Spark
spark_disconnect(spark_conn)  

# Link to the track_metadata table in Spark
track_metadata_tbl <- tbl(spark_conn, "track_metadata")

# See how big the dataset is
dim(track_metadata_tbl)

# See how small the tibble is
object_size(track_metadata_tbl)

# Print 5 rows, all columns
print(track_metadata_tbl, n = 5, width = Inf)

# Examine structure of tibble
str(track_metadata_tbl)

# Examine structure of data
glimpse(track_metadata_tbl)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year)

# Try to select columns using [ ]
tryCatch({
    # Selection code here
    track_metadata_tbl[, c("artist_name", "release", "title", "year")]
  },
  error = print
)

# track_metadata_tbl has been pre-defined
glimpse(track_metadata_tbl)

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year) %>%
  # Filter rows
  filter(year >= 1960, year < 1970)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year) %>%
  # Filter rows
  filter(year >= 1960, year < 1970) %>%
  # Arrange rows
  arrange(artist_name, desc(year), title)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(title, duration) %>%
  # Mutate columns
  mutate(duration_minutes = duration / 60)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(title, duration) %>%
  # Mutate columns
  mutate(duration_minutes = duration / 60) %>%
  # Summarize columns
  summarize(mean_duration_minutes = mean(duration_minutes))

#######################

### 2. Advanced dplyr Usage

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Select columns starting with artist
  select(starts_with("artist"))

track_metadata_tbl %>%
  # Select columns ending with id
  select(ends_with("id"))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Select columns containing ti
  select(contains("ti"))

track_metadata_tbl %>%
  # Select columns matching ti.?t
  select(matches("ti.?t"))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Only return rows with distinct artist_name
  distinct(artist_name)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Count the artist_name values
  count(artist_name, sort = T) %>%
  # Restrict to top 20
  top_n(20)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

results <- track_metadata_tbl %>%
  # Filter where artist familiarity is greater than 0.9
  filter(artist_familiarity > .9)

# Examine the class of the results
class(results)

# Collect your results
collected <- results %>%
  collect()

# Examine the class of the collected results
class(collected)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

computed <- track_metadata_tbl %>%
  # Filter where artist familiarity is greater than 0.8
  filter(artist_familiarity > .8) %>%
  # Compute the results
  compute("familiar_artists")

# See the available datasets
src_tbls(spark_conn)

# Examine the class of the computed results
class(computed)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

duration_by_artist <- track_metadata_tbl %>%
  # Group by artist
  group_by(artist_name) %>%
  # Calc mean duration
  summarize(mean_duration = mean(duration))

duration_by_artist %>%
  # Sort by ascending mean duration
  arrange(mean_duration)

duration_by_artist %>%
  # Sort by descending mean duration
  arrange(desc(mean_duration))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Group by artist
  group_by(artist_name) %>%
  # Calc time since first release
  mutate(time_since_first_release = year - min(year)) %>%
  # Arrange by descending time since first release
  arrange(desc(time_since_first_release))

# Write SQL query
query <- "SELECT * FROM track_metadata WHERE year < 1935 AND duration > 300"

# Run the query
(results <- dbGetQuery(spark_conn, query))

# track_metadata_tbl and artist_terms_tbl have been pre-defined
track_metadata_tbl
artist_terms_tbl

# Left join artist terms to track metadata by artist_id
joined <- left_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")

# How many rows and columns are in the joined table?
dim(joined)

# track_metadata_tbl and artist_terms_tbl have been pre-defined
track_metadata_tbl
artist_terms_tbl

# Anti join artist terms to track metadata by artist_id
joined <- anti_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")

# How many rows and columns are in the joined table?
dim(joined)

# track_metadata_tbl and artist_terms_tbl have been pre-defined
track_metadata_tbl
artist_terms_tbl

# Semi join artist terms to track metadata by artist_id
joined <- semi_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")

# How many rows and columns are in the joined table?
dim(joined)













