# Data in the Wild–Data Cleaning
Ellen Bledsoe
2025-07-28

This documents cleans the data for the Data in the Wild manuscript
analyses.

## Packages and Data

Load the packages used in data cleaning.

``` r
library(tidyverse)    # for data wrangling
library(readxl)       # read in data
```

Read in the data.

``` r
data <- readxl::read_excel("../data_raw/DataInTheWild_PrePost_ForEllen.xlsx", 
                           col_names = FALSE)
glimpse(data)
```

    Rows: 143
    Columns: 16
    $ ...1  <chr> NA, "Q42", "What term are you taking the course?", "Fall 2022", …
    $ ...2  <chr> "Pre", "Q39", "How important do you think data is for understand…
    $ ...3  <chr> "Post", "Q39", "How important do you think data is for understan…
    $ ...4  <chr> "Pre", "Q15", "How interested are you in data science.", "1", "4…
    $ ...5  <chr> "Post", "Q15", "How interested are you in data science.", "2", "…
    $ ...6  <chr> "Pre", "Q17_1", "Please rank the degree to which you agree or di…
    $ ...7  <chr> "Post", "Q17_1", "Please rank the degree to which you agree or d…
    $ ...8  <chr> "Pre", "Q17_2", "Please rank the degree to which you agree or di…
    $ ...9  <chr> "Post", "Q17_2", "Please rank the degree to which you agree or d…
    $ ...10 <chr> "Pre", "Q17_3", "Please rank the degree to which you agree or di…
    $ ...11 <chr> "Post", "Q17_3", "Please rank the degree to which you agree or d…
    $ ...12 <chr> "Pre", "Q17_4", "Please rank the degree to which you agree or di…
    $ ...13 <chr> "Post", "Q17_4", "Please rank the degree to which you agree or d…
    $ ...14 <chr> NA, "Q5", "What is your gender identity? - Selected Choice", "Wo…
    $ ...15 <chr> NA, "Q6", "With which race/ethnicity do you identify? (choose al…
    $ ...16 <chr> NA, "Q1", "Which institution are you enrolled at?", "Lewis and C…

## Data Cleaning

Remove the row with what all the questions are and save them in case we
need them later.

``` r
# save questions as a vector
questions <- slice(data, 3)

# remove rows with questions and summary stats
data <- data %>% 
  # keep all but the last 3 rows
  slice(1:(n()-3)) %>% 
  # remove the 3rd row
  slice(-3)

head(data)
```

    # A tibble: 6 × 16
      ...1   ...2  ...3  ...4  ...5  ...6  ...7  ...8  ...9  ...10 ...11 ...12 ...13
      <chr>  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    1 <NA>   Pre   Post  Pre   Post  Pre   Post  Pre   Post  Pre   Post  Pre   Post 
    2 Q42    Q39   Q39   Q15   Q15   Q17_1 Q17_1 Q17_2 Q17_2 Q17_3 Q17_3 Q17_4 Q17_4
    3 Fall … 7     4     1     2     1     2     1     2     2     2     1     2    
    4 Fall … 9     10    4     4     4     4     5     5     1     5     3     3    
    5 Sprin… 10    9     3     3     3     2     3     5     5     5     3     1    
    6 Fall … 10    10    3     2     3     4     4     4     5     5     4     2    
    # ℹ 3 more variables: ...14 <chr>, ...15 <chr>, ...16 <chr>

``` r
tail(data)
```

    # A tibble: 6 × 16
      ...1   ...2  ...3  ...4  ...5  ...6  ...7  ...8  ...9  ...10 ...11 ...12 ...13
      <chr>  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    1 Fall … 9     8     3     3     3     2     4     4     5     5     2     2    
    2 Fall … 9     9     3     4     3     4     4     5     3     4     2     2    
    3 Fall … 8     10    3     3     4     3     4     4     5     5     1     4    
    4 Fall … 8     10    3     4     4     5     5     5     5     5     5     5    
    5 Fall … 10    10    5     1     5     1     3     1     5     5     1     1    
    6 Fall … 9     10    3     4     3     5     5     5     4     5     4     5    
    # ℹ 3 more variables: ...14 <chr>, ...15 <chr>, ...16 <chr>

Combine the first two rows to one row to create column names.

``` r
colnames(data) <- c("Term", 
                    "Q39_Pre", "Q39_Post", 
                    "Q15_Pre", "Q15_Post", 
                    "Q17_1_Pre", "Q17_1_Post",
                    "Q17_2_Pre", "Q17_2_Post",
                    "Q17_3_Pre", "Q17_3_Post",
                    "Q17_4_Pre", "Q17_4_Post",
                    "Gender", "Race", "School")
data <- slice(data, -1:-2)

head(data)
```

    # A tibble: 6 × 16
      Term        Q39_Pre Q39_Post Q15_Pre Q15_Post Q17_1_Pre Q17_1_Post Q17_2_Pre
      <chr>       <chr>   <chr>    <chr>   <chr>    <chr>     <chr>      <chr>    
    1 Fall 2022   7       4        1       2        1         2          1        
    2 Fall 2023   9       10       4       4        4         4          5        
    3 Spring 2023 10      9        3       3        3         2          3        
    4 Fall 2022   10      10       3       2        3         4          4        
    5 Fall 2024   9       10       3       4        3         4          4        
    6 Fall 2024   10      7        3       3        3         4          5        
    # ℹ 8 more variables: Q17_2_Post <chr>, Q17_3_Pre <chr>, Q17_3_Post <chr>,
    #   Q17_4_Pre <chr>, Q17_4_Post <chr>, Gender <chr>, Race <chr>, School <chr>

Add a column to identify that each row represents one student.

``` r
data <- data %>% 
  mutate(StudentID = as.character(1:nrow(.)), .before = Term)
```

Create a column for whether the data are pre or post survey.

``` r
data <- data %>% 
  # make the data longer
  pivot_longer(cols = ends_with(c("Pre", "Post")),
               names_to = "Question",
               values_to = "Value") %>% 
  # create two columns--one for the question and one for the survey type
  # this splits based on the last deliminter
  separate_wider_regex(Question, c(Question = ".*", "_", Survey_Type = ".*")) %>% 
  # put back into a wider format with each question as one column
  pivot_wider(names_from = "Question", 
              values_from = "Value")

head(data)
```

    # A tibble: 6 × 12
      StudentID Term   Gender Race  School Survey_Type Q39   Q15   Q17_1 Q17_2 Q17_3
      <chr>     <chr>  <chr>  <chr> <chr>  <chr>       <chr> <chr> <chr> <chr> <chr>
    1 1         Fall … Woman  Hisp… Lewis… Pre         7     1     1     1     2    
    2 1         Fall … Woman  Hisp… Lewis… Post        4     2     2     2     2    
    3 2         Fall … Woman  <NA>  Lewis… Pre         9     4     4     5     1    
    4 2         Fall … Woman  <NA>  Lewis… Post        10    4     4     5     5    
    5 3         Sprin… Woman  Whit… Unive… Pre         10    3     3     3     5    
    6 3         Sprin… Woman  Whit… Unive… Post        9     3     2     5     5    
    # ℹ 1 more variable: Q17_4 <chr>

Convert all of the scale responses to numeric.

``` r
data <- data %>% 
  mutate(across(.cols = starts_with("Q"), .fns = as.numeric))
```

## Save Clean Data

Save the cleaned version of the data as a .CSV file.

``` r
write_csv(data, "../data_clean/data_cleaned.csv")
```
