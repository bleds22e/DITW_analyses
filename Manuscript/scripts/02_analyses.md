# Data in the Wild–Analyses


This document contains the analyses for the Data in the Wild course.

## Packages and Data

Load the packages used in analyses.

``` r
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
```

Read in the cleaned dataset.

``` r
data <- read_csv("../data_clean/data_cleaned.csv")
```

    Rows: 274 Columns: 12
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (5): Term, Gender, Race, School, Survey_Type
    dbl (7): StudentID, Q39, Q15, Q17_1, Q17_2, Q17_3, Q17_4

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(data)
```

    Rows: 274
    Columns: 12
    $ StudentID   <dbl> 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, …
    $ Term        <chr> "Fall 2022", "Fall 2022", "Fall 2023", "Fall 2023", "Sprin…
    $ Gender      <chr> "Woman", "Woman", "Woman", "Woman", "Woman", "Woman", "Wom…
    $ Race        <chr> "Hispanic or Latinx", "Hispanic or Latinx", NA, NA, "White…
    $ School      <chr> "Lewis and Clark", "Lewis and Clark", "Lewis and Clark", "…
    $ Survey_Type <chr> "Pre", "Post", "Pre", "Post", "Pre", "Post", "Pre", "Post"…
    $ Q39         <dbl> 7, 4, 9, 10, 10, 9, 10, 10, 9, 10, 10, 7, 10, 9, 10, 10, 1…
    $ Q15         <dbl> 1, 2, 4, 4, 3, 3, 3, 2, 3, 4, 3, 3, 4, 3, 4, 4, 3, 3, 3, 3…
    $ Q17_1       <dbl> 1, 2, 4, 4, 3, 2, 3, 4, 3, 4, 3, 4, 4, 3, 4, 4, 4, 4, 4, 4…
    $ Q17_2       <dbl> 1, 2, 5, 5, 3, 5, 4, 4, 4, 5, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5…
    $ Q17_3       <dbl> 2, 2, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5…
    $ Q17_4       <dbl> 1, 2, 3, 3, 3, 1, 4, 2, 3, 3, 2, 4, 4, 3, 5, 5, 3, 3, 2, 2…

## Check Distributions

Look at the distribution of responses by question.

Do these need to be transformed? (e.g., Q17_2, Q17_3, Q39)

``` r
data %>% 
  pivot_longer(starts_with("Q"), 
               names_to = "Question", 
               values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_histogram(aes(fill = Survey_Type), 
                 position = "identity", alpha = 0.5, bins = 5) +
  facet_wrap(~ Question, scales = "free_x") +
  labs(x = "Score",
       y = "Frequency",
       fill = "Survey") +
  theme_bw()
```

![](02_analyses_files/figure-commonmark/unnamed-chunk-3-1.png)

## Run a Linear Model

Run a model for Q39 responses as a function of the survey (pre
vs. post). Include student, semester, and university as random effects.

TO DO: Model does not run with all 3 random effects (“singular”). Also
does not run with just semester and school. Does not work for either Q39
(very skewed) or Q15 (normal distribution).

``` r
model_Q39 <- lmer(Q15 ~ Survey_Type + (1 | Term) + (1 | School), 
              data = data)
```

    boundary (singular) fit: see help('isSingular')

``` r
#summary(model)
```
