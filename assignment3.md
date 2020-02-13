Statistical assignment 3
================
\[Daniel Orchard\] \[680040213\]
\[13 Feb 2020\]

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.5.3

``` r
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "C:/Users/dan19/Documents/MY DOCUMENTS/Year 2 Term 2 Modules/Data III/Understanding Society Data/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
   pivot_longer(cols = a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
   separate(variable, into = c("wave","variable"), sep = "_", extra = "merge") %>%
   pivot_wider(names_from = variable, values_from = value)

Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # ... with 584,224 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = ifelse(sex_dv == 2, "female",
                           ifelse(sex_dv == 1, "male", NA)) ) %>%
        mutate(vote6 = ifelse(vote6 < 1, NA, vote6))

        table(Long$sex_dv)
```

    ## 
    ## female   male 
    ## 117665 100342

``` r
        table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%  
  
  filter(!is.na(sex_dv)) %>%
       
  group_by(sex_dv, wave) %>%
  
  summarise( meanvoting = mean(vote6, na.rm = TRUE))
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   sex_dv [2]
    ##    sex_dv wave  meanvoting
    ##    <chr>  <chr>      <dbl>
    ##  1 female a           2.84
    ##  2 female b           2.82
    ##  3 female c           2.87
    ##  4 female d           2.89
    ##  5 female e           2.87
    ##  6 female f           2.81
    ##  7 female g           2.73
    ##  8 male   a           2.53
    ##  9 male   b           2.51
    ## 10 male   c           2.54
    ## 11 male   d           2.55
    ## 12 male   e           2.51
    ## 13 male   f           2.47
    ## 14 male   g           2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
wide <- meanVote6 %>%
  pivot_wider(names_from = wave, values_from = meanvoting)

wide
```

    ## # A tibble: 2 x 8
    ## # Groups:   sex_dv [2]
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

This data seems to show that for the most part mean political interest
has remained fairly stable over time with only minor changes year on
year. It also shows that females, on average, have a greater political
interest than men as each wave they are about 0.3 higher on the 4 point
scale than their male counterparts.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
# removing pidp that are not present in all 7 waves
 No_NAs <- all7 %>% 
  
      mutate(a_vote6 = recode(a_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>%
        
       mutate(b_vote6 = recode(b_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>%
        
         mutate(c_vote6 = recode(c_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>%
        
         mutate(d_vote6 = recode(d_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>%
        
         mutate(e_vote6 = recode(e_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>%
        
         mutate(f_vote6 = recode(f_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>%
          
         mutate(g_vote6 = recode(g_vote6,
        `1` = 1, 
        `2` = 2,
        `3` = 3,
        `4` = 4,
        .default = NA_real_)) %>% 
  
         mutate(a_sex_dv = ifelse(a_sex_dv == 2, "female",
                           ifelse(a_sex_dv == 1, "male", NA))) %>%
  
  na.omit() %>% filter(a_memorig == 1)

table(No_NAs$g_memorig)
```

    ## 
    ##     1 
    ## 14821

``` r
## memorig remains consistent along waves. Removing one rmeoves them all



# calculating Delta for each person in the data
No_NAs <- No_NAs %>% mutate(delta = abs(b_vote6 - a_vote6) + 
                                    abs(c_vote6 - b_vote6) +
                                    abs(d_vote6 - c_vote6) + 
                                    abs(e_vote6 - d_vote6) + 
                                    abs(f_vote6 - e_vote6) + 
                                    abs(g_vote6 - f_vote6))
# abs computes absolute values so we do this for all of them
table(No_NAs$delta)
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   16 
    ## 3027 1837 3194 2417 2115 1050  668  279  139   51   33    5    4    1    1

``` r
# no negatives. good

mean(No_NAs$delta)
```

    ## [1] 2.508603

``` r
tapply(No_NAs$delta, No_NAs$a_sex_dv , mean)
```

    ##   female     male 
    ## 2.494703 2.527760

``` r
#  female     male 
# 2.494703 2.527760 

meandeltasex <- No_NAs %>% group_by(a_sex_dv) %>% summarise(mean = mean(delta))
# making it into its own dataframe

#doing the same thing for age
meandeltaage <- No_NAs %>% group_by(a_age_dv) %>% summarise(mean = mean(delta))
```

# Mean Delta for Men and Women

``` r
 meandeltasex
```

    ## # A tibble: 2 x 2
    ##   a_sex_dv  mean
    ##   <chr>    <dbl>
    ## 1 female    2.49
    ## 2 male      2.53

This table shows that men and women, on average, have similar levels of
political change in their lives with men having very slightly higher
average deltas.

# Mean Delta for each age (at Wave one)

``` r
scatter.smooth(meandeltaage, main = "Change in Political Interest (Delta) for different ages" ,xlab = "Age", ylab = "Delta: Change in Political Interest over 7 waves")
```

![](assignment3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# putting it on a scatter plot
```

This graph appears to show that the mean deltas of people age 20 to 60
at the time of wave 1 are about 2.5. After 60, the mean delta for each
age goes up, reaching a peak of around3.5 at age 90. There are quite a
few outliers at this point in the graph, perhaps due to the low number
of people at that age.

It must be note that these ages will be slightly offset as they are
marked by age at wave one. This means the point at 15 years of age is
actually 7 years older and the delta is average change in political
interest over those 7 years.
