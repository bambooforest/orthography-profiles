ACQDIV-PHON data tests
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(tidyr)
library(testthat)
```

``` r
load('data/db/production.Rdata')
df <- df %>% mutate(segments = strsplit(as.character(segments), "\\s")) %>% unnest(segments)
head(df)
```

    ##   session_id utterance_id word_id language    word pos_word_stem
    ## 1          1            1       1 Chintang habinɨŋ             V
    ## 2          1            1       1 Chintang habinɨŋ             V
    ## 3          1            1       1 Chintang habinɨŋ             V
    ## 4          1            1       1 Chintang habinɨŋ             V
    ## 5          1            1       1 Chintang habinɨŋ             V
    ## 6          1            1       1 Chintang habinɨŋ             V
    ##   word_length session_target_child uniquespeaker_id speaker_label    age
    ## 1           7                   11                5           GKR 40;0.0
    ## 2           7                   11                5           GKR 40;0.0
    ## 3           7                   11                5           GKR 40;0.0
    ## 4           7                   11                5           GKR 40;0.0
    ## 5           7                   11                5           GKR 40;0.0
    ## 6           7                   11                5           GKR 40;0.0
    ##   age_in_days gender macrorole       utterance segments
    ## 1       14600 Female     Adult habinɨŋ habinɨŋ        h
    ## 2       14600 Female     Adult habinɨŋ habinɨŋ        a
    ## 3       14600 Female     Adult habinɨŋ habinɨŋ        b
    ## 4       14600 Female     Adult habinɨŋ habinɨŋ        i
    ## 5       14600 Female     Adult habinɨŋ habinɨŋ        n
    ## 6       14600 Female     Adult habinɨŋ habinɨŋ        ɨ

``` r
counts <- df %>% group_by(language, segments) %>% select(language, segments) %>% summarize(count=n())
# counts <- ungroup(counts)
head(counts)
```

    ## # A tibble: 6 x 3
    ## # Groups:   language [1]
    ##   language segments  count
    ##   <chr>    <chr>     <int>
    ## 1 Chintang a        836167
    ## 2 Chintang ã         31136
    ## 3 Chintang b         88131
    ## 4 Chintang bʰ        15509
    ## 5 Chintang d         79857
    ## 6 Chintang dʰ         5495

Chintang
========

``` r
# Load orthography profiles and check that their contents are in 
## Chintang
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Chintang.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Chintang") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
expect_true(all(x$segments %in% op$simple_target))
```

Indonesian
==========

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Indonesian.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Indonesian") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
expect_true(all(x$segments %in% op$simple_target))
```

Inuktitut
=========

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Inuktitut.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Inuktitut") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
expect_true(all(x$segments %in% op$simple_target))
```

Japanese
========

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Japanese.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Japanese") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
# expect_true(all(x$segments %in% op$simple_target))
x[which(!(x$segments %in% op$simple_target)),]
```

    ## # A tibble: 2 x 2
    ## # Groups:   language [1]
    ##   language segments
    ##   <chr>    <chr>   
    ## 1 Japanese ç       
    ## 2 Japanese �

``` r
counts %>% filter(language=="Japanese") %>% filter(segments=="ç")
```

    ## # A tibble: 0 x 3
    ## # Groups:   language [0]
    ## # ... with 3 variables: language <chr>, segments <chr>, count <int>

``` r
counts %>% filter(language=="Japanese") %>% filter(segments=="�")
```

    ## # A tibble: 1 x 3
    ## # Groups:   language [1]
    ##   language segments count
    ##   <chr>    <chr>    <int>
    ## 1 Japanese �          135

Russian
=======

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Russian.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Russian") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
expect_true(all(x$segments %in% op$simple_target))
```

Sesotho
=======

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Sesotho.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Sesotho") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
# expect_true(all(x$segments %in% op$simple_target))
x[which(!(x$segments %in% op$simple_target)),]
```

    ## # A tibble: 1 x 2
    ## # Groups:   language [1]
    ##   language segments
    ##   <chr>    <chr>   
    ## 1 Sesotho  �

``` r
counts %>% filter(language=="Sesotho") %>% filter(segments=="�")
```

    ## # A tibble: 1 x 3
    ## # Groups:   language [1]
    ##   language segments count
    ##   <chr>    <chr>    <int>
    ## 1 Sesotho  �          144

Turkish
=======

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Turkish.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Turkish") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
expect_true(all(x$segments %in% op$simple_target))
```

Yucatec
=======

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Yucatec.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Yucatec") %>% select(segments)
```

    ## Adding missing grouping variables: `language`

``` r
# expect_true(all(x$segments %in% op$simple_target))
x[which(!(x$segments %in% op$simple_target)),]
```

    ## # A tibble: 1 x 2
    ## # Groups:   language [1]
    ##   language segments
    ##   <chr>    <chr>   
    ## 1 Yucatec  �

``` r
counts %>% filter(language=="Yucatec") %>% filter(segments=="�")
```

    ## # A tibble: 1 x 3
    ## # Groups:   language [1]
    ##   language segments count
    ##   <chr>    <chr>    <int>
    ## 1 Yucatec  �          182
