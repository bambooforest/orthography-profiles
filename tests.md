ACQDIV-PHON data tests for segments
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;

``` r
library(dplyr)
library(tidyr)
library(testthat)
```

``` r
load('data/db/production.Rdata')
df.long <- df %>% mutate(segments = strsplit(as.character(segments), "\\s")) %>% unnest(segments)
counts <- df.long %>% group_by(language, segments) %>% select(language, segments) %>% summarize(count=n())
```

Load orthography profiles and check that their contents are in
==============================================================

Chintang
--------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Chintang.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Chintang") %>% select(language, segments)
expect_true(all(x$segments %in% op$simple_target))
```

Indonesian
----------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Indonesian.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Indonesian") %>% select(language, segments)
expect_true(all(x$segments %in% op$simple_target))
```

Inuktitut
---------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Inuktitut.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Inuktitut") %>% select(language, segments)
expect_true(all(x$segments %in% op$simple_target))
```

Japanese
--------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Japanese.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Japanese") %>% select(language, segments)
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
# Lots of loanwords with <c> not translated by the OP
df %>% filter(language=="Japanese") %>% filter(grepl("� ", segments)) %>% select(word_id, word, segments)
```

    ##    word_id                                 word
    ## 1  3754582                              tractor
    ## 2  3806317                             occhatta
    ## 3  3999869                         trafficlight
    ## 4  4002449                            crocodile
    ## 5  4044645                              perfect
    ## 6  4149076                         pachipacchin
    ## 7  4156089                      chocchocchoccho
    ## 8  4178930                             ariacchu
    ## 9  4178949                             racchito
    ## 10 4237306                      chocchocchoccho
    ## 11 4240436 otchikatsukiioocikatsukeeochikatsuke
    ## 12 4269072                          okkocchatta
    ## 13 4291043                          okkocchatta
    ## 14 4294091                             knocking
    ## 15 4356625                              cutting
    ## 16 4366351                          okkocchatta
    ## 17 4366358                          okkocchatta
    ## 18 4390043                              octopus
    ## 19 4411153          welcome_to_darling_paradise
    ## 20 4413318                Santa_Claus_is_coming
    ## 21 4423461                             wellcome
    ## 22 4441817                                socks
    ## 23 4451105                                 ciao
    ## 24 4451108                                 ciao
    ## 25 4462633                                 cchi
    ## 26 4477798                             icecream
    ## 27 4480060                                black
    ## 28 4480062                                black
    ## 29 4480063                                black
    ## 30 4480064                                black
    ## 31 4487786                     nice_to_meet_you
    ## 32 4487789                     nice_to_meet_you
    ## 33 4487802                     nice_to_meet_you
    ## 34 4487805                     nice_to_meet_you
    ## 35 4487869                        nice_meet_you
    ## 36 4487871                     nice_to_meet_you
    ## 37 4487873                     nice_to_meet_you
    ## 38 4487883                     nice_to_meet_you
    ## 39 4487918                     nice_to_meet_you
    ## 40 4491959                                  cry
    ## 41 4491988                                 -cry
    ## 42 4491989                                 -cry
    ## 43 4491997                                 -cry
    ## 44 4491998                                 -cry
    ## 45 4492261               what_color_do_you_like
    ## 46 4493967                               ucchai
    ## 47 4499517                       chanocchanocha
    ## 48 4503160                                uccho
    ## 49 4505179                                 ciao
    ## 50 4554901              chuctchurururururutataa
    ## 51 4577298                                  cry
    ## 52 4577305                                  cry
    ## 53 4578457                                  cry
    ## 54 4582852                          okkocchatta
    ## 55 4582854                          okkocchatta
    ## 56 4582855                          okkocchatta
    ## 57 4592250                               camera
    ## 58 4592251                               camera
    ## 59 4592255                                  cat
    ## 60 4592256                                  cat
    ## 61 4592261                               coffee
    ## 62 4592262                               coffee
    ## 63 4592286                               doctor
    ## 64 4592520                        policestation
    ## 65 4592526                               castle
    ## 66 4592534                               cicada
    ## 67 4592537                               cicada
    ## 68 4592583                             hickhick
    ## 69 4706351                                  cry
    ## 70 4724936                 bechupichupichupicyo
    ## 71 4732115                              octopus
    ## 72 4732121                              octopus
    ## 73 4732125                              octopus
    ## 74 4732148                              volcano
    ## 75 4753020                              chaccha
    ## 76 4755117                               cyumam
    ## 77 4764073           chakachakacchakachakachaka
    ## 78 4767102                              icihiji
    ##                                                                   segments
    ## 1                                                            t r a � t o r
    ## 2                                                          o � � h a t t a
    ## 3                                                  t r a ɸ ɸ i � l i ɡ h t
    ## 4                                                        � r o � o d i l e
    ## 5                                                            p e r ɸ e � t
    ## 6                                                  p a � h i p a � � h i n
    ## 7                                            � h o � � h o � � h o � � h o
    ## 8                                                          a r i a � � h u
    ## 9                                                          r a � � h i t o
    ## 10                                           � h o � � h o � � h o � � h o
    ## 11 o t � h i k a t s u k i i o o � i k a t s u k e e o � h i k a t s u k e
    ## 12                                                   o k k o � � h a t t a
    ## 13                                                   o k k o � � h a t t a
    ## 14                                                         k n o � k i n ɡ
    ## 15                                                           � u t t i n ɡ
    ## 16                                                   o k k o � � h a t t a
    ## 17                                                   o k k o � � h a t t a
    ## 18                                                           o � t o p u s
    ## 19                         w e l � o m e t o d a r l i n ɡ p a r a d i s e
    ## 20                                     s a n t a c l a u s i s � o m i n ɡ
    ## 21                                                         w e l l � o m e
    ## 22                                                               s o � k s
    ## 23                                                                 � i a o
    ## 24                                                                 � i a o
    ## 25                                                                 � � h i
    ## 26                                                         i � e � r e a m
    ## 27                                                               b l a � k
    ## 28                                                               b l a � k
    ## 29                                                               b l a � k
    ## 30                                                               b l a � k
    ## 31                                               n i � e t o m e e t j o u
    ## 32                                               n i � e t o m e e t j o u
    ## 33                                               n i � e t o m e e t j o u
    ## 34                                               n i � e t o m e e t j o u
    ## 35                                                   n i � e m e e t j o u
    ## 36                                               n i � e t o m e e t j o u
    ## 37                                               n i � e t o m e e t j o u
    ## 38                                               n i � e t o m e e t j o u
    ## 39                                               n i � e t o m e e t j o u
    ## 40                                                                   � r j
    ## 41                                                                   � r j
    ## 42                                                                   � r j
    ## 43                                                                   � r j
    ## 44                                                                   � r j
    ## 45                                     w h a t � o l o r d o j o u l i k e
    ## 46                                                             u � � h a i
    ## 47                                             � h a n o � � h a n o � h a
    ## 48                                                               u � � h o
    ## 49                                                                 � i a o
    ## 50                           � h u � t � h u r u r u r u r u r u t a t a a
    ## 51                                                                   � r j
    ## 52                                                                   � r j
    ## 53                                                                   � r j
    ## 54                                                   o k k o � � h a t t a
    ## 55                                                   o k k o � � h a t t a
    ## 56                                                   o k k o � � h a t t a
    ## 57                                                             � a m e r a
    ## 58                                                             � a m e r a
    ## 59                                                                   � a t
    ## 60                                                                   � a t
    ## 61                                                             � o ɸ ɸ e e
    ## 62                                                             � o ɸ ɸ e e
    ## 63                                                             d o � t o r
    ## 64                                               p o l i � e s t a t i o n
    ## 65                                                             � a s t l e
    ## 66                                                             � i � a d a
    ## 67                                                             � i � a d a
    ## 68                                                         h i � k h i � k
    ## 69                                                                   � r j
    ## 70                                 b e � h u p i � h u p i � h u p i � j o
    ## 71                                                           o � t o p u s
    ## 72                                                           o � t o p u s
    ## 73                                                           o � t o p u s
    ## 74                                                           v o l � a n o
    ## 75                                                           � h a � � h a
    ## 76                                                             � j u m a m
    ## 77                     � h a k a � h a k a � � h a k a � h a k a � h a k a
    ## 78                                                          i � i h i d̠ʒ i

``` r
# Nasty hack because copy and paste in RStudio
c <- x[which(!(x$segments %in% op$simple_target)),]
c <- c$segments[1]
# The c-cedilla does occur in the OP -- that it fails here is a pre-/de-compose issue
df %>% filter(language=="Japanese") %>% filter(grepl(c, segments)) %>% select(word_id, word, segments)
```

    ##     word_id                  word                               segments
    ## 1   3665513                  hyoo                                   ç oː
    ## 2   3665515                  hyoo                                   ç oː
    ## 3   3667621                  hyoo                                   ç oː
    ## 4   3667622                  hyoo                                   ç oː
    ## 5   3667623                  hyoo                                   ç oː
    ## 6   3667626                  hyoo                                   ç oː
    ## 7   3668821               hyakuen                            ç a k u e n
    ## 8   3668823               hyakuen                            ç a k u e n
    ## 9   3668825               hyakuen                            ç a k u e n
    ## 10  3669602               hyakuen                            ç a k u e n
    ## 11  3669604               hyakuen                            ç a k u e n
    ## 12  3669707               hyakuen                            ç a k u e n
    ## 13  3669712               hyakuen                            ç a k u e n
    ## 14  3671288               hyakuen                            ç a k u e n
    ## 15  3671294               hyakuen                            ç a k u e n
    ## 16  3671295               hyakuen                            ç a k u e n
    ## 17  3674037           hyakuendama                    ç a k u e n d a m a
    ## 18  3674154           hyakuendama                    ç a k u e n d a m a
    ## 19  3675199               hyakuen                            ç a k u e n
    ## 20  3675200               hyakuen                            ç a k u e n
    ## 21  3675201               hyakuen                            ç a k u e n
    ## 22  3675286               hyakuen                            ç a k u e n
    ## 23  3675287               hyakuen                            ç a k u e n
    ## 24  3676317               hyakuen                            ç a k u e n
    ## 25  3678772               hyakuen                            ç a k u e n
    ## 26  3678774               hyakuen                            ç a k u e n
    ## 27  3678795               hyakuen                            ç a k u e n
    ## 28  3678882               hyakuen                            ç a k u e n
    ## 29  3678884               hyakuen                            ç a k u e n
    ## 30  3678885               hyakuen                            ç a k u e n
    ## 31  3678886               hyakuen                            ç a k u e n
    ## 32  3678985               hyakuen                            ç a k u e n
    ## 33  3678987               hyakuen                            ç a k u e n
    ## 34  3681737                 hyaku                                ç a k u
    ## 35  3681746               hyakuen                            ç a k u e n
    ## 36  3689869               hyakuen                            ç a k u e n
    ## 37  3693008                 hyaku                                ç a k u
    ## 38  3693011                 hyaku                                ç a k u
    ## 39  3708447               hyakuen                            ç a k u e n
    ## 40  3710885               hyakuen                            ç a k u e n
    ## 41  3710887               hyakuen                            ç a k u e n
    ## 42  3712061               hyakuen                            ç a k u e n
    ## 43  3712073               hyakuen                            ç a k u e n
    ## 44  3712095               hyakuen                            ç a k u e n
    ## 45  3712096               hyakuen                            ç a k u e n
    ## 46  3712230               hyakuen                            ç a k u e n
    ## 47  3712237               hyakuen                            ç a k u e n
    ## 48  3712240               hyakuen                            ç a k u e n
    ## 49  3777733                  hyui                                  ç u i
    ## 50  3780168                   hyu                                    ç u
    ## 51  3787448                  hyui                                  ç u i
    ## 52  3787609                   hya                                    ç a
    ## 53  3787624                dohyaa                               d o ç aː
    ## 54  3789046                   hyu                                    ç u
    ## 55  3789049                   hyu                                    ç u
    ## 56  3789051                   hyu                                    ç u
    ## 57  3791771                  hyuu                                   ç uː
    ## 58  3794477                  hyuu                                   ç uː
    ## 59  3794489                  hyuu                                   ç uː
    ## 60  3794517                 hyuun                                 ç uː n
    ## 61  3794524                 hyuun                                 ç uː n
    ## 62  3794602                 hyuun                                 ç uː n
    ## 63  3794617                  hyuu                                   ç uː
    ## 64  3794630                  hyuu                                   ç uː
    ## 65  3794642                 hyuun                                 ç uː n
    ## 66  3801308                  hyuq                                  ç u q
    ## 67  3801512                  hyaq                                  ç a q
    ## 68  3808366                  hyuu                                   ç uː
    ## 69  3809637                  hyuu                                   ç uː
    ## 70  3809754                  hyuu                                   ç uː
    ## 71  3809756                  hyuu                                   ç uː
    ## 72  3812177               hyakuen                            ç a k u e n
    ## 73  3812178               hyakuen                            ç a k u e n
    ## 74  3812179               hyakuen                            ç a k u e n
    ## 75  3812189               hyakuen                            ç a k u e n
    ## 76  3812195               hyakuen                            ç a k u e n
    ## 77  3812202               hyakuen                            ç a k u e n
    ## 78  3812204               hyakuen                            ç a k u e n
    ## 79  3812206               hyakuen                            ç a k u e n
    ## 80  3812207               hyakuen                            ç a k u e n
    ## 81  3812213             nihyakuen                        n i ç a k u e n
    ## 82  3812216             nihyakuen                        n i ç a k u e n
    ## 83  3812220               hyakuen                            ç a k u e n
    ## 84  3812248             nihyakuen                        n i ç a k u e n
    ## 85  3812249               hyakuen                            ç a k u e n
    ## 86  3812303               hyakuen                            ç a k u e n
    ## 87  3812304               hyakuen                            ç a k u e n
    ## 88  3812321               hyakuen                            ç a k u e n
    ## 89  3812322               hyakuen                            ç a k u e n
    ## 90  3812323               hyakuen                            ç a k u e n
    ## 91  3812326               hyakuen                            ç a k u e n
    ## 92  3812327               hyakuen                            ç a k u e n
    ## 93  3812332               hyakuen                            ç a k u e n
    ## 94  3812348               hyakuen                            ç a k u e n
    ## 95  3812379               hyakuen                            ç a k u e n
    ## 96  3812435                 hyuun                                 ç uː n
    ## 97  3812458               hyakuen                            ç a k u e n
    ## 98  3812462               hyakuen                            ç a k u e n
    ## 99  3812489               hyakuen                            ç a k u e n
    ## 100 3812497                 hyaku                                ç a k u
    ## 101 3812503               hyakuen                            ç a k u e n
    ## 102 3812686            hyuunhyuun                          ç uː n ç uː n
    ## 103 3812710                  hyuu                                   ç uː
    ## 104 3813273                  hyuu                                   ç uː
    ## 105 3813274                  hyuu                                   ç uː
    ## 106 3813277                  hyuu                                   ç uː
    ## 107 3815628                  hyuu                                   ç uː
    ## 108 3815630                  hyuu                                   ç uː
    ## 109 3815639                  hyuu                                   ç uː
    ## 110 3815640                  hyuu                                   ç uː
    ## 111 3815645                  hyuu                                   ç uː
    ## 112 3815880                 hyuun                                 ç uː n
    ## 113 3816197                  hyuu                                   ç uː
    ## 114 3816198                  hyuu                                   ç uː
    ## 115 3816199                  hyuu                                   ç uː
    ## 116 3821319               hyakuen                            ç a k u e n
    ## 117 3821322               hyakuen                            ç a k u e n
    ## 118 3821326               hyakuen                            ç a k u e n
    ## 119 3822291              hyuuhyuu                              ç uː ç uː
    ## 120 3822292                  hyuu                                   ç uː
    ## 121 3822293                  hyuu                                   ç uː
    ## 122 3822354                   hya                                    ç a
    ## 123 3822365                   hya                                    ç a
    ## 124 3829172               hyakuen                            ç a k u e n
    ## 125 3829441               hyakuen                            ç a k u e n
    ## 126 3829444               hyakuen                            ç a k u e n
    ## 127 3829447               hyakuen                            ç a k u e n
    ## 128 3829448               hyakuen                            ç a k u e n
    ## 129 3829471               hyakuen                            ç a k u e n
    ## 130 3832715                  hyuu                                   ç uː
    ## 131 3833238                  hyuu                                   ç uː
    ## 132 3838509                  hyuu                                   ç uː
    ## 133 3838557                  hyuu                                   ç uː
    ## 134 3841308                  hyuu                                   ç uː
    ## 135 3848240                  hyuu                                   ç uː
    ## 136 3848916                 hyuun                                 ç uː n
    ## 137 3848917                  hyuu                                   ç uː
    ## 138 3855261                  hyun                                  ç u n
    ## 139 3855562           hyottoshite                      ç o t t o ʃ i t e
    ## 140 3855787                 hyuun                                 ç uː n
    ## 141 3856840              hihyuhai                          h i ç u h a i
    ## 142 3856842              hihyuhai                          h i ç u h a i
    ## 143 3856844              hihyuhai                          h i ç u h a i
    ## 144 3856846              hihyuhai                          h i ç u h a i
    ## 145 3856847              hihyuhai                          h i ç u h a i
    ## 146 3856852              hihyuhai                          h i ç u h a i
    ## 147 3856856              hihyuhai                          h i ç u h a i
    ## 148 3856857              hihyuhai                          h i ç u h a i
    ## 149 3856903               hyakuen                            ç a k u e n
    ## 150 3857747                 hyuun                                 ç uː n
    ## 151 3859230                   hya                                    ç a
    ## 152 3863821                  hyuu                                   ç uː
    ## 153 3863837                  hyuu                                   ç uː
    ## 154 3863838                 hyuun                                 ç uː n
    ## 155 3867784             hyohyohyo                            ç o ç o ç o
    ## 156 3867785             hyohyohyo                            ç o ç o ç o
    ## 157 3869593               hyakuen                            ç a k u e n
    ## 158 3872880               hyakuen                            ç a k u e n
    ## 159 3872883               hyakuen                            ç a k u e n
    ## 160 3872884               hyakuen                            ç a k u e n
    ## 161 3873005               hyakuen                            ç a k u e n
    ## 162 3873009               hyakuen                            ç a k u e n
    ## 163 3873012               hyakuen                            ç a k u e n
    ## 164 3873014               hyakuen                            ç a k u e n
    ## 165 3873019               hyakuen                            ç a k u e n
    ## 166 3873022               hyakuen                            ç a k u e n
    ## 167 3876973                   hyu                                    ç u
    ## 168 3878836                  hyuq                                  ç u q
    ## 169 3879050               koohyuu                              k oː ç uː
    ## 170 3879817              hyakuten                          ç a k u t e n
    ## 171 3879818              hyakuten                          ç a k u t e n
    ## 172 3881060                  hyuu                                   ç uː
    ## 173 3886535               koohyuu                              k oː ç uː
    ## 174 3887763                  hyuu                                   ç uː
    ## 175 3887764                  hyuu                                   ç uː
    ## 176 3888068                  hyuu                                   ç uː
    ## 177 3888204               hyakuen                            ç a k u e n
    ## 178 3888285            yonhyakuen                      j o n ç a k u e n
    ## 179 3890405                  hyun                                  ç u n
    ## 180 3893868                   hyu                                    ç u
    ## 181 3894264                   hyu                                    ç u
    ## 182 3895849                  hyuu                                   ç uː
    ## 183 3896133             gohyakuen                        ɡ o ç a k u e n
    ## 184 3897131                   hya                                    ç a
    ## 185 3899918                  hyuu                                   ç uː
    ## 186 3906775             gohyakuen                        ɡ o ç a k u e n
    ## 187 3908320                  hyuu                                   ç uː
    ## 188 3910073                  hyuu                                   ç uː
    ## 189 3911357                  hyuu                                   ç uː
    ## 190 3913474               hyakuen                            ç a k u e n
    ## 191 3914660                   hyu                                    ç u
    ## 192 3916102                  hyuu                                   ç uː
    ## 193 3916371                 hyuun                                 ç uː n
    ## 194 3918067                  hyuu                                   ç uː
    ## 195 3918068                  hyuu                                   ç uː
    ## 196 3918080                  hyuu                                   ç uː
    ## 197 3921003                   hyu                                    ç u
    ## 198 3929425                   hya                                    ç a
    ## 199 3933454             nihyakuen                        n i ç a k u e n
    ## 200 3933577             nihyakuen                        n i ç a k u e n
    ## 201 3933578             nihyakuen                        n i ç a k u e n
    ## 202 3933585             nihyakuen                        n i ç a k u e n
    ## 203 3937333                  hyuu                                   ç uː
    ## 204 3937412                  hyuu                                   ç uː
    ## 205 3937432                  hyuu                                   ç uː
    ## 206 3938411                   hyu                                    ç u
    ## 207 3939208             gohyakuen                        ɡ o ç a k u e n
    ## 208 3939559                  hyuu                                   ç uː
    ## 209 3948402                  hyuu                                   ç uː
    ## 210 3948406                  hyuu                                   ç uː
    ## 211 3952598             nihyakuen                        n i ç a k u e n
    ## 212 3952606             nihyakuen                        n i ç a k u e n
    ## 213 3952607             nihyakuen                        n i ç a k u e n
    ## 214 3952609             nihyakuen                        n i ç a k u e n
    ## 215 3952614             nihyakuen                        n i ç a k u e n
    ## 216 3952626               nihyaku                            n i ç a k u
    ## 217 3952627             nihyakuen                        n i ç a k u e n
    ## 218 3952656               nihyaku                            n i ç a k u
    ## 219 3955040                   hyu                                    ç u
    ## 220 3960581                   hyu                                    ç u
    ## 221 3960588                   hyu                                    ç u
    ## 222 3961152                 hyuun                                 ç uː n
    ## 223 3961599             nihyakuen                        n i ç a k u e n
    ## 224 3961750                 hyaku                                ç a k u
    ## 225 3961829                 hyaku                                ç a k u
    ## 226 3961861               nihyaku                            n i ç a k u
    ## 227 3961866               nihyaku                            n i ç a k u
    ## 228 3961869               nihyaku                            n i ç a k u
    ## 229 3962091                 hyaku                                ç a k u
    ## 230 3962829               nihyaku                            n i ç a k u
    ## 231 3964437             nihyakuen                        n i ç a k u e n
    ## 232 3964639               nihyaku                            n i ç a k u
    ## 233 3964645               nihyaku                            n i ç a k u
    ## 234 3964831             gohyakuen                        ɡ o ç a k u e n
    ## 235 3964834             gohyakuen                        ɡ o ç a k u e n
    ## 236 3964835             gohyakuen                        ɡ o ç a k u e n
    ## 237 3965968           hyururururu                    ç u r u r u r u r u
    ## 238 3972194                 hyuui                                 ç uː i
    ## 239 3972195                  hyuu                                   ç uː
    ## 240 3973552                  hyuu                                   ç uː
    ## 241 3973563                  hyun                                  ç u n
    ## 242 3973780                   hyu                                    ç u
    ## 243 3974000             gohyakuen                        ɡ o ç a k u e n
    ## 244 3974135             gohyakuen                        ɡ o ç a k u e n
    ## 245 3974137               hyakuen                            ç a k u e n
    ## 246 3974143                 hyaku                                ç a k u
    ## 247 3974145                 hyaku                                ç a k u
    ## 248 3974156                 hyaku                                ç a k u
    ## 249 3974160                 hyaku                                ç a k u
    ## 250 3974299             gohyakuen                        ɡ o ç a k u e n
    ## 251 3974328             gohyakuen                        ɡ o ç a k u e n
    ## 252 3974338             gohyakuen                        ɡ o ç a k u e n
    ## 253 3974340             gohyakuen                        ɡ o ç a k u e n
    ## 254 3974344             gohyakuen                        ɡ o ç a k u e n
    ## 255 3974346             gohyakuen                        ɡ o ç a k u e n
    ## 256 3974750             gohyakuen                        ɡ o ç a k u e n
    ## 257 3974847             nihyakuen                        n i ç a k u e n
    ## 258 3974862               gohyaku                            ɡ o ç a k u
    ## 259 3975064                 hyaku                                ç a k u
    ## 260 3975183                 hyaku                                ç a k u
    ## 261 3975380               nihyaku                            n i ç a k u
    ## 262 3976606                 hyuun                                 ç uː n
    ## 263 3976774                   hyu                                    ç u
    ## 264 3976778                   hyu                                    ç u
    ## 265 3977146             gohyakuen                        ɡ o ç a k u e n
    ## 266 3977148             gohyakuen                        ɡ o ç a k u e n
    ## 267 3977151             gohyakuen                        ɡ o ç a k u e n
    ## 268 3984312                 hyuun                                 ç uː n
    ## 269 3985487                 hyaku                                ç a k u
    ## 270 3986422             gohyakuen                        ɡ o ç a k u e n
    ## 271 3986426             gohyakuen                        ɡ o ç a k u e n
    ## 272 3986433             gohyakuen                        ɡ o ç a k u e n
    ## 273 3986442             gohyakuen                        ɡ o ç a k u e n
    ## 274 3986453             gohyakuen                        ɡ o ç a k u e n
    ## 275 3986458             gohyakuen                        ɡ o ç a k u e n
    ## 276 3986465             gohyakuen                        ɡ o ç a k u e n
    ## 277 3986472             gohyakuen                        ɡ o ç a k u e n
    ## 278 3986497             gohyakuen                        ɡ o ç a k u e n
    ## 279 3986500             gohyakuen                        ɡ o ç a k u e n
    ## 280 3986510                 hyaku                                ç a k u
    ## 281 3989783               hyakuen                            ç a k u e n
    ## 282 3989892               nihyaku                            n i ç a k u
    ## 283 3992438           Piihyaradon                    p i i ç a r a d o n
    ## 284 3992439           Piihyaradon                    p i i ç a r a d o n
    ## 285 3992445           Piihyaradon                    p i i ç a r a d o n
    ## 286 3993172              dohyarii                          d o ç a r i i
    ## 287 3993174              dohyarii                          d o ç a r i i
    ## 288 3993249                  hyuu                                   ç uː
    ## 289 3993358                  hyuu                                   ç uː
    ## 290 3993389                  hyuu                                   ç uː
    ## 291 3993563                  hyuu                                   ç uː
    ## 292 3993582                 hyaku                                ç a k u
    ## 293 3993583              hyakkiro                          ç a k k i r o
    ## 294 3993585              hyakkiro                          ç a k k i r o
    ## 295 3993595                 hyaku                                ç a k u
    ## 296 3993598                 hyaku                                ç a k u
    ## 297 3993627                 hyuun                                 ç uː n
    ## 298 3994456                  hyuu                                   ç uː
    ## 299 3994457                hyuwaa                               ç u w aː
    ## 300 3994561                   hyu                                    ç u
    ## 301 3994886                   hya                                    ç a
    ## 302 3994887                  hyuu                                   ç uː
    ## 303 3994908                  hyuu                                   ç uː
    ## 304 3995005           kyuuhyakuen                      kʲ uː ç a k u e n
    ## 305 3995009           kyuuhyakuen                      kʲ uː ç a k u e n
    ## 306 3995010           kyuuhyakuen                      kʲ uː ç a k u e n
    ## 307 3995387     Meetetsuhyakkaten          m eː t e ts u ç a k k a t e n
    ## 308 4000147                   hyu                                    ç u
    ## 309 4000951                  hyuu                                   ç uː
    ## 310 4000952                  hyuu                                   ç uː
    ## 311 4000953                  hyuu                                   ç uː
    ## 312 4003183                hyooji                              ç oː d̠ʒ i
    ## 313 4007375              hyakunen                          ç a k u n e n
    ## 314 4007376              hyakunen                          ç a k u n e n
    ## 315 4007378              hyakunen                          ç a k u n e n
    ## 316 4007382              hyakunen                          ç a k u n e n
    ## 317 4007430              hyakunen                          ç a k u n e n
    ## 318 4007479              hyakunen                          ç a k u n e n
    ## 319 4007516              hyakunen                          ç a k u n e n
    ## 320 4010152         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 321 4020527             nanahyaku                        n a n a ç a k u
    ## 322 4020656                  hyuu                                   ç uː
    ## 323 4020762             ohyatchan                         o ç a t t̠ʃ a n
    ## 324 4021979                  hyuu                                   ç uː
    ## 325 4024989               hyakkai                            ç a k k a i
    ## 326 4024998               hyakkai                            ç a k k a i
    ## 327 4025439             nihyakuen                        n i ç a k u e n
    ## 328 4026376             gohyakuen                        ɡ o ç a k u e n
    ## 329 4026380             gohyakuen                        ɡ o ç a k u e n
    ## 330 4026384            yonhyakuen                      j o n ç a k u e n
    ## 331 4027510             gohyakuen                        ɡ o ç a k u e n
    ## 332 4027514             gohyakuen                        ɡ o ç a k u e n
    ## 333 4027519             gohyakuen                        ɡ o ç a k u e n
    ## 334 4027520             gohyakuen                        ɡ o ç a k u e n
    ## 335 4027523             gohyakuen                        ɡ o ç a k u e n
    ## 336 4027528               hyakuen                            ç a k u e n
    ## 337 4027539             gohyakuen                        ɡ o ç a k u e n
    ## 338 4027572             gohyakuen                        ɡ o ç a k u e n
    ## 339 4027574             gohyakuen                        ɡ o ç a k u e n
    ## 340 4027575             gohyakuen                        ɡ o ç a k u e n
    ## 341 4027871             nihyakuen                        n i ç a k u e n
    ## 342 4027882             nihyakuen                        n i ç a k u e n
    ## 343 4028543           nanahyakuen                    n a n a ç a k u e n
    ## 344 4029346               hyooshi                               ç oː ʃ i
    ## 345 4034498           nanahyakkee                     n a n a ç a k k eː
    ## 346 4034500           nanahyakkee                     n a n a ç a k k eː
    ## 347 4038328               hyakkin                            ç a k k i n
    ## 348 4038772           nanahyakkee                     n a n a ç a k k eː
    ## 349 4038872            nihyakusan                      n i ç a k u s a n
    ## 350 4038873               nihyaku                            n i ç a k u
    ## 351 4038874            gohyakkiro                      ɡ o ç a k k i r o
    ## 352 4038879               nihyaku                            n i ç a k u
    ## 353 4038880            gohyakkiro                      ɡ o ç a k k i r o
    ## 354 4038882            gohyakkiro                      ɡ o ç a k k i r o
    ## 355 4038978           nanahyakkee                     n a n a ç a k k eː
    ## 356 4040646          tashahyakkee                     t a ʃ a ç a k k eː
    ## 357 4041568              tiihyaku                          t i i ç a k u
    ## 358 4041580              tiihyaku                          t i i ç a k u
    ## 359 4044598               hyoogii                             ç oː ɡ i i
    ## 360 4045811                 hyaku                                ç a k u
    ## 361 4045814                 hyaku                                ç a k u
    ## 362 4045824          shussekihyoo                     ʃ u s s e k i ç oː
    ## 363 4046109               dohhyaa                             d o h ç aː
    ## 364 4047360                  hyaq                                  ç a q
    ## 365 4047361                  hyaq                                  ç a q
    ## 366 4047362                   hya                                    ç a
    ## 367 4049156                hyatsu                               ç a ts u
    ## 368 4050688                   hyu                                    ç u
    ## 369 4055736                 hyaku                                ç a k u
    ## 370 4056323                  hyun                                  ç u n
    ## 371 4058025                muhhya                              m u h ç a
    ## 372 4058037                 hyuwa                                ç u w a
    ## 373 4058317                ohyama                              o ç a m a
    ## 374 4058361                  hyaa                                   ç aː
    ## 375 4073757                   hyu                                    ç u
    ## 376 4074728                  hyaa                                   ç aː
    ## 377 4076241                  hyuu                                   ç uː
    ## 378 4076242                  hyuu                                   ç uː
    ## 379 4076243                  hyuu                                   ç uː
    ## 380 4076244                  hyuu                                   ç uː
    ## 381 4076245                  hyuu                                   ç uː
    ## 382 4076246                  hyuu                                   ç uː
    ## 383 4076247                  hyuu                                   ç uː
    ## 384 4076251                  hyuu                                   ç uː
    ## 385 4076653               hyakuen                            ç a k u e n
    ## 386 4076658               hyakuen                            ç a k u e n
    ## 387 4076727                  hyoo                                   ç oː
    ## 388 4077655                  hyaq                                  ç a q
    ## 389 4077996                  hyaa                                   ç aː
    ## 390 4078326                  hyoo                                   ç oː
    ## 391 4079437                hyakko                              ç a k k o
    ## 392 4079443                hyakko                              ç a k k o
    ## 393 4079453              nihyakko                          n i ç a k k o
    ## 394 4079463               nihyaku                            n i ç a k u
    ## 395 4079468               nihyaku                            n i ç a k u
    ## 396 4079470                 hyaku                                ç a k u
    ## 397 4079476               nihyaku                            n i ç a k u
    ## 398 4079524                 hyaku                                ç a k u
    ## 399 4082185              hyakujuu                          ç a k u d̠ʒ uː
    ## 400 4082193              hyakujuu                          ç a k u d̠ʒ uː
    ## 401 4082203                 hyuun                                 ç uː n
    ## 402 4083599                  hyaq                                  ç a q
    ## 403 4083600                  hyaq                                  ç a q
    ## 404 4083601                  hyaq                                  ç a q
    ## 405 4083602                  hyaq                                  ç a q
    ## 406 4083646              fuuihyuu                            ɸ uː i ç uː
    ## 407 4083648                  hyuu                                   ç uː
    ## 408 4083649                  hyuu                                   ç uː
    ## 409 4083650                  hyuu                                   ç uː
    ## 410 4083742                  hyuu                                   ç uː
    ## 411 4083822              hyakkara                          ç a k k a r a
    ## 412 4084379           hyakutooban                     ç a k u t oː b a n
    ## 413 4086395                  hyoo                                   ç oː
    ## 414 4089126                  hyu:                                    ç u
    ## 415 4089170                  hyoo                                   ç oː
    ## 416 4098824               hyukkai                            ç u k k a i
    ## 417 4101577               nihyaku                            n i ç a k u
    ## 418 4101637               nihyaku                            n i ç a k u
    ## 419 4101648               nihyaku                            n i ç a k u
    ## 420 4101663               nihyaku                            n i ç a k u
    ## 421 4101700               nihyaku                            n i ç a k u
    ## 422 4101749               nihyaku                            n i ç a k u
    ## 423 4102146                  hyuu                                   ç uː
    ## 424 4103648                  hyuu                                   ç uː
    ## 425 4103652                  hyuu                                   ç uː
    ## 426 4104245                  hyuu                                   ç uː
    ## 427 4109371               hyakuen                            ç a k u e n
    ## 428 4109610               hyakuen                            ç a k u e n
    ## 429 4109659           nanahyakkee                     n a n a ç a k k eː
    ## 430 4109751             gohyakkee                         ɡ o ç a k k eː
    ## 431 4109753             gohyakkee                         ɡ o ç a k k eː
    ## 432 4109758              hyatsubu                           ç a ts u b u
    ## 433 4109767               gohyaku                            ɡ o ç a k u
    ## 434 4109769             gohyakkee                         ɡ o ç a k k eː
    ## 435 4109881     nanahyakkeenozomi         n a n a ç a k k eː n o z o m i
    ## 436 4110079    nanahyakukeenozomi       n a n a ç a k u k eː n o z o m i
    ## 437 4110181                 hyika                                ç i k a
    ## 438 4110182                hyakue                              ç a k u e
    ## 439 4121734                  hyuu                                   ç uː
    ## 440 4124232               hyookun                             ç oː k u n
    ## 441 4130938              kyoohyoo                             kʲ oː ç oː
    ## 442 4131949                 hyaku                                ç a k u
    ## 443 4131953               hyakuen                            ç a k u e n
    ## 444 4134842           hyottoshite                      ç o t t o ʃ i t e
    ## 445 4136634             gohyakuen                        ɡ o ç a k u e n
    ## 446 4136637              gohyaken                          ɡ o ç a k e n
    ## 447 4137062             nihyakuen                        n i ç a k u e n
    ## 448 4137064             gohyakuen                        ɡ o ç a k u e n
    ## 449 4137068             nihyakuen                        n i ç a k u e n
    ## 450 4137854               nihyaku                            n i ç a k u
    ## 451 4137892                 hyaku                                ç a k u
    ## 452 4138095                  hyoi                                  ç o i
    ## 453 4142343               hyakuen                            ç a k u e n
    ## 454 4142358               hyakuen                            ç a k u e n
    ## 455 4142365               nihyaku                            n i ç a k u
    ## 456 4142381             gohyakuen                        ɡ o ç a k u e n
    ## 457 4142385               nihyaku                            n i ç a k u
    ## 458 4142438               nihyaku                            n i ç a k u
    ## 459 4142883               hyakuen                            ç a k u e n
    ## 460 4142885               hyakuen                            ç a k u e n
    ## 461 4142886               hyakuen                            ç a k u e n
    ## 462 4142890               hyakuen                            ç a k u e n
    ## 463 4143043               hyakuen                            ç a k u e n
    ## 464 4143045             nihyakuen                        n i ç a k u e n
    ## 465 4143046             gohyakuen                        ɡ o ç a k u e n
    ## 466 4143056         gohyakuendama                ɡ o ç a k u e n d a m a
    ## 467 4144650         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 468 4144670         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 469 4144709         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 470 4144711         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 471 4144713         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 472 4144715         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 473 4144717         hyakuenshoppu                  ç a k u e n ʃ o p p u
    ## 474 4145241                 hyaku                                ç a k u
    ## 475 4145243                 hyaku                                ç a k u
    ## 476 4145252                 hyaku                                ç a k u
    ## 477 4145272                 hyaku                                ç a k u
    ## 478 4146989         hyommorookoka                 ç o m m o r oː k o k a
    ## 479 4149307             gohyakuen                        ɡ o ç a k u e n
    ## 480 4149355               hyakuen                            ç a k u e n
    ## 481 4149372               hyakuen                            ç a k u e n
    ## 482 4149386             gohyakuen                        ɡ o ç a k u e n
    ## 483 4149402               hyakuen                            ç a k u e n
    ## 484 4151256               hyakuen                            ç a k u e n
    ## 485 4151265               hyakuen                            ç a k u e n
    ## 486 4151277               hyakuen                            ç a k u e n
    ## 487 4151279               hyakuen                            ç a k u e n
    ## 488 4151716             gohyakuen                        ɡ o ç a k u e n
    ## 489 4151785             gohyakuen                        ɡ o ç a k u e n
    ## 490 4151798         gohyakuendama                ɡ o ç a k u e n d a m a
    ## 491 4151802             gohyakuen                        ɡ o ç a k u e n
    ## 492 4153623                 hyaku                                ç a k u
    ## 493 4153625                 hyaku                                ç a k u
    ## 494 4156523                 hyaku                                ç a k u
    ## 495 4169912                 gohya                                ɡ o ç a
    ## 496 4176559                  hyaa                                   ç aː
    ## 497 4178304              hyaanyaa                              ç aː ɲ aː
    ## 498 4178373                 hyaaa                                 ç aː a
    ## 499 4183200                 hyaku                                ç a k u
    ## 500 4183204               hyakuen                            ç a k u e n
    ## 501 4183912              hyunhyun                            ç u n ç u n
    ## 502 4186764                 hyuun                                 ç uː n
    ## 503 4186766                 hyuun                                 ç uː n
    ## 504 4193830           hyottoshite                      ç o t t o ʃ i t e
    ## 505 4197452                 hyaku                                ç a k u
    ## 506 4200226                 hyaku                                ç a k u
    ## 507 4200251                 hyaku                                ç a k u
    ## 508 4205779           hyottoshite                      ç o t t o ʃ i t e
    ## 509 4208474                 hyaku                                ç a k u
    ## 510 4208893             nihyakuen                        n i ç a k u e n
    ## 511 4209793               hyakuen                            ç a k u e n
    ## 512 4209859               hyakuen                            ç a k u e n
    ## 513 4217705                   hya                                    ç a
    ## 514 4221017             kyuuhyaku                          kʲ uː ç a k u
    ## 515 4222869             gohyakuen                        ɡ o ç a k u e n
    ## 516 4223233           hyottoshite                      ç o t t o ʃ i t e
    ## 517 4230145                  hyuu                                   ç uː
    ## 518 4236626                 hyaku                                ç a k u
    ## 519 4236631                 hyaku                                ç a k u
    ## 520 4237598                buhyee                               b u ç eː
    ## 521 4237988              hyoogara                           ç oː ɡ a r a
    ## 522 4240219                  hyuu                                   ç uː
    ## 523 4240396         hyoonpiinpyoo                   ç oː n p i i n pʲ oː
    ## 524 4240749         ohyakushoosan                   o ç a k u ʃ oː s a n
    ## 525 4241136               hyooshi                               ç oː ʃ i
    ## 526 4246508                  hyuu                                   ç uː
    ## 527 4250424           chuumonhyoo                       t̠ʃ uː m o n ç oː
    ## 528 4263967                   hyu                                    ç u
    ## 529 4265876                   hyu                                    ç u
    ## 530 4266584                  hyuq                                  ç u q
    ## 531 4267696                  hyaq                                  ç a q
    ## 532 4267742             shaqhyoa:                            ʃ a q ç o a
    ## 533 4268047                  hyoo                                   ç oː
    ## 534 4268048                  hyoo                                   ç oː
    ## 535 4268049                  hyoo                                   ç oː
    ## 536 4268050                  hyoo                                   ç oː
    ## 537 4268052                   hyo                                    ç o
    ## 538 4268053                  hyoo                                   ç oː
    ## 539 4268067                  hyoo                                   ç oː
    ## 540 4268091                  hyoo                                   ç oː
    ## 541 4268092                  hyoo                                   ç oː
    ## 542 4268174                  hyoq                                  ç o q
    ## 543 4268183                  hyoq                                  ç o q
    ## 544 4268397                  hyoo                                   ç oː
    ## 545 4268399                  hyoo                                   ç oː
    ## 546 4268400                  hyoo                                   ç oː
    ## 547 4268428                  hyoq                                  ç o q
    ## 548 4269978                  hyuu                                   ç uː
    ## 549 4270518           hyottoshite                      ç o t t o ʃ i t e
    ## 550 4272176                   hyu                                    ç u
    ## 551 4272210                   hyu                                    ç u
    ## 552 4274297                   hyo                                    ç o
    ## 553 4274830                   hyu                                    ç u
    ## 554 4275527            hyuugoro:n                         ç uː ɡ o r o n
    ## 555 4278938              hyakunen                          ç a k u n e n
    ## 556 4278939              hyakunen                          ç a k u n e n
    ## 557 4278950              hyakunen                          ç a k u n e n
    ## 558 4284776                   hyo                                    ç o
    ## 559 4286503                  hyuu                                   ç uː
    ## 560 4287935                  hyuu                                   ç uː
    ## 561 4287937             hyuttanko                        ç u t t a n k o
    ## 562 4288002                  hyun                                  ç u n
    ## 563 4290203                   hyu                                    ç u
    ## 564 4291801             nihyakuen                        n i ç a k u e n
    ## 565 4291802             nihyakuen                        n i ç a k u e n
    ## 566 4291805             nihyakuen                        n i ç a k u e n
    ## 567 4291879             nihyakuen                        n i ç a k u e n
    ## 568 4291889             nihyakuen                        n i ç a k u e n
    ## 569 4291898             nihyakuen                        n i ç a k u e n
    ## 570 4291899             nihyakuen                        n i ç a k u e n
    ## 571 4291905             nihyakuen                        n i ç a k u e n
    ## 572 4291915             nihyakuen                        n i ç a k u e n
    ## 573 4291919                 hyaku                                ç a k u
    ## 574 4291935             nihyakuen                        n i ç a k u e n
    ## 575 4291939               nihyaku                            n i ç a k u
    ## 576 4291940             nihyakuen                        n i ç a k u e n
    ## 577 4291945             nihyakuen                        n i ç a k u e n
    ## 578 4291949               hyakuen                            ç a k u e n
    ## 579 4291990                 hyaku                                ç a k u
    ## 580 4292012             nihyakuen                        n i ç a k u e n
    ## 581 4292169                 hyaku                                ç a k u
    ## 582 4292175               hyakuen                            ç a k u e n
    ## 583 4292176                   hya                                    ç a
    ## 584 4292200             nihyakuen                        n i ç a k u e n
    ## 585 4292214                 hyaku                                ç a k u
    ## 586 4292220                 hyaku                                ç a k u
    ## 587 4292226               hyakuen                            ç a k u e n
    ## 588 4292470                 hyaku                                ç a k u
    ## 589 4292680             gohyakuen                        ɡ o ç a k u e n
    ## 590 4295120                 hyaku                                ç a k u
    ## 591 4295121                 hyaku                                ç a k u
    ## 592 4295310             nihyakuen                        n i ç a k u e n
    ## 593 4295315             nihyakuen                        n i ç a k u e n
    ## 594 4295321               nihyaku                            n i ç a k u
    ## 595 4295323             nihyakuen                        n i ç a k u e n
    ## 596 4300852                   hyu                                    ç u
    ## 597 4300853                   hyu                                    ç u
    ## 598 4301007                   hyu                                    ç u
    ## 599 4301031                   hyu                                    ç u
    ## 600 4301033                   hyu                                    ç u
    ## 601 4301034                   hyu                                    ç u
    ## 602 4301035                   hyu                                    ç u
    ## 603 4301036                   hyu                                    ç u
    ## 604 4301037                   hyu                                    ç u
    ## 605 4301038                   hyu                                    ç u
    ## 606 4301151                  hyun                                  ç u n
    ## 607 4301152                  hyun                                  ç u n
    ## 608 4301153                  hyun                                  ç u n
    ## 609 4301163                  hyun                                  ç u n
    ## 610 4301168                   hyu                                    ç u
    ## 611 4301494                   hyu                                    ç u
    ## 612 4301498                   hyu                                    ç u
    ## 613 4305061           hyakuendama                    ç a k u e n d a m a
    ## 614 4305064           hyakuendama                    ç a k u e n d a m a
    ## 615 4305071           hyakuendama                    ç a k u e n d a m a
    ## 616 4307183           nanahyakuen                    n a n a ç a k u e n
    ## 617 4307616             nihyakuen                        n i ç a k u e n
    ## 618 4307630             nihyakuen                        n i ç a k u e n
    ## 619 4312384                   hyo                                    ç o
    ## 620 4317374            yonhyakuen                      j o n ç a k u e n
    ## 621 4322013               hyakuen                            ç a k u e n
    ## 622 4322016               hyakuen                            ç a k u e n
    ## 623 4322583                 hihyu                                h i ç u
    ## 624 4324385             hyuhyuhyu                            ç u ç u ç u
    ## 625 4325299           hyottoshite                      ç o t t o ʃ i t e
    ## 626 4325609          hyuruhyuhyuq                      ç u r u ç u ç u q
    ## 627 4325610          hyuqhyuqhyu:                        ç u q ç u q ç u
    ## 628 4325623              hyuruhyu                            ç u r u ç u
    ## 629 4325666             hyuhyuhyu                            ç u ç u ç u
    ## 630 4325693                   hyu                                    ç u
    ## 631 4327191           hyottoshite                      ç o t t o ʃ i t e
    ## 632 4330503              hyuuhyuu                              ç uː ç uː
    ## 633 4334487               hyoomen                             ç oː m e n
    ## 634 4337815                 hyaku                                ç a k u
    ## 635 4337818                 hyaku                                ç a k u
    ## 636 4337823                 hyaku                                ç a k u
    ## 637 4337847                 hyaku                                ç a k u
    ## 638 4337849                 hyaku                                ç a k u
    ## 639 4338020                 hyaku                                ç a k u
    ## 640 4338036                 hyaku                                ç a k u
    ## 641 4338042                 hyaku                                ç a k u
    ## 642 4340942           hyottoshite                      ç o t t o ʃ i t e
    ## 643 4340947           hyottoshite                      ç o t t o ʃ i t e
    ## 644 4342725                   hyu                                    ç u
    ## 645 4342773                  hyaa                                   ç aː
    ## 646 4344861                  hyuq                                  ç u q
    ## 647 4346532        hyakutemmanten              ç a k u t e m m a n t e n
    ## 648 4346580              hyakuten                          ç a k u t e n
    ## 649 4346636              hyakuten                          ç a k u t e n
    ## 650 4346654              hyakuten                          ç a k u t e n
    ## 651 4358274              hyakuten                          ç a k u t e n
    ## 652 4358278              hyakuten                          ç a k u t e n
    ## 653 4358347              hyakuten                          ç a k u t e n
    ## 654 4358444             nihyakuen                        n i ç a k u e n
    ## 655 4358586                  hyaa                                   ç aː
    ## 656 4359914        hyakutemmanten              ç a k u t e m m a n t e n
    ## 657 4359948        hyakutemmanten              ç a k u t e m m a n t e n
    ## 658 4359996              hyakuten                          ç a k u t e n
    ## 659 4360031        hyakutemmanten              ç a k u t e m m a n t e n
    ## 660 4360078        hyakutemmanten              ç a k u t e m m a n t e n
    ## 661 4363187           hyottoshite                      ç o t t o ʃ i t e
    ## 662 4365668                  hyoq                                  ç o q
    ## 663 4365669                  hyoq                                  ç o q
    ## 664 4365670                  hyoq                                  ç o q
    ## 665 4365671                  hyoq                                  ç o q
    ## 666 4365854                  hyuu                                   ç uː
    ## 667 4366007                  hyaa                                   ç aː
    ## 668 4368355               nihyaku                            n i ç a k u
    ## 669 4368358               nihyaku                            n i ç a k u
    ## 670 4369112                 hyaku                                ç a k u
    ## 671 4369187               hyakuen                            ç a k u e n
    ## 672 4369286             gohyakuen                        ɡ o ç a k u e n
    ## 673 4369302             gohyakuen                        ɡ o ç a k u e n
    ## 674 4369312             gohyakuen                        ɡ o ç a k u e n
    ## 675 4369325             gohyakuen                        ɡ o ç a k u e n
    ## 676 4369326             gohyakuen                        ɡ o ç a k u e n
    ## 677 4369349             gohyakuen                        ɡ o ç a k u e n
    ## 678 4369364               hyakuen                            ç a k u e n
    ## 679 4369365               hyakuen                            ç a k u e n
    ## 680 4369368               hyakuen                            ç a k u e n
    ## 681 4369375               hyakuen                            ç a k u e n
    ## 682 4369379               hyakuen                            ç a k u e n
    ## 683 4369380               hyakuen                            ç a k u e n
    ## 684 4369384               hyakuen                            ç a k u e n
    ## 685 4369388               hyakuen                            ç a k u e n
    ## 686 4369392               hyakuen                            ç a k u e n
    ## 687 4369394               hyakuen                            ç a k u e n
    ## 688 4369396               hyakuen                            ç a k u e n
    ## 689 4369409               hyakuen                            ç a k u e n
    ## 690 4369414               hyakuen                            ç a k u e n
    ## 691 4369418               hyakuen                            ç a k u e n
    ## 692 4369730             gohyakuen                        ɡ o ç a k u e n
    ## 693 4369809               hyakuen                            ç a k u e n
    ## 694 4369860               hyakuen                            ç a k u e n
    ## 695 4369871               hyakuen                            ç a k u e n
    ## 696 4369874             gohyakuen                        ɡ o ç a k u e n
    ## 697 4369883               hyakuen                            ç a k u e n
    ## 698 4369885               hyakuen                            ç a k u e n
    ## 699 4374629               gohyaku                            ɡ o ç a k u
    ## 700 4378485                 hyuun                                 ç uː n
    ## 701 4379642                  hyun                                  ç u n
    ## 702 4379690                  hyun                                  ç u n
    ## 703 4379695                  hyun                                  ç u n
    ## 704 4379762                  hyuu                                   ç uː
    ## 705 4382487                  hyun                                  ç u n
    ## 706 4390491                hyaapu                               ç aː p u
    ## 707 4390765                 hyuun                                 ç uː n
    ## 708 4390770                 hyuun                                 ç uː n
    ## 709 4390771                 hyuun                                 ç uː n
    ## 710 4392942           hyottoshite                      ç o t t o ʃ i t e
    ## 711 4396077                  hyee                                   ç eː
    ## 712 4396094                  hyuq                                  ç u q
    ## 713 4397167            uhyahyahei                        u ç a ç a h e i
    ## 714 4398786           hyooshigiri                       ç oː ʃ i ɡ i r i
    ## 715 4398909                  hyuu                                   ç uː
    ## 716 4400791                 hyuun                                 ç uː n
    ## 717 4403223                  hyuu                                   ç uː
    ## 718 4404445                  hyaa                                   ç aː
    ## 719 4406186                 hyuun                                 ç uː n
    ## 720 4406187                  hyuu                                   ç uː
    ## 721 4408636                 hyuun                                 ç uː n
    ## 722 4409346                  hyuu                                   ç uː
    ## 723 4409347                  hyuu                                   ç uː
    ## 724 4409352                  hyuu                                   ç uː
    ## 725 4410999               hyoadaa                             ç o a d aː
    ## 726 4416837                 hyuun                                 ç uː n
    ## 727 4416841                 hyuun                                 ç uː n
    ## 728 4417179                   hye                                    ç e
    ## 729 4420216                  hyaq                                  ç a q
    ## 730 4423202                  hyuu                                   ç uː
    ## 731 4423203                  hyuu                                   ç uː
    ## 732 4423206                  hyuu                                   ç uː
    ## 733 4423296                 fuhye                                ɸ u ç e
    ## 734 4429098                  hye:                                    ç e
    ## 735 4429270                   hyu                                    ç u
    ## 736 4429271                   hyu                                    ç u
    ## 737 4431627               hyatchi                             ç a t t̠ʃ i
    ## 738 4431628               hyatchi                             ç a t t̠ʃ i
    ## 739 4431640                 uhya:                                  u ç a
    ## 740 4432577                  hyuu                                   ç uː
    ## 741 4432581                  hyuu                                   ç uː
    ## 742 4432582                  hyuu                                   ç uː
    ## 743 4432583                  hyuu                                   ç uː
    ## 744 4434049                 hyuun                                 ç uː n
    ## 745 4435035                  hyuu                                   ç uː
    ## 746 4445052              hyakunen                          ç a k u n e n
    ## 747 4452621               hyootan                             ç oː t a n
    ## 748 4452625               hyootan                             ç oː t a n
    ## 749 4452627               hyootan                             ç oː t a n
    ## 750 4452630               hyootan                             ç oː t a n
    ## 751 4452632               hyootan                             ç oː t a n
    ## 752 4460706                 hyaku                                ç a k u
    ## 753 4461497              hyakunen                          ç a k u n e n
    ## 754 4473007                  hyoo                                   ç oː
    ## 755 4473024               hyoosan                             ç oː s a n
    ## 756 4481771             nihyakuen                        n i ç a k u e n
    ## 757 4483596                 dehyo                                d e ç o
    ## 758 4485019                  hyuu                                   ç uː
    ## 759 4485967              yaruhyoo                           j a r u ç oː
    ## 760 4491426                  hyoo                                   ç oː
    ## 761 4492618                  hyuu                                   ç uː
    ## 762 4503613                  hyaa                                   ç aː
    ## 763 4503627                  hyaa                                   ç aː
    ## 764 4527584                   hyu                                    ç u
    ## 765 4530096               hyakuen                            ç a k u e n
    ## 766 4530098               hyakuen                            ç a k u e n
    ## 767 4536699               hyakuen                            ç a k u e n
    ## 768 4538112             hyooshita                           ç oː ʃ i t a
    ## 769 4541758               hyakuen                            ç a k u e n
    ## 770 4541816               hyakuen                            ç a k u e n
    ## 771 4541817               hyakuen                            ç a k u e n
    ## 772 4541818               hyakuen                            ç a k u e n
    ## 773 4541875               hyakuen                            ç a k u e n
    ## 774 4541878               hyakuen                            ç a k u e n
    ## 775 4541912               hyakuen                            ç a k u e n
    ## 776 4542009               hyakuen                            ç a k u e n
    ## 777 4542023                 hyaku                                ç a k u
    ## 778 4542024                 hyaku                                ç a k u
    ## 779 4542028                 hyaku                                ç a k u
    ## 780 4542030                 hyaku                                ç a k u
    ## 781 4542055               hyakuen                            ç a k u e n
    ## 782 4542835             gohyakuen                        ɡ o ç a k u e n
    ## 783 4569681                  hyuu                                   ç uː
    ## 784 4574468              hyakkiro                          ç a k k i r o
    ## 785 4574469              hyakkiro                          ç a k k i r o
    ## 786 4576057                  hyuu                                   ç uː
    ## 787 4576080                 hyaou                                ç a o u
    ## 788 4578582                hyahho                              ç a h h o
    ## 789 4578583             hyahhohya                          ç a h h o ç a
    ## 790 4586066        hyakuenkinichu               ç a k u e n k i n i t̠ʃ u
    ## 791 4586068        hyakuenkinitsu               ç a k u e n k i n i ts u
    ## 792 4586072               hyakuen                            ç a k u e n
    ## 793 4588258             nihyakuen                        n i ç a k u e n
    ## 794 4592563                  hyaa                                   ç aː
    ## 795 4592568                  hyaa                                   ç aː
    ## 796 4600779               hyakuen                            ç a k u e n
    ## 797 4600783                 hyaku                                ç a k u
    ## 798 4600788               hyakuen                            ç a k u e n
    ## 799 4601100               hyakuen                            ç a k u e n
    ## 800 4601103               hyakuen                            ç a k u e n
    ## 801 4601105               hyakuen                            ç a k u e n
    ## 802 4601298                hyakko                              ç a k k o
    ## 803 4601337           hyappyakuen                     ç a p pʲ a k u e n
    ## 804 4608842                 hyuun                                 ç uː n
    ## 805 4610196                duhyuu                               d u ç uː
    ## 806 4619457                  hyuo                                  ç u o
    ## 807 4624015                  ohya                                  o ç a
    ## 808 4625437                  hyuu                                   ç uː
    ## 809 4628550               hyoojoo                             ç oː d̠ʒ oː
    ## 810 4634180                  hyuu                                   ç uː
    ## 811 4635334               hyakuen                            ç a k u e n
    ## 812 4635341               hyakuen                            ç a k u e n
    ## 813 4635349               hyakuen                            ç a k u e n
    ## 814 4635369               hyakuen                            ç a k u e n
    ## 815 4639963      wahyahyahyahyaan                 w a ç a ç a ç a ç aː n
    ## 816 4643290                  hyaa                                   ç aː
    ## 817 4644872                  hyun                                  ç u n
    ## 818 4644931                hyuuma                               ç uː m a
    ## 819 4648885             iihyahhya                          i i ç a h ç a
    ## 820 4648886             iihyahhya                          i i ç a h ç a
    ## 821 4648887            iihyahhyai                        i i ç a h ç a i
    ## 822 4648893            iihyahhyai                        i i ç a h ç a i
    ## 823 4652861            hyikushoon                         ç i k u ʃ oː n
    ## 824 4654478                   hyu                                    ç u
    ## 825 4655667                  hyuu                                   ç uː
    ## 826 4699035                  hyoo                                   ç oː
    ## 827 4699565                  hyuu                                   ç uː
    ## 828 4699567                  hyuu                                   ç uː
    ## 829 4699569                  hyuu                                   ç uː
    ## 830 4699571                  hyuu                                   ç uː
    ## 831 4699586                  hyuu                                   ç uː
    ## 832 4699588                  hyuu                                   ç uː
    ## 833 4699619                  hyuu                                   ç uː
    ## 834 4699627                  hyuu                                   ç uː
    ## 835 4699629                  hyuu                                   ç uː
    ## 836 4699631                  hyuu                                   ç uː
    ## 837 4699633                  hyuu                                   ç uː
    ## 838 4699635                  hyuu                                   ç uː
    ## 839 4702323                  hyuu                                   ç uː
    ## 840 4702325                  hyuu                                   ç uː
    ## 841 4702327                  hyuu                                   ç uː
    ## 842 4704140           hyogupamman                    ç o ɡ u p a m m a n
    ## 843 4704154           hyokopamman                    ç o k o p a m m a n
    ## 844 4704195           hyokupamman                    ç o k u p a m m a n
    ## 845 4706357               ka:hyan                              k a ç a n
    ## 846 4706710               ahyobu:                              a ç o b u
    ## 847 4707085                   hyo                                    ç o
    ## 848 4707372                 dohya                                d o ç a
    ## 849 4707407                  hyoo                                   ç oː
    ## 850 4707915                 ohyoo                                 o ç oː
    ## 851 4707931                dahyu:                                d a ç u
    ## 852 4708417                  hyuu                                   ç uː
    ## 853 4708713                 dehyu                                d e ç u
    ## 854 4708867                 nahyu                                n a ç u
    ## 855 4708868                 nahyu                                n a ç u
    ## 856 4708885                  hyo:                                    ç o
    ## 857 4709045                 ihhyu                                i h ç u
    ## 858 4709204                 dehyu                                d e ç u
    ## 859 4709589                 dehyu                                d e ç u
    ## 860 4709591              oyahyumi                          o j a ç u m i
    ## 861 4709594                 dehyu                                d e ç u
    ## 862 4709825                 dehyo                                d e ç o
    ## 863 4709851                 dehyu                                d e ç u
    ## 864 4709869                  ehyu                                  e ç u
    ## 865 4709877        hambaaguyahyan               h a m b aː ɡ u j a ç a n
    ## 866 4709879        okaimonoyahyan              o k a i m o n o j a ç a n
    ## 867 4709884        hamba:guyahyan                h a m b a ɡ u j a ç a n
    ## 868 4709888          omochayahyan                   o m o t̠ʃ a j a ç a n
    ## 869 4709957                 dehyu                                d e ç u
    ## 870 4710239               hyakuen                            ç a k u e n
    ## 871 4710245             nihyakuen                        n i ç a k u e n
    ## 872 4710276               hyakuen                            ç a k u e n
    ## 873 4710290               hyakuen                            ç a k u e n
    ## 874 4710456             murahyaki                        m u r a ç a k i
    ## 875 4712299                 nhyoo                                 n ç oː
    ## 876 4713757             gohyakuen                        ɡ o ç a k u e n
    ## 877 4713794              gohyaken                          ɡ o ç a k e n
    ## 878 4713795              gohyaken                          ɡ o ç a k e n
    ## 879 4713809             gohyakuen                        ɡ o ç a k u e n
    ## 880 4713998                 hyuu:                                   ç uː
    ## 881 4715814              hyuuduun                            ç uː d uː n
    ## 882 4715815              hyuuduun                            ç uː d uː n
    ## 883 4715818              hyuuduun                            ç uː d uː n
    ## 884 4715913                hyugoi                              ç u ɡ o i
    ## 885 4716205                 hyuun                                 ç uː n
    ## 886 4717950                  hyuu                                   ç uː
    ## 887 4721590                  hyuu                                   ç uː
    ## 888 4721779                  hyuu                                   ç uː
    ## 889 4721853                  hyuu                                   ç uː
    ## 890 4722063                  hyuu                                   ç uː
    ## 891 4722168                  hyuu                                   ç uː
    ## 892 4723718                   hyu                                    ç u
    ## 893 4724791                   hyu                                    ç u
    ## 894 4724792                   hyu                                    ç u
    ## 895 4724841                  hyuu                                   ç uː
    ## 896 4724889               hyoosho                               ç oː ʃ o
    ## 897 4729333           nanahyagata                    n a n a ç a ɡ a t a
    ## 898 4729680              hyuahaqe                          ç u a h a q e
    ## 899 4730227    nanasengohyakugata      n a n a s e n ɡ o ç a k u ɡ a t a
    ## 900 4731352                   hya                                    ç a
    ## 901 4738319               hyooshi                               ç oː ʃ i
    ## 902 4741582                   hya                                    ç a
    ## 903 4742046                   hyu                                    ç u
    ## 904 4742056                   hyu                                    ç u
    ## 905 4742084                  hyun                                  ç u n
    ## 906 4742768                  hyun                                  ç u n
    ## 907 4743346                   hya                                    ç a
    ## 908 4743397          kowarabahyu:                    k o w a r a b a ç u
    ## 909 4743404          kowarabahya:                    k o w a r a b a ç a
    ## 910 4743410           kowarabahyu                    k o w a r a b a ç u
    ## 911 4743849                  hyuq                                  ç u q
    ## 912 4743854                  hyuq                                  ç u q
    ## 913 4743971                 dehyo                                d e ç o
    ## 914 4744939                  hyoo                                   ç oː
    ## 915 4745132            hyantahyan                        ç a n t a ç a n
    ## 916 4746375                  hya:                                    ç a
    ## 917 4750434                hyuden                              ç u d e n
    ## 918 4750435                hyuden                              ç u d e n
    ## 919 4750436                hyuden                              ç u d e n
    ## 920 4750437                hyuden                              ç u d e n
    ## 921 4758875       hyakuhachijuudo             ç a k u h a t̠ʃ i d̠ʒ uː d o
    ## 922 4758920       hyakuhachijuudo             ç a k u h a t̠ʃ i d̠ʒ uː d o
    ## 923 4761957                  hyoo                                   ç oː
    ## 924 4765436                 hyaku                                ç a k u
    ## 925 4765438                 hyaku                                ç a k u
    ## 926 4770351                  hyuu                                   ç uː
    ## 927 4771130                 puhyo                                p u ç o
    ## 928 4771160                  hyuu                                   ç uː
    ## 929 4771174                  hyuu                                   ç uː
    ## 930 4771177                  hyuu                                   ç uː
    ## 931 4771359                   hyu                                    ç u
    ## 932 4771384                   hyu                                    ç u
    ## 933 4771429                  hyuu                                   ç uː
    ## 934 4771560                 puhyu                                p u ç u
    ## 935 4771561                 puhyu                                p u ç u
    ## 936 4771562                 puhyu                                p u ç u
    ## 937 4771563                 puhyu                                p u ç u
    ## 938 4771605                  hyun                                  ç u n
    ## 939 4771658                hyudon                              ç u d o n
    ## 940 4773069                 dehyo                                d e ç o
    ## 941 4773197                 dehyo                                d e ç o
    ## 942 4774906                  hyoi                                  ç o i
    ## 943 4774913                  hyaa                                   ç aː
    ## 944 4775129              okaahyan                           o k aː ç a n
    ## 945 4775169              okaahyan                           o k aː ç a n
    ## 946 4775385              okaahyan                           o k aː ç a n
    ## 947 4775488                   hya                                    ç a
    ## 948 4775992                  hyuu                                   ç uː
    ## 949 4776168                   hya                                    ç a
    ## 950 4776804             murahyaki                        m u r a ç a k i
    ## 951 4776845                 dehyo                                d e ç o
    ## 952 4777239              okaahyan                           o k aː ç a n
    ## 953 4777311                 hyoko                                ç o k o
    ## 954 4777375              okaahyan                           o k aː ç a n
    ## 955 4779291                  hyuu                                   ç uː
    ## 956 4780116         daitanhyooron                 d a i t a n ç oː r o n
    ## 957 4781401              hyoogara                           ç oː ɡ a r a
    ## 958 4784091               hyakken                            ç a k k e n
    ## 959 4785111                 hyumo                                ç u m o
    ## 960 4788433                hyokko                              ç o k k o
    ## 961 4788434              mihyokko                          m i ç o k k o
    ## 962 4792752                  hyuu                                   ç uː
    ## 963 4800706                   hyu                                    ç u
    ## 964 4804044                 hyaku                                ç a k u
    ## 965 4804052                 hyaku                                ç a k u
    ## 966 4804068                 hyaku                                ç a k u
    ## 967 4805924               Kaahyan                             k aː ç a n
    ## 968 4808038                pohyun                              p o ç u n
    ## 969 4813609                hyuuma                               ç uː m a
    ## 970 4813616                hyuuma                               ç uː m a
    ## 971 4813685                 hyaku                                ç a k u
    ## 972 4813907 hyakujuunikiromeetoru ç a k u d̠ʒ uː n i k i r o m eː t o r u
    ## 973 4813940              hyakkiro                          ç a k k i r o
    ## 974 4814114         hyooshooshiki                      ç oː ʃ oː ʃ i k i
    ## 975 4814127                 hyaku                                ç a k u

Russian
-------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Russian.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Russian") %>% select(language, segments)
expect_true(all(x$segments %in% op$simple_target))
```

Sesotho
-------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Sesotho.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Sesotho") %>% select(language, segments)
# expect_true(all(x$segments %in% op$simple_target))

# This is also an issue of <c> and in loanwords.
x[which(!(x$segments %in% op$simple_target)),]
```

    ## # A tibble: 1 x 2
    ## # Groups:   language [1]
    ##   language segments
    ##   <chr>    <chr>   
    ## 1 Sesotho  �

``` r
df %>% filter(language=="Sesotho") %>% filter(grepl("� ", segments)) %>% select(word_id, word, segments)
```

    ##     word_id         word                segments
    ## 1   6861382      secotch           s e � o t � h
    ## 2   6861392      secotch           s e � o t � h
    ## 3   6861397      secotch           s e � o t � h
    ## 4   6861399      secotch           s e � o t � h
    ## 5   6861414      secotch           s e � o t � h
    ## 6   6861417      secotch           s e � o t � h
    ## 7   6861431      secotch           s e � o t � h
    ## 8   6861432       scotch             s � o t � h
    ## 9   6861525       scotch             s � o t � h
    ## 10  6868423     direcote         d i r e � o t e
    ## 11  6872617      vascomo           v a s � o m o
    ## 12  6872643       vascom             v a s � o m
    ## 13  6877962        cente               � e n t e
    ## 14  6877966        cente               � e n t e
    ## 15  6877971        cente               � e n t e
    ## 16  6877976        cente               � e n t e
    ## 17  6877981        cente               � e n t e
    ## 18  6877986        cente               � e n t e
    ## 19  6878309      dicenta           d i � e n t a
    ## 20  6891728  helicoptara   h e l i � o t t a r a
    ## 21  6891734  helicoptara   h e l i � o t t a r a
    ## 22  6891764     dicapisi         d i � a t i s i
    ## 23  6891765     dicapisi         d i � a t i s i
    ## 24  6891772     dicapisi         d i � a t i s i
    ## 25  6891796       capisi             � a t i s i
    ## 26  6912347        chick               � h i � k
    ## 27  6917560     lebiscus         l e b i s � u s
    ## 28  6919094      cabbage           � a b b a χ e
    ## 29  6921091      secomba           s e � o m b a
    ## 30  6921096      secomba           s e � o m b a
    ## 31  6923144        comba               � o m b a
    ## 32  6923145        comba               � o m b a
    ## 33  6923179        comba               � o m b a
    ## 34  6923180         coma                 � o m a
    ## 35  6933245      Patrick           p a t r i � k
    ## 36  6933885       kutica             k u t i � a
    ## 37  6935803      Patrick           p a t r i � k
    ## 38  6964001         cold                 � o l d
    ## 39  6965056         loca                 l o � a
    ## 40  6965057        court               � o u r t
    ## 41  6965059        local               l o � a l
    ## 42  6965060        court               � o u r t
    ## 43  6965064          col                   � o l
    ## 44  6965065        court               � o u r t
    ## 45  6965067          col                   � o l
    ## 46  6965068        court               � o u r t
    ## 47  6965087          col                   � o l
    ## 48  6965088        court               � o u r t
    ## 49  6965089          col                   � o l
    ## 50  6965090        court               � o u r t
    ## 51  6965750          cok                   � o k
    ## 52  6965751         kock                 k o � k
    ## 53  6965755         rock                 r o � k
    ## 54  6965758         rock                 r o � k
    ## 55  6969277        black               b l a � k
    ## 56  6969285        black               b l a � k
    ## 57  6969296        black               b l a � k
    ## 58  6970003    mabiscuse       m a b i s � u s e
    ## 59  6971081    vascomong       v a s � o m o n χ
    ## 60  6975069  casteroleng   � a s t e r o l e n χ
    ## 61  6982036  aacaotapisa   a a � a o t a t i s a
    ## 62  6985995      capsela           � a t s e l a
    ## 63  6985998      capsela           � a t s e l a
    ## 64  6989694         ihcu                 i h � u
    ## 65  6991530       Macose             m a � o s e
    ## 66  6991763        dicap               d i � a t
    ## 67  6991788         capo                 � a t o
    ## 68  6992940          can                   � a n
    ## 69  6999814   atlacalana     a t l a � a l a n a
    ## 70  6999815      acalana           a � a l a n a
    ## 71  6999816       aucala             a u � a l a
    ## 72  7000117 chekecolaite � h e k e � o l a i t e
    ## 73  7000174    katlacala       k a t l a � a l a
    ## 74  7000946        compa               � o m t a
    ## 75  7000947        compa               � o m t a
    ## 76  7000954     comacoma         � o m a � o m a
    ## 77  7001406        comba               � o m b a
    ## 78  7001407        comba               � o m b a
    ## 79  7001448        comba               � o m b a
    ## 80  7001449        comba               � o m b a
    ## 81  7002111        comba               � o m b a
    ## 82  7002112        comba               � o m b a
    ## 83  7002124        comba               � o m b a
    ## 84  7002125        comba               � o m b a
    ## 85  7003526      scaftin           s � a f t i n
    ## 86  7007597        clean               � l e a n
    ## 87  7010354       africa             a f r i � a
    ## 88  7012510    dicandele       d i � a n d e l e
    ## 89  7012566        Rocki               r o � k i
    ## 90  7012724      cubothe           � u b o t h e
    ## 91  7012872      candele           � a n d e l e
    ## 92  7013472       clache             � l a � h e
    ## 93  7016526   dipancakes     d i t a n � a k e s
    ## 94  7016527   dipancakes     d i t a n � a k e s
    ## 95  7016528     pancakes         t a n � a k e s
    ## 96  7016529   dipancakes     d i t a n � a k e s
    ## 97  7017057        Lency               l e n � j
    ## 98  7017063        Lency               l e n � j
    ## 99  7017080        Lency               l e n � j
    ## 100 7017560      bascomo           b a s � o m o
    ## 101 7017564      bascomo           b a s � o m o
    ## 102 7021625         clin                 � l i n
    ## 103 7023469        Nancy               n a n � j
    ## 104 7028790    cliniking       � l i n i k i n χ
    ## 105 7033947    Florencia       f l o r e n � i a
    ## 106 7041120   Florencina     f l o r e n � i n a
    ## 107 7048108      scaftin           s � a f t i n
    ## 108 7052954        clapa               � l a t a
    ## 109 7053402     blacksam         b l a � k s a m
    ## 110 7062113       camera             � a m e r a
    ## 111 7062131       camera             � a m e r a
    ## 112 7068169        compa               � o m t a
    ## 113 7068170        compa               � o m t a
    ## 114 7068357        compa               � o m t a
    ## 115 7068358        compa               � o m t a
    ## 116 7068683    cliniking       � l i n i k i n χ
    ## 117 7078672         caps                 � a t s
    ## 118 7078673         caps                 � a t s
    ## 119 7079524   disenclase     d i s e n � l a s e
    ## 120 7079530   disenclase     d i s e n � l a s e
    ## 121 7081180   disanclase     d i s a n � l a s e
    ## 122 7081216   disenclase     d i s e n � l a s e
    ## 123 7081225   disenclese     d i s e n � l e s e
    ## 124 7083063   lisenclase     l i s e n � l a s e

Turkish
-------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Turkish.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Turkish") %>% select(language, segments)
expect_true(all(x$segments %in% op$simple_target))
```

Yucatec
-------

``` r
op <- read.delim('https://raw.githubusercontent.com/bambooforest/orthography-profiles/master/Yucatec.tsv', header=T, stringsAsFactors = F, quote = '"', sep="\t", comment.char = "\\", na.strings = "NULL")
x <- counts %>% filter(language=="Yucatec") %>% select(language, segments)
# expect_true(all(x$segments %in% op$simple_target))

# This is an issue of <H> and <h> 
x[which(!(x$segments %in% op$simple_target)),]
```

    ## # A tibble: 1 x 2
    ## # Groups:   language [1]
    ##   language segments
    ##   <chr>    <chr>   
    ## 1 Yucatec  �

``` r
df %>% filter(language=="Yucatec") %>% filter(grepl("� ", segments)) %>% select(word_id, word, segments)
```

    ##     word_id              word                          segments
    ## 1   8220979           Hunukʼu                     � u n u k ʔ u
    ## 2   8223946           Hilario                     � i l a r i o
    ## 3   8223949           Hilario                     � i l a r i o
    ## 4   8223952           Hilario                     � i l a r i o
    ## 5   8223954           Hilario                     � i l a r i o
    ## 6   8224027           Hilario                     � i l a r i o
    ## 7   8224028           Hilario                     � i l a r i o
    ## 8   8228417               Hey                             � e j
    ## 9   8229780               Hey                             � e j
    ## 10  8233835               Hey                             � e j
    ## 11  8234356              Heey                           � e e j
    ## 12  8252188       Hununkʼuheʼ             � u n u n k ʔ u h e ʔ
    ## 13  8255035               Han                             � a n
    ## 14  8255047               Hnn                             � n n
    ## 15  8255048               Heʼ                             � e ʔ
    ## 16  8255070              Helá                         � e l a �
    ## 17  8255077            Haypʼe                      � a j pʼ ʔ e
    ## 18  8255086               Haa                             � a a
    ## 19  8255163             Haʼan                         � a ʔ a n
    ## 20  8255242            Heʼlee                       � e ʔ l e e
    ## 21  8255356               Han                             � a n
    ## 22  8255400               Hnn                             � n n
    ## 23  8255406              Haan                           � a a n
    ## 24  8255421            Hoʼlak                       � o ʔ l a k
    ## 25  8255432               Hnn                             � n n
    ## 26  8255433             Haʼah                         � a ʔ a h
    ## 27  8255490             Haʼah                         � a ʔ a h
    ## 28  8255529             Hnʼnh                         � n ʔ n h
    ## 29  8255551               Hay                             � a j
    ## 30  8255566            Heʼlaa                       � e ʔ l a a
    ## 31  8255623              Hnnʼ                           � n n ʔ
    ## 32  8255624            Heʼlaʼ                       � e ʔ l a ʔ
    ## 33  8255625               Hnn                             � n n
    ## 34  8255696             Helex                         � e l e ʃ
    ## 35  8255704            Heʼlaa                       � e ʔ l a a
    ## 36  8255739              Hele                           � e l e
    ## 37  8255741              Hele                           � e l e
    ## 38  8255759              Hnʼn                           � n ʔ n
    ## 39  8255781               Hnn                             � n n
    ## 40  8255784 manitosHermanitos m a n i t o s � e r m a n i t o s
    ## 41  8255820              Heli                           � e l i
    ## 42  8255822                Hi                               � i
    ## 43  8255825              Hele                           � e l e
    ## 44  8255858             Hales                         � a l e s
    ## 45  8255859             Halee                         � a l e e
    ## 46  8255860             Halee                         � a l e e
    ## 47  8255861             Halee                         � a l e e
    ## 48  8255864             Halee                         � a l e e
    ## 49  8255882                Ha                               � a
    ## 50  8255909               Hnn                             � n n
    ## 51  8255911              Hedó                         � e d o �
    ## 52  8255912                He                               � e
    ## 53  8255927              Hela                           � e l a
    ## 54  8256063              Hale                           � a l e
    ## 55  8256064               Hna                             � n a
    ## 56  8256077             Heʼlá                       � e ʔ l a �
    ## 57  8256083              Hele                           � e l e
    ## 58  8256092             Haʼan                         � a ʔ a n
    ## 59  8256134            Heʼdaʼ                       � e ʔ d a ʔ
    ## 60  8256168                He                               � e
    ## 61  8256221              Hnʼn                           � n ʔ n
    ## 62  8256225             Haach                        � a a t̠ʃ h
    ## 63  8256279                He                               � e
    ## 64  8256302                He                               � e
    ## 65  8256306              Hele                           � e l e
    ## 66  8256322              Heló                         � e l o �
    ## 67  8256362              Helo                           � e l o
    ## 68  8256379               Hnn                             � n n
    ## 69  8256408               Haa                             � a a
    ## 70  8256514              Hale                           � a l e
    ## 71  8256524              Hale                           � a l e
    ## 72  8256538                He                               � e
    ## 73  8256542                He                               � e
    ## 74  8256569            Heʼlaʼ                       � e ʔ l a ʔ
    ## 75  8256571              Hnnʼ                           � n n ʔ
    ## 76  8256595                He                               � e
    ## 77  8256654                He                               � e
    ## 78  8256658                He                               � e
    ## 79  8256665                He                               � e
    ## 80  8256700             Hobon                         � o b o n
    ## 81  8256727               Hnn                             � n n
    ## 82  8256744             Heeló                       � e e l o �
    ## 83  8256837               Hnn                             � n n
    ## 84  8256857              Helu                           � e l u
    ## 85  8256867              Heló                         � e l o �
    ## 86  8256887              Helá                         � e l a �
    ## 87  8256892               Hay                             � a j
    ## 88  8256910             Heloh                         � e l o h
    ## 89  8256953             Heloo                         � e l o o
    ## 90  8257004                Hé                             � e �
    ## 91  8257042                Ha                               � a
    ## 92  8257045                Ha                               � a
    ## 93  8257134             Hedaʼ                         � e d a ʔ
    ## 94  8257177               Hnn                             � n n
    ## 95  8257186               Hnn                             � n n
    ## 96  8257209              Hach                          � a t̠ʃ h
    ## 97  8257212             Hélee                       � e � l e e
    ## 98  8257287              Hiih                           � i i h
    ## 99  8257290              Hiih                           � i i h
    ## 100 8257297             Héloʼ                       � e � l o ʔ
    ## 101 8257342               Hnn                             � n n
    ## 102 8257360               Hnn                             � n n
    ## 103 8257391                Hi                               � i
    ## 104 8257428                He                               � e
    ## 105 8257430           Hwátlaʼ                   � w a � t l a ʔ
    ## 106 8257475            Helulá                     � e l u l a �
    ## 107 8257524              Hach                          � a t̠ʃ h
    ## 108 8257531             Hédaa                       � e � d a a
    ## 109 8257546                Ha                               � a
    ## 110 8257548                Ha                               � a
    ## 111 8257550                Ha                               � a
    ## 112 8257552                Ha                               � a
    ## 113 8257659                Ha                               � a
    ## 114 8257711             Helaʼ                         � e l a ʔ
    ## 115 8257745             Hélaʼ                       � e � l a ʔ
    ## 116 8257779             Heʼló                       � e ʔ l o �
    ## 117 8257814             Hoken                         � o k e n
    ## 118 8257830              Helu                           � e l u
    ## 119 8264453           Heʼelaʼ                     � e ʔ e l a ʔ
    ## 120 8266406            Heʼloʼ                       � e ʔ l o ʔ
    ## 121 8266431             Helaj                         � e l a x
    ## 122 8266437            Heʼlaʼ                       � e ʔ l a ʔ
    ## 123 8266465             Heloj                         � e l o x
    ## 124 8266939             Huebo                         � u e b o
    ## 125 8266941             Huebo                         � u e b o
    ## 126 8266943             Huevo                         � u e v o
    ## 127 8266949             Huebo                         � u e b o
    ## 128 8266952             Huebo                         � u e b o
    ## 129 8266954             Huebo                         � u e b o
    ## 130 8307823             Hasta                         � a s t a
    ## 131 8308214          Hastalaj                   � a s t a l a x
    ## 132 8308218             Hastu                         � a s t u
    ## 133 8308487             Hoxta                         � o ʃ t a
    ## 134 8309562               Hun                             � u n
    ## 135 8311662            Homble                       � o m b l e
    ## 136 8316003                Hm                               � m
    ## 137 8316022                Hm                               � m
    ## 138 8316051               Haa                             � a a
    ## 139 8316060           Huntaxe                     � u n t a ʃ e
    ## 140 8316077              Huʼu                           � u ʔ u
    ## 141 8316080            Hacheʼ                      � a t̠ʃ h e ʔ
    ## 142 8316084              Hach                          � a t̠ʃ h
    ## 143 8316089           Heʼeloʼ                     � e ʔ e l o ʔ
    ## 144 8316988          Hatsʼbil                   � a t s ʔ b i l
    ## 145 8317113               Hum                             � u m
    ## 146 8317127                Ha                               � a
    ## 147 8317135                Ha                               � a
    ## 148 8317238                Ha                               � a
    ## 149 8317400           Huan-oʼ                       � u a n o ʔ
    ## 150 8321610              Hola                           � o l a
    ## 151 8321614              Hola                           � o l a
    ## 152 8333836           Huan-eʼ                       � u a n e ʔ
    ## 153 8333837              Huan                           � u a n
    ## 154 8340253               Heʼ                             � e ʔ
    ## 155 8341753          Hospital                  � o s pʼ i t a l
    ## 156 8382440             Heʼla                         � e ʔ l a
    ## 157 8382789              Helu                           � e l u
    ## 158 8392232               Hey                             � e j
    ## 159 8398919              Heey                           � e e j
    ## 160 8399158             Heʼey                         � e ʔ e j
    ## 161 8400493              Heey                           � e e j
    ## 162 8400742               Hey                             � e j
    ## 163 8400922               Hey                             � e j
    ## 164 8404367              Heey                           � e e j
    ## 165 8427752             Heidi                         � e i d i
    ## 166 8427755             Heidi                         � e i d i
    ## 167 8427764          Heidi-eʼ                     � e i d i e ʔ
