Expert Choice
================
Jed Stephens
11 May 2019

<!-- README.md is generated from README.Rmd. Please edit README.Rmd to make changes. -->

The purpose of this example is to present a practical worked example of
how to design a conjoint and discrete choice experiment without
blocking. This tutorial accompanies the note: Designing conjoint scaling
and discrete choice experiments for small sample expert surveys by Jed
Stephens which may give some more theoretical insight.

# Step 0: Decide on what to test

The following table gives the attributes and thier associated levels for
reserach involving antique silver experts. Although you do not need to
create a table of this format in order to use the software it is
beneficial to have an idea of what you wish to test and what would be
suitable levels upon which to test it. The process of choosing a design
often involes interating over steps 0 to 4. Some designs are more
difficult to create than others.

Here are some practical suggestions as to what makes a good design. 1.
In general avoid attributes with only two levels. The design such as the
one below suffers because it is difficult to convert from the fractional
fractorial of this design into an effient choice experiment design. The
lack of efficiency is not from the methods of converting, but inherit in
the fact that achieving the minimal overlap when there is only two
levels is diffciult.

| z |  attribute name (z)  | z\_l |            level name (z\_l)             |          Description          |
| :-: | :------------------: | :--: | :--------------------------------------: | :---------------------------: |
| 1 |    Makers Renown     |  1   |               little known               |     Bottom 50% of makers      |
| 1 |    Makers Renown     |  2   |           known to specialists           |       Middle 50% to 75%       |
| 1 |    Makers Renown     |  3   |                recognised                |          75% to 90%           |
| 1 |    Makers Renown     |  4   |                  famous                  |            top 10%            |
| 2 | Technical Perfection |  1   |              below average               |  Bottom 50% of craftsmanship  |
| 2 | Technical Perfection |  2   |                   good                   |       Middle 50% to 75%       |
| 2 | Technical Perfection |  3   |               meritorious                |          75% to 90%           |
| 2 | Technical Perfection |  4   |               exceptional                |            top 10%            |
| 3 |   Category Rarity    |  1   |                  common                  | bottom 25% of category rarity |
| 3 |   Category Rarity    |  2   |                 uncommon                 |          25% to 50%           |
| 3 |   Category Rarity    |  3   |                   rare                   |          50% to 75%           |
| 3 |   Category Rarity    |  4   |                very rare                 |            top 25%            |
| 4 |   Size (of object)   |  1   |                  small                   |          under 150g           |
| 4 |   Size (of object)   |  2   |                  medium                  |     between 151g and 400g     |
| 4 |   Size (of object)   |  3   |                  large                   |    between 401g and 1000g     |
| 4 |   Size (of object)   |  4   |               extra large                |       larger than 1000g       |
| 5 |   Age (of object)    |  1   |           21st or 20th Century           |     Years 1900 to present     |
| 5 |   Age (of object)    |  2   |               19th Century               |      Years 1800 to 1899       |
| 5 |   Age (of object)    |  3   |               18th Century               |      Years 1700 to 1799       |
| 5 |   Age (of object)    |  4   |           Before 18th Century            |          Before 1700          |
| 6 |      Provenance      |  1   | unavailable or available but unimportant |                               |
| 6 |      Provenance      |  2   |          available & important           |                               |

# Step 1: Construct the full factorial

First load the the `ExpertDesign` package into your R environemnent.

``` r
library(ExpertChoice)
```

Create a list object which specifies the name of the variables as well
as thier respective levels.  
**NB: The levels should be integer sequential and start from 1** As the
proposed design above is a \(4^4 2^1\) I have chosen to denote the
object as `attr4521`:

``` r
attri4521  = list(maker = c("1", "2", "3", "4"),
                  technical =c("1", "2", "3", "4"),
                  category_rarity = c("1", "2", "3", "4"),
                  size = c("1", "2", "3", "4"),
                  age = c("1", "2", "3", "4"),
                  provenance = c("1", "2"))
```

Calling the list object something like this is advantagous because you
could have multiple competing designs still at this stage. For example
`attri4531`:

``` r
attri4531  = list(maker = c("1", "2", "3", "4"),
                  technical =c("1", "2", "3", "4"),
                  category_rarity = c("1", "2", "3", "4"),
                  size = c("1", "2", "3", "4"),
                  age = c("1", "2", "3", "4"),
                  provenance = c("1", "2", "3"))
```

Create the full factorial object. Using the design specification as a
suffix remains a handy way of keeping track of the design.

``` r
ff4521 <- full_factorial(attri4521)
```

The full factorial will contain many rows. The first five rows and the
last five are given below:

``` r
rbind(head(ff4521, 5), tail(ff4521, 5))
##      maker technical category_rarity size age provenance
## 1        1         1               1    1   1          1
## 2        2         1               1    1   1          1
## 3        3         1               1    1   1          1
## 4        4         1               1    1   1          1
## 5        1         2               1    1   1          1
## 2044     4         3               4    4   4          2
## 2045     1         4               4    4   4          2
## 2046     2         4               4    4   4          2
## 2047     3         4               4    4   4          2
## 2048     4         4               4    4   4          2
```

For every variable in the full factorial has the standaridised
orthogonal contrast applied. These constrasts are very useful when
eveluating the efficacy of a design. This is simply illustrative:

``` r
contrasts(ff4521$maker)
##        [,1]       [,2]       [,3]
## 1  1.414214 -0.8164966 -0.5773503
## 2  0.000000  1.6329932 -0.5773503
## 3  0.000000  0.0000000  1.7320508
## 4 -1.414214 -0.8164966 -0.5773503
```

# Step 2: Augment the full factorial

Once the full facotrial is constructed it is possible to augment it some
additional information. Many of these augmentations happen as
attributes. This includes adding the B-matrix (for main effects) as
described by Street et al… The prefix `af` is used to refer to the
augmented (full) factorial. (You could of course name the object
whatever you prefer.)

``` r
aff4521 <- augment_levels(ff4521)
## [1] "Applying B mat"
```

A console log will appear stating that the processes of applying the
B-matrix has started. If you do not get this message then the B-matrix
cannot be added. (Please open a GitHub issue if this is the case. I am
not aware of instances where this should happen.) The B-matrix plays an
important role in the choice efficiency of design. Below are ten random
rows drawn from the agumented full factorial. Notice the additional of
the `levels` column.

``` r
aff4521[sample(nrow(aff4521), 10), ]
##      maker technical category_rarity size age provenance levels
## 1962     2         3               3    3   4          2 233342
## 278      2         2               2    1   2          1 222121
## 558      2         4               3    1   3          1 243131
## 629      1         2               4    2   3          1 124231
## 500      4         1               4    4   2          1 414421
## 433      1         1               4    3   2          1 114321
## 655      3         4               1    3   3          1 341331
## 1207     3         2               4    3   1          2 324312
## 249      1         3               4    4   1          1 134411
## 1823     3         4               2    1   4          2 342142
```

# Step 3: Creating a fractional factorial design.

``` r
library(AlgDesign)
library(DoE.base)
## Loading required package: grid
## Loading required package: conf.design
## Registered S3 method overwritten by 'DoE.base':
##   method           from       
##   factorize.factor conf.design
## 
## Attaching package: 'DoE.base'
## The following objects are masked from 'package:stats':
## 
##     aov, lm
## The following object is masked from 'package:graphics':
## 
##     plot.design
## The following object is masked from 'package:base':
## 
##     lengths
library(DoE.MIParray)
## Registered S3 method overwritten by 'DoE.MIParray':
##   method   from    
##   print.oa DoE.base
```

There are many ways to create a fractional factorial design. See Section
… of the associated note Designing conjoint scaling and discrete choice
experiments for small sample expert surveys by Jed Stephens for a full
discussion. Practically speaking though two methods are designed to be
flawlessly integreated into this package. These are the construction of
a fractional factorial design using an orthogonal array with either the
`DoE.MIParray` or `DoE.base` packages or using D-optimal fractional
factorial designs from the `AlgDesign` package.

## Ortogonal Arrays (`DoE.MIParray` or `DoE.base`)

The function `oa_feasible()` from the `DoE.base` package
(`DoE.base::oa_feasible()`) provides many methods for determining if a
particular design can be construed with \(N_D\) rows. For the silver
expert it was found that the following design was feasiable. It is
possible to specifiy higher resolution designs. These are always
advantageous. See Section … of the associated note Designing conjoint
scaling and discrete choice experiments for small sample expert surveys
by Jed Stephens for a full discussion.

``` r
# Design: DF: 17, 32 OA (Resolution II), 64 OA (Resolution III)
oa_feasible(32, c(4,4,4,4,4,2), strength = 2)
## no violation of necessary criteria  for strength  2  was found
## [1] TRUE
```

Using the `DoE.base` package it is possible to construct a 32 .

When consturcting a design using the `DoE.MIParray`… The function
`mosek_MIParray()` was used to construct the example 64 run orthogonal
array included with this package.

``` r
# Not run because it requires time as well as some setting up if this is your first time.
# See DoE.MIParray for more detials.
# fractional_factorial_4521_64 <- mosek_MIParray(64, c(4,4,4,4,4,2), maxtime = 54000)
```

We can instead load this object from the package. It is called
`silver_4521_64`. The data is loaded into the enviroment and then the
first ten rows are presented.

``` r
silver_4521_64 <- ExpertChoice::silver_4521_64
head(silver_4521_64, 10)
##       [,1] [,2] [,3] [,4] [,5] [,6]
##  [1,]    1    1    1    1    1    1
##  [2,]    1    1    2    4    3    2
##  [3,]    1    1    3    3    2    2
##  [4,]    1    1    4    2    4    1
##  [5,]    1    2    1    4    4    2
##  [6,]    1    2    2    1    2    1
##  [7,]    1    2    3    2    3    1
##  [8,]    1    2    4    3    1    2
##  [9,]    1    3    1    2    2    2
## [10,]    1    3    2    3    4    1
```

The results of the `mosek_MIParray` function are orthogonal arrays
without colnames. Hence in this instance the colnames need to be added.
This design clearly needed to be made with the full factiorial in mind.
Hence the colnames from the `ff4521` object are appropriate. **Note: the
colnames from the aff4521 would include the levels column – hence avoid
these…**

``` r
colnames(silver_4521_64)<- colnames(ff4521)
fractional_f4521_64 <- search_design(ff4521, silver_4521_64)
```

The result is a fracitonal fractional design. Importantly though the
fractional factorial design retains and inherits information from the
full factorial such as the standarised orthgonal coding. To mark that
many such attributes are held a special attribute is assigned to the
object.

``` r
# Check to see if the searched attribute exists on the fractional_f4521_64 object.
attributes(fractional_f4521_64)$searched
## [1] TRUE
```

Once an object is search converted it is now easy to run diagonsitcs.

# Step Five

The theoertical discussion of these diagonstics is presented extensively
in the associated note Designing conjoint scaling and discrete choice
experiments for small sample expert surveys by Jed Stephens. See
Section…

The generalised world length patterns gives a good overall summary of
the design.

``` r
DoE.base::GWLP(fractional_f4521_64)
##  0  1  2  3  4  5  6 
##  1  0  0  0 25  0  6
```

From this we can tell that this fractional factorial design is
resolution IV i.e. strength of 3. Hence the all main effects are
estimable free of each other, but some are confounded with two-attribute
interactions.

The function `fractional_factorial_efficiency` provides a formula based
method of investigating the proposed fractional factorial design in more
detials. This function also includes in its list of results the GWLP so
there is no need to specifiy it.

Two examples are given which follow the two examples in the associated
note.

``` r
# Test for main effects
main_effects <- fractional_factorial_efficiency(~ maker + technical + category_rarity + size + age + provenance, fractional_f4521_64)
## Your fractional factorial design has an A-efficiency of 100 
##  Your fractional factorial design has a D-efficiency of 100
```

### Determine feasiability

## D-efficient

Not yet discussed or though it should be achieveable with mininal
effort. If a reader wishes for this example to be completed before I
have done so please open a GitHub issue and I shall happily oblige
completing.

# Step 4: Searching the full factorial for the chosen fractional factorial design

The ability to use multiple different packages to construct the
fractional factorial design is ensured by this step. There can exist
small differences between the different methods which require some
fiddiling.
