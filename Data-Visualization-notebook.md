Data Visualization
================
Sean O’Neal
8/21/2020

# Data Visualization

### 3.1 Introduction

``` r
# we must load the tidyverse library every session

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.1     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

### 3.2 First Steps

QUESTION: Do cars with big engines use more gas than cars with small
engines?

##### 3.2.1 the mpg data frame

A data frame is a rectangular collection of variables (in the columns)
and observations (in the rows). mpg contains observations collected by
the US Environmental Protection Agency on 38 models of car.

``` r
mpg
```

    ## # A tibble: 234 x 11
    ##    manufacturer model    displ  year   cyl trans   drv     cty   hwy fl    class
    ##    <chr>        <chr>    <dbl> <int> <int> <chr>   <chr> <int> <int> <chr> <chr>
    ##  1 audi         a4         1.8  1999     4 auto(l… f        18    29 p     comp…
    ##  2 audi         a4         1.8  1999     4 manual… f        21    29 p     comp…
    ##  3 audi         a4         2    2008     4 manual… f        20    31 p     comp…
    ##  4 audi         a4         2    2008     4 auto(a… f        21    30 p     comp…
    ##  5 audi         a4         2.8  1999     6 auto(l… f        16    26 p     comp…
    ##  6 audi         a4         2.8  1999     6 manual… f        18    26 p     comp…
    ##  7 audi         a4         3.1  2008     6 auto(a… f        18    27 p     comp…
    ##  8 audi         a4 quat…   1.8  1999     4 manual… 4        18    26 p     comp…
    ##  9 audi         a4 quat…   1.8  1999     4 auto(l… 4        16    25 p     comp…
    ## 10 audi         a4 quat…   2    2008     4 manual… 4        20    28 p     comp…
    ## # … with 224 more rows

##### 3.2.2 Creating a ggplot

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
\#\#\#\#\# Exercises

> 1.  Run ggplot(data = mpg). What do you see?

``` r
ggplot(data = mpg)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
I see a large blank grey slate

> 2.How many rows are in mpg? How many columns?

There are 234 rows and 11 columns

> 3.  What does the drv variable describe? Read the help for ?mpg to
>     find out.

The drv variable describes the drive-train of the car, whether it’s a
front-wheel drive, rear-wheel drive, or all-wheel drive car.

> 4.  Make a scatterplot of hwy vs cyl.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

> 5.  What happens if you make a scatterplot of class vs drv? Why is the
>     plot not useful?

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
This plot is not useful because the class of car is not always dependent
on the drive train and the variables are categorical.

### Aesthetic mappings

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))
```

    ## Warning: Using size for a discrete variable is not advised.

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))
```

    ## Warning: The shape palette can deal with a maximum of 6 discrete values because
    ## more than 6 becomes difficult to discriminate; you have 7. Consider
    ## specifying shapes manually if you must have them.

    ## Warning: Removed 62 rows containing missing values (geom_point).

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(data = mpg) +geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

##### 3.3.1 Exercises

> 1.  What’s gone wrong with this code? Why are the points not blue?

This code is incorrect because the “color =”blue"" is supposed to be
inside of the second group of parentheses.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

> 2.  Which variables in mpg are categorical? Which variables are
>     continuous?

The categorical variables are manufacturer, model, trans, drv,fl, and
class. The continuous variables are displ, year, cyl, cty, and hwy.

``` r
?mpg
```

> 3.  Map a continuous variable to color, size, and shape. How do these
>     aesthetics behave differently for categorical vs. continuous
>     variables?

For colour, continuous variables change shades of the same colour while
categorical changes the entire colour. For size, the variables react in
the same way but there is more data for the continuous variables than
the categorical. For shape, continuous variables cannot be be mapped to
shape while categorical variables can.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = drv))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

> 4.  What happens if you map the same variable to multiple aesthetics?

When you map the same variable to multiple aesthetics the variable that
is already mapped is mapped again but as an aesthetic.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = hwy))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

> 6.  What happens if you map an aesthetic to something other than a
>     variable name, like aes(colour = displ \< 5)?

When you map something other than a variable name, the graph takes the
data and makes it conform to the specification that you typed. For this
example, all displacements larger than 5 litres are coloured red and all
displacements smaller than 5 litres are coloured blue.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### 3.5 Facets

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

> 1.  What happens if you facet on a continuous variable?

It turns the continuous variable into categories for the graphs.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ cyl, nrow = 2)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

> 2.  What do the empty cells in plot with facet\_grid(drv \~ cyl) mean?
>     How do they relate to this plot?

You see that 5 cylinder 4 wheel drive and rear wheel drive cars, as well
as 4 cylinder rear wheel drive cars are very rare and/or nonexistent.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

> 3.  What plots does the following code make? What does . do?

The “.” on the left side organized the data into columns while the “.”
on the right side organizes the data into rows.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ .)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

> 4.  What are the advantages to using faceting instead of the colour
>     aesthetic? What are the disadvantages? How might the balance
>     change if you had a larger dataset?

The advantages of using faceting are that it organizes the variables
into smaller separate graphs based on the criteria you define. The
balance would change with a larger data set because it is easier to
discern the data in facets as opposed to multiple colours that may look
similar.

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

> 5.  Read ?facet\_wrap. What does nrow do? What does ncol do? What
>     other options control the layout of the individual panels? Why
>     doesn’t facet\_grid() have nrow and ncol arguments?

nrow and ncol changes the number of rows and columns. The other options
are scales, shrink, labeller, as.table, switch, drop, dir, and
strip.position. Facet\_grid doesnt have nrow and ncol arguments because
they don’t apply to that type of command.

``` r
?facet_wrap
```

> 6.  When using facet\_grid() you should usually put the variable with
>     more unique levels in the columns. Why?

You should put the variable with more unique levels in the columns
because the facet\_grid command has an easier time organizing and
graphing larger variables in columns

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(cyl ~ cty)
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### 3.6 Geometric objects

``` r
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
ggplot(data = mpg) +geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
ggplot(data = mpg) +geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth() + geom_point(mapping = aes(color = class))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

## Excercises 1-5

> 1.  What geom would you use to draw a line chart? A boxplot? A
>     histogram? An area chart?

To draw a line chart, you would use geom\_abline. To make a box plot,
you would use geom\_boxplot, to make a histogram, you would use
geom\_histogram, and to make an area chart you would use geom\_area.

> 2.  Run this code in your head and predict what the output will look
>     like. Then, run the code in R and check your predictions.

I predict that this will look like a normal chart with both the line and
the points coloured by drv, but the se takes out the standard error.
This was correct.

``` r
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + geom_point() + geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

> 3.  What does show.legend = FALSE do? What happens if you remove it?
>     Why do you think I used it earlier in the chapter?

Show.legend = false drops the legend from the graph. When you remove it,
the legend appears again. You used this earlier in the chapter in order
for the graphs to not get too complicated.

``` r
ggplot(data = mpg) +geom_point(mapping = aes(x = displ, y = hwy, show.legend = FALSE))
```

    ## Warning: Ignoring unknown aesthetics: show.legend

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

> 4.  What does the se argument to geom\_smooth() do?

The se argument computes the standard error of the mean.

> 5.  Will these two graphs look different? Why/why not?

No they will not because it is the same exact data and commands but they
are spread out over the different chunks of code.

### 3.7 Statistical transformations

``` r
diamonds
```

    ## # A tibble: 53,940 x 10
    ##    carat cut       color clarity depth table price     x     y     z
    ##    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
    ##  1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
    ##  2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
    ##  3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31
    ##  4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
    ##  5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75
    ##  6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
    ##  7 0.24  Very Good I     VVS1     62.3    57   336  3.95  3.98  2.47
    ##  8 0.26  Very Good H     SI1      61.9    55   337  4.07  4.11  2.53
    ##  9 0.22  Fair      E     VS2      65.1    61   337  3.87  3.78  2.49
    ## 10 0.23  Very Good H     VS1      59.4    61   338  4     4.05  2.39
    ## # … with 53,930 more rows

``` r
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

### 3.8 Position adjustments

``` r
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, color = cut))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(alpha = 0.2, position = "identity")
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
ggplot( data = diamonds, mapping = aes(x = cut, color = clarity)) + geom_bar(fill = NA, position = "identity")
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(position = "fill")
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(position = "dodge")
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

##### 3.8.1 Exercises

Notebook check - exercises and notes

Project - start next week / due the week after

> 1.  The problem with this plot is that the points will be
>     automatically calculated into averages, so you could improve it by
>     adding the “jitter” adjustment to add some randomness to the graph
>     in order for it to be more realistic.

> 2.  The parameters that controls the amount of jittering are width and
>     height

``` r
?geom_jitter
```

> 3.  Geom\_jitter makes the points move around a little more so that
>     you can see all of the various points if they are overlapping.
>     Geom\_count counts the number of overlapping points in one area
>     and maps the count to point area.

``` r
?geom_count
```

> 4.  

``` r
ggplot(data = mpg, mapping = aes(x = displ)) + geom_boxplot()
```

![](Data-Visualization-notebook_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->
