Lab 06 - Ugly charts and Simpson’s paradox
================
Cailey Fay
2/15/26

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
staff <- read_csv("data/instructional-staff.csv")
```

I think when we convert to long-form data, we will have 6 columns. If
there are five different faculty types and 11 years, I think that means
we will have 5x11 = 55 rows?

To get the data long:

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

This confirms my thought that there would be 55!

To try to plot this as a line graph with years on the x, the percentage
of hires on the y, and 5 lines for the diferent faculty types:

``` r
staff_long %>%
  ggplot(aes(x = year, y = value, color = faculty_type)) +
  geom_line()
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](lab-06_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> This doesn’t
quite work.

### Exercise 1

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line() +
  labs(title = "Percentage of Faculty Types Hired by Year",
       x= "Year",
       y= "Percentage of Hires",
       color = "Faculty Type")
```

![](lab-06_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Exercise 2

Suppose the objective of this plot was to show that the proportion of
part-time faculty have gone up over time compared to other instructional
staff types. What changes would you propose making to this plot to tell
this story?

I would make all of the lines grey except the part time faculty. I think
this pretty clearly communicates that the percentage of part time
faculty hires is increasing.

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line() +
  labs(title = "Percentage of Faculty Types Hired by Year",
       x= "Year",
       y= "Percentage of Hires",
       color = "Faculty Type") +
    scale_color_manual(values = c(
    "Full-Time Tenured Faculty" = "grey",
    "Full-Time Tenure-Track Faculty" = "grey",
    "Full-Time Non-Tenure-Track Faculty" = "gray",
    "Part-Time Faculty" = "red",
    "Graduate Student Employees" = "grey"
  )) 
```

![](lab-06_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Exercise 3

…

Add exercise headings as needed.
