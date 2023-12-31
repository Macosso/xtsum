<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/xtsum)](https://CRAN.R-project.org/package=xtsum) [![R-CMD-check](https://github.com/Macosso/xtsum/workflows/R-CMD-check/badge.svg)](https://github.com/Macosso/xtsum/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/xtsum)](https://www.r-pkg.org/pkg/xtsum) 
<!-- need [![Launch binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Macosso/xtsum/master)  -->

<!-- badges: end -->

================

# Introduction

`xtsum` is an R wrapper based on `STATA` `xtsum` command, it used to
provide summary statistics for a panel data set. It decomposes the
variable $x_{it}$ into a between $(\bar{x_i})$ and within
$(x_{it} − \bar{x_i} + \bar{\bar{x}})$, the global mean $\bar{\bar{x}}$ being added
back in make results comparable (StataCorp 2023).

# Installation

``` r
install.packages("xtsum")

# For dev version
# install.packages("devtools")
devtools::install_github("macosso/xtsum")
```

# Getting Started

``` r
# Load the librarry
library(xtsum)
```

## xtsum

This function computes summary statistics for panel data, including
overall statistics, between-group statistics, and within-group
statistics.

**Usage**

    xtsum(
      data,
      variables = NULL,
      id = NULL,
      t = NULL,
      na.rm = FALSE,
      return.data.frame = TRUE,
      dec = 3
    )

**Arguments**

- `data` A data.frame or pdata.frame object representing panel data.

- `variables` (Optional) Vector of variable names for which to calculate
  statistics. If not provided, all numeric variables in the data will be
  used.

- `id` (Optional) Name of the individual identifier variable.

- `t` (Optional) Name of the time identifier variable.

- `na.rm` Logical indicating whether to remove NAs when calculating
  statistics.

- `return.data.frame` If the return object should be a dataframe

- `dec` Number of significant digits to report

### Example

#### Genral example

Based on National Longitudinal Survey of Young Women, 14-24 years old in
1968

``` r
data("nlswork", package = "sampleSelection")
xtsum(nlswork, "hours", id = "idcode", t = "year", na.rm = T, dec = 6)
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:left;">
Dim
</th>
<th style="text-align:left;">
Mean
</th>
<th style="text-align:left;">
SD
</th>
<th style="text-align:left;">
Min
</th>
<th style="text-align:left;">
Max
</th>
<th style="text-align:left;">
Observations
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
___________
</td>
<td style="text-align:left;">
_________
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
hours
</td>
<td style="text-align:left;">
overall
</td>
<td style="text-align:left;">
36.55956
</td>
<td style="text-align:left;">
9.869623
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
168
</td>
<td style="text-align:left;">
N = 28467
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
between
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
7.846585
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
83.5
</td>
<td style="text-align:left;">
n = 4710
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
within
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
7.520712
</td>
<td style="text-align:left;">
-2.154726
</td>
<td style="text-align:left;">
130.05956
</td>
<td style="text-align:left;">
T = 6.043949
</td>
</tr>
</tbody>
</table>

The table above can be interpreted as below paraphrased from (StataCorp
2023).

The overall and within are calculated over `N = 28,467` person-years of
data. The between is calculated over `n = 4,710` persons, and the
average number of years a person was observed in the hours data
is`T = 6`.

`xtsum` also reports standard deviation(`SD`), minimums(`Min`), and
maximums(`Max`).

Hours worked varied between `Overal Min = 1` and `Overall Max = 168`.
Average hours worked for each woman varied between `between Min = 1` and
`between Max = 83.5`. “Hours worked within” varied between
`within Min = −2.15` and `within Max = 130.1`, which is not to say that
any woman actually worked negative hours. The within number refers to
the deviation from each individual’s average, and naturally, some of
those deviations must be negative. Then the negative value is not
disturbing but the positive value is. Did some woman really deviate from
her average by +130.1 hours? No. In our definition of within, we add
back in the global average of 36.6 hours. Some woman did deviate from
her average by 130.1 − 36.6 = 93.5 hours, which is still large.

The reported standard deviations tell us that the variation in hours
worked last week across women is nearly equal to that observed within a
woman over time. That is, if you were to draw two women randomly from
our data, the difference in hours worked is expected to be nearly equal
to the difference for the same woman in two randomly selected years.


you can also get each stat seperately

```
between_sd(nlswork, "hours", id = "idcode", t = "year", na.rm = T)
7.846585
```

```
between_max(nlswork, "hours", id = "idcode", t = "year", na.rm = T)
83.5
```

```
between_min(nlswork, "hours", id = "idcode", t = "year", na.rm = T)
1
```

```
within_sd(nlswork, "hours", id = "idcode", t = "year", na.rm = T)
7.520712
```

```
within_max(nlswork, "hours", id = "idcode", t = "year", na.rm = T)
130.0596
```

```
within_min(nlswork, "hours", id = "idcode", t = "year", na.rm = T)
-2.154726
```

More detailed interpretation can be found in handout(Porter n.d.)

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-stephenporter" class="csl-entry">

Porter, Stephen. n.d. *Understanding Xtsum Output*. stephenporter.org.
Accessed December 6, 2023.
<https://stephenporter.org/files/xtsum_handout.pdf>.

</div>

<div id="ref-stata" class="csl-entry">

StataCorp. 2023. “STATA LONGITUDINALDATA/PANELDATA REFERENCEMANUAL
RELEASE 18.” *A Stata Press Publication*.

</div>

</div>
