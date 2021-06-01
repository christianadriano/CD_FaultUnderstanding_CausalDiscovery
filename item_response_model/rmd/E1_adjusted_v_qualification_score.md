Compare Adjusted and Original Qualification Score - Experiment-1
================

## About this report

1- How adjusted\_score and qualification\_score are distributed 2- How
many people increased their score relative to their peers? 3- How many
people decreased? 4- Who were these people who increased and decreased
(distribution w.r.t. original score)?

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## \#—————————————————————-

# 1- How adjusted\_score and qualification\_score are distributed

![](E1_adjusted_v_qualification_score_files/figure-gfm/density%20plots-1.png)<!-- -->

The chart shows that the adjusted score smoothed the distribution, but
still preserved the two general patterns of concentration on high and
low medium-to-low scores.

However, it also increased the frequency of the lowest score and highest
score groups. The final distribution has more oa an exponential shape,
while the original one had a right skewed Gaussian shape.

The reason for the smoothing is two-fold: adjusted score is continuous
scale and the original shifted some very low scores to a low-to-medium
score. The latter corresponds to giving a lower weight to questions that
most people got it correctly This was the case of question 4.
