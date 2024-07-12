library(siplab)
library(sepci)

# We are going to see how to add a new index
# We are going to implement the one called CI_1 in the appendix of
# Rouvinen, S. and Kuuluvainen, T. (1997) ‘Structure and asymmetry of tree crowns in
# relation to local competition in a natural mature Scots pine forest’, Canadian Journal
# of Forest Research, 27(6), pp. 890–902. Available at: https://doi.org/10.1139/x97-012.

# A kernel function is a function that computes the influence of trees j over
# tree i in a vectorialized way. The variables of the function have to be
# imarks, jmarks, dists, dranks, par. Where
# imarks are the variables of the subject tree
# jmarks are the variables of the competitor trees
# dists are the distances between the subject tree and the competitor trees
# dranks are the ranks of the distances
# par is a list with any extra parameter you might need



RK1 <- function(imarks, jmarks, dists, dranks, par= list(smark=1)){
  smark = par$smark
  atan(jmarks[smark]/dists)
}

# We are going to use the data from sepci. The data needs to be in spatstat
# ppp format.
valdepoza
# The data of the trees is in the marks part of the ppp object
head(valdepoza$marks)
# # A tibble: 6 × 6
# dbh height heightStartCrown heightLargestCrownRadius largestCrownRadius species
# <dbl>  <dbl>            <dbl>                    <dbl>              <dbl> <chr>
#   1   6.9    3.5              2.9                     3.01              0.844 Quercus pyrenaica
# 2  45.3   21.3             11                      18.2               0.415 Pinus sylvestris
# 3  29.8   21.1             12.7                    17.5               0.286 Pinus sylvestris
# 4  30.7   20.3             14.4                    15.4               0.325 Pinus sylvestris
# 5  27.6   20.8             14.8                    20.5               0.276 Pinus sylvestris
# 6  26.4   19.8             14.6                    19.5               0.300 Pinus sylvestris
results <- pairwise(valdepoza, maxN = 3, kernel = RK1,
                    kerpar = list(smark='dbh'))

head(results$marks)
# dbh height heightStartCrown heightLargestCrownRadius largestCrownRadius           species   cindex
# 1  6.90    3.5              2.9                 3.010929          0.8437508 Quercus pyrenaica 3.485354
# 2 45.30   21.3             11.0                18.234453          0.4152717  Pinus sylvestris 3.864969
# 3 29.85   21.1             12.7                17.515941          0.2862109  Pinus sylvestris 4.375941
# 4 30.70   20.3             14.4                15.391506          0.3247555  Pinus sylvestris 4.307696
# 5 27.65   20.8             14.8                20.463036          0.2762191  Pinus sylvestris 4.440827
# 6 26.45   19.8             14.6                19.506070          0.2999157  Pinus sylvestris 4.320218
