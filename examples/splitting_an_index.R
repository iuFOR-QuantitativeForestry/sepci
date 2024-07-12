library(siplab)
library(sepci)

# A kernel function is a function that computes the influence of trees j over
# tree i in a vectorialized way. The variables of the function have to be
# imarks, jmarks, dists, dranks, par.

# We are going to use the one called CI_1 in the appendix of
# Rouvinen, S. and Kuuluvainen, T. (1997) ‘Structure and asymmetry of tree crowns in
# relation to local competition in a natural mature Scots pine forest’, Canadian Journal
# of Forest Research, 27(6), pp. 890–902. Available at: https://doi.org/10.1139/x97-012.

RK1 <- function(imarks, jmarks, dists, dranks, par= list(smark=1)){
  smark = par$smark
  atan(jmarks[smark]/dists)
}


# We want to split the index in intra and inter species competition.

# We are going to use 3 nearest neighbors competition.
# One behaviour of siplab pairwise function is that you can combine number of
# nearest neighbors and fixed radius with a selection kernel. First the nearest
# neighbors or the fixed radius is applied and then the kernel is applied.

# We create two selection kernels. The kernels are functions that
# return a boolean vector of length where he number of trees where true
# indicates the competitors. The variables of the kernel have to be
# imarks, jmarks, dists, dranks, par. Where
# imarks are the variables of the subject tree
# jmarks are the variables of the possible competitor trees
# dists are the distances between the subject tree and the possible competitor trees
# dranks are the ranks of the distances
# par is a list with any extra parameter you might need

same_species_sel <- function(imarks, jmarks, dists, dranks, par = list(speciesmark = 1)) {
  speciesmark = par$speciesmark
  imarks[[speciesmark]] == jmarks[speciesmark]
  # We could use here (or before) a second selection function if we want to combine the behavior
  # with other selection method
  # e.g:
  # sel1 <- imarks[[speciesmark]] == jmarks[speciesmark]
  # return(powlinear_sel(imarks, jmarks[sel1], dists, dranks, par = list(ki = 0.2, kj = 0, p = 1, r0 = 0, smark=2)))
}

different_species_sel <- function(imarks, jmarks, dists, dranks, par = list(speciesmark = 1)) {
  speciesmark = par$speciesmark
  imarks[[speciesmark]] != jmarks[speciesmark]
}

# We compute each part separately

results_1 <- pairwise(valdepoza, maxN=3, select = same_species_sel,
                      selpar = list(speciesmark = 'species'),
                      kernel = RK1,
                      kerpar = list(smark='dbh'))

results_2 <- pairwise(valdepoza, maxN=3, select = different_species_sel,
                      selpar = list(speciesmark = 'species'),
                      kernel = RK1,
                      kerpar = list(smark='dbh'))

# And together

results_3 <- pairwise(valdepoza, maxN = 3, kernel = RK1,
                    kerpar = list(smark='dbh'))

# We can check that the third index should be the sum of the other two
# We use all.equal for the comparison to avoid numerical problems
all.equal(results_1$marks$cindex + results_2$marks$cindex, results_3$marks$cindex)
# [1] TRUE

# We want to split the index in effect due to bigger competitors and
# effects of smaller (or equal) competitors

# We are going to use 3 nearest neighbors competition.
# One behaviour of siplab pairwise function is that you can combine number of
# nearest neighbors and fixed radius with a selection kernel. First the nearest
# neighbors or the fixed radius is applied and then the kernel is applied.

# We create two selection kernels. The kernels are functions that
# return a boolean vector of length where he number of trees where true
# indicates the competitors. The variables of the kernel have to be
# imarks, jmarks, dists, dranks, par. Where
# imarks are the variables of the subject tree
# jmarks are the variables of the possible competitor trees
# dists are the distances between the subject tree and the possible competitor trees
# dranks are the ranks of the distances
# par is a list with any extra parameter you might need

bigger_ind_sel <- function(imarks, jmarks, dists, dranks, par = list(smark = 1)) {
  smark = par$smark
  imarks[[smark]] < jmarks[smark]
  # We could use here (or before) a second selection function if we want to combine the behavior
  # with other selection method
  # e.g:
  # sel1 <- imarks[[speciesmark]] == jmarks[speciesmark]
  # return(powlinear_sel(imarks, jmarks[sel1], dists, dranks, par = list(ki = 0.2, kj = 0, p = 1, r0 = 0, smark=2)))
}

smaller_ind_sel <- function(imarks, jmarks, dists, dranks, par = list(smark = 1)) {
  smark = par$smark
  imarks[[smark]] >= jmarks[smark]
}

# We compute each part separately

results_1 <- pairwise(valdepoza, maxN=3, select = bigger_ind_sel,
                      selpar = list(smark = 'height'),
                      kernel = RK1,
                      kerpar = list(smark='dbh'))

results_2 <- pairwise(valdepoza, maxN=3, select = smaller_ind_sel,
                      selpar = list(smark = 'height'),
                      kernel = RK1,
                      kerpar = list(smark='dbh'))

# And together

results_3 <- pairwise(valdepoza, maxN = 3, kernel = RK1,
                      kerpar = list(smark='dbh'))

# We can check that the third index should be the sum of the other two
# We use all.equal for the comparison to avoid numerical problems
all.equal(results_1$marks$cindex + results_2$marks$cindex, results_3$marks$cindex)
# [1] TRUE











