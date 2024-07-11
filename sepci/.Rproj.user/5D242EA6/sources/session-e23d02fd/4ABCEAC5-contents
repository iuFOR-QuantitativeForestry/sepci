
# Auxiliary functions
intersection_of_circles <- function(d, r1, r2) {
  ans <- c()
  for (i in seq_along(d)) {
    if (d[i] > r1 + r2[i]) {
      # Circles don't intersect
      ans <- c(ans, 0)
    } else if (d[i] <= (r1 - r2[i]) && r1 >= r2[i]) {
      # Circle 2 inside circle 1
      ans <- c(ans, pi * r2[i]^2)
    } else if (d[i] <= (r2[i] - r1) && r2[i] >= r1) {
      # Circle 1 inside circle 2
      ans <- c(ans, pi * r1^2)
    } else {
      # Circles intersect
      lengthToCord <-  (r1^2 - r2[i]^2 + d[i]^2) / (2 * d[i])
      lengthToOtherCord <- d[i] -lengthToCord

      alpha <- acos(lengthToCord / r1) * 2
      beta <- acos(lengthToOtherCord / r2[i]) * 2

      areaSector1 <- r1 ^ 2 * alpha * 1 / 2
      areaSector2 <- r2[i] ^ 2  * beta * 1 / 2
      areaTriangle1 <- lengthToCord * sqrt(r1 ^ 2 - lengthToCord ^ 2)
      areaTriangle2 <- lengthToOtherCord * sqrt(r1 ^ 2 - lengthToCord ^ 2)
      areaSegment1 <- areaSector1 -areaTriangle1
      areaSegment2 <- areaSector2 -areaTriangle2
      ans <- c(ans, areaSegment1 + areaSegment2)
    }
  }
  return(ans)
}

##############
#####TEST#####
##############

# martin_ek
test_that("martin_ek is equivalent to manual computation", {

  print("martin_ek is equivalent to manual computation")
  #Package computation
  me <- pairwise(valdepoza, maxR = 10, kernel = martin_ek,
                 kerpar = list(dbh_mark = 1))


  # Manual computation
  for (tree_index in 1:me$n){
    idbh <- as.numeric(valdepoza$marks[tree_index, 1])
    distances <- as.matrix(dist(cbind(valdepoza$x,
                                      valdepoza$y)))[tree_index, ]
    competitors <- which(distances <= 10 & distances > 0)
    dist_competitors <- distances[competitors]
    jdbh <- valdepoza$marks[competitors, 1]
    cindex <- sum(jdbh / idbh * exp(16 * dist_competitors / (idbh + jdbh)))
    cindex_function <- me$marks[tree_index, 7]
    if(cindex != cindex_function){
      print(cindex)
      print(cindex_function)
    }
    expect_equal(cindex_function, cindex)
  }
})

# bella
test_that("bella is equivalent to manual computation", {

  print("bella is equivalent to manual computation")

  #Package computation
  me <- pairwise(valdepoza, maxR = 10, kernel = bella,
                 kerpar = list(dbh_mark = 1, largestCrownRadiusMark = 5))

  # Manual computation
  for (tree_index in 1:me$n){
    idbh <- as.numeric(valdepoza$marks[tree_index, 1])
    iradius <- as.numeric(valdepoza$marks[tree_index, 5])
    distances <- as.matrix(dist(cbind(valdepoza$x,
                                      valdepoza$y)))[tree_index, ]
    competitors <- which(distances <= 10 & distances > 0)
    dist_competitors <- distances[competitors]
    jdbh <- valdepoza$marks[competitors, 1]
    jradius <- valdepoza$marks[competitors, 5]
    cindex <- sum(intersection_of_circles(dist_competitors, iradius, t(jradius))
                  * jdbh / (pi * iradius^2 * idbh))
    cindex_function <- me$marks[tree_index, 7]
    expect_equal(cindex_function, cindex)
  }
})

# alemdag
test_that("alemdag is equivalent to manual computation", {

  print("alemdag is equivalent to manual computation")
  #Package computation
  me <- pairwise(valdepoza, maxR = 10, kernel = alemdag,
                 kerpar = list(dbh_mark = 1))

  # Manual computation
  for (tree_index in 1:me$n){
    idbh <- as.numeric(valdepoza$marks[tree_index, 1])
    distances <- as.matrix(dist(cbind(valdepoza$x,
                                      valdepoza$y)))[tree_index, ]
    competitors <- which(distances <= 10 & distances > 0)
    dist_competitors <- distances[competitors]
    jdbh <- valdepoza$marks[competitors, 1]

    first_term <- ((dist_competitors * idbh) / (idbh + jdbh)) ^ 2
    second_term <- (jdbh / dist_competitors) / sum(jdbh / dist_competitors)
    cindex <- sum(pi * first_term * second_term)
    cindex_function <- me$marks[tree_index, 7]
    expect_equal(cindex_function, cindex)
  }
})

# crown cross functional kernel
test_that("crown cross functional kernel is equivalent to manual computation", {

  print("crown cross functional kernel is equivalent to manual computation")
  # Using crown cross sectional area
  # generate marks with the functions
  tree_csa_functions <- unlist(apply(valdepoza$marks, 1, function(x) {
    return(function(y) {
      return(ccs_cparea_ellipsoid(as.numeric(x[3]), as.numeric(x[4]),
                                  as.numeric(x[5]), as.numeric(x[2]),
                                  y))})}))
  valdepoza$marks <- valdepoza$marks %>%
    mutate(csFunction = tree_csa_functions)
  height_percentage <- 50
  #Package computation
  me <- pairwise(valdepoza, maxR = 10, kernel = crown_cross_fk, kerpar =
                   list(function_mark = 7, height_tree_mark = 2,
                        height_percentage = height_percentage))

  # Manual computation
  for (tree_index in 1:me$n){

    i_function <- valdepoza$marks[tree_index, 7][[1]][[1]]
    i_height <- as.numeric(valdepoza$marks[tree_index, 2])

    distances <- as.matrix(dist(cbind(valdepoza$x,
                                      valdepoza$y)))[tree_index, ]
    competitors <- which(distances <= 10 & distances > 0)
    dist_competitors <- distances[competitors]

    j_functions <- valdepoza$marks[competitors, 7]


    height_csf <- i_height * (height_percentage / 100)
    cross_sectional_i <- i_function(height_csf)
    cross_sectional_j <- apply(j_functions, 1, function(x) {
      x[[1]](height_csf)})

    cindex <- sum(cross_sectional_j / (cross_sectional_i * dist_competitors))
    cindex_function <- me$marks[tree_index, 8]
    if(cindex_function!=cindex){
      print(cindex_function)
      print(cindex)
    }
    expect_equal(cindex_function, cindex)
  }
})

# crown cross unweighted functional kernel
test_that("crown cross unweighted functional kernel is equivalent to manual
          computation", {

            print("crown cross unweighted functional kernel is equivalent to manual computation")

            # Using crown cross sectional volume
            # generate marks with the functions
            tree_csv_functions <- unlist(
              apply(valdepoza$marks, 1, function(x) {
                return(function(y) {
                  return(ccs_volume_ellipsoid(as.numeric(x[3]), as.numeric(x[4]),
                                              as.numeric(x[5]), as.numeric(x[2]),
                                              y))})})
              )
            height_percentage <- 50
            alpha <- 60
            valdepoza$marks <- valdepoza$marks %>%
              mutate(csFunction = tree_csv_functions)
            #Package computation
            me <- pairwise(valdepoza, maxR = 10,
                           kernel = crown_unweighted_cross_fk,
                           kerpar = list(function_mark = 7,
                                         height_tree_mark = 2,
                                         height_percentage = height_percentage,
                                         alpha = alpha))

            # Manual computation
            for (tree_index in 1:me$n){

              i_function <- valdepoza$marks[tree_index, 7][[1]][[1]]
              i_height <- as.numeric(valdepoza$marks[tree_index, 2])

              distances <- as.matrix(dist(cbind(valdepoza$x,
                                                valdepoza$y)))[tree_index, ]
              competitors <- which(distances <= 10 & distances > 0)
              dist_competitors <- distances[competitors]

              j_functions <- cbind(valdepoza$marks[competitors, 7],
                                   dist_competitors)
              colnames(j_functions) <- c("cs_function", "dists")

              height_csf <- i_height * (height_percentage / 100)

              cross_sectional_i <- i_function(height_csf)
              cross_sectional_j <- apply(j_functions, 1, function(x) {
                height_csf_j <- x$dists * tan(alpha / 2 * pi / 180)
                x$cs_function(height_csf_j)})

              cindex <- sum(cross_sectional_j / cross_sectional_i)
              cindex_function <- me$marks[tree_index, 8]
              if(cindex_function!=cindex){
                print(cindex_function)
                print(cindex)
              }
              expect_equal(cindex_function, cindex)
            }
          })
