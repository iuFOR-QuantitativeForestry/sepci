## code to prepare "valdepoza" dataset goes here
library(usethis)
library(dplyr)
library(tibble)
library(siplab)

set.seed(2)

raw_data <- read.csv("data-raw/Marteloscopes_BiomassBiodiversity_data.csv")

martelo_list <- raw_data %>% group_split(martelo)

valdepoza <- martelo_list [[2]]

# Error correction
temp <- valdepoza$Total_height_m[783]
valdepoza$Total_height_m[783] <- valdepoza$Crownbase_height_m[783]
valdepoza$Crownbase_height_m[783] <- temp

# Error height = crown start height
error_index <- which(valdepoza$Crownbase_height_m ==  valdepoza$Total_height_m)
for (i in error_index){
  valdepoza$Crownbase_height_m[i] <- valdepoza$Crownbase_height_m[i] - .5
}


# I want to preserve
# xyposition
# dbh Diameter at breast height in metres
# height Tree height in metres
# heightStartCrown The height were the crown of the tree starts in metres
# heightLargestCrownRadius The height were the crown of the tree
# reachs its maximun radius in metres
# largestCrownRadius The maximun crown radius of the tree in metres


columns_selected <- c("utmX", "utmY", "DBH_cm", "Total_height_m",
                      "Crownbase_height_m", "species")
valdepoza <- valdepoza [, columns_selected]

# Maximum crown width height equations
# NO EQUATIONS FOR THE MOMENT

compute_max_crown_width_heigt <- function(tree_height, crown_start) {
  runif(1, min = crown_start, max = tree_height)
}

# Crown width equations

# "Quercus ilex"
qi_crown_width <- function(dbh, tree_height) {
  exp(-1.108 + 0.753 * log(dbh) + 0.176 * log(tree_height))
}

# "Quercus pyrenaica"

qp_crown_width <- function(dbh, tree_height) {
  exp(-1.009 + 0.672 * log(dbh) + 0.187 * log(tree_height))
}

# "Pinus sylvestris"

ps_crown_width <-  function(dbh, tree_height) {
  # 0.123 changed to -0.123 original equation wrong ¿?
  exp(-1.100 + 0.952 * log(dbh) - 0.0314 * log(tree_height) - 0.123
      * tree_height)
}

# "Pinus nigra"

pn_crown_width <-  function(dbh, tree_height) {
  exp(-1.082 + 0.774 * log(dbh) - 0.0035 * log(tree_height) - 0.114
      * tree_height)
}

# "Pinus pinea" # NOTFOUND

pp_crown_width <-  pn_crown_width

# "Quercus faginea"     # NOTFOUND

qf_crown_width <- qp_crown_width

# "Juniperus thurifera" # NOTFOUND

jt_crown_width <-  pn_crown_width

compute_crown_width <- function(specie, dbh, tree_height) {
  if (specie == "Quercus ilex") {
    (qi_crown_width(dbh, tree_height) / 2)
  }
  if (specie == "Quercus pyrenaica") {
    return(qp_crown_width(dbh, tree_height) / 2)
  }
  if (specie == "Pinus sylvestris") {
    return(ps_crown_width(dbh, tree_height) / 2)
  }
  if (specie == "Pinus nigra") {
    return(pn_crown_width(dbh, tree_height) / 2)
  }
  if (specie == "Pinus pinea") {
    return(pp_crown_width(dbh, tree_height) / 2)
  }
  if (specie == "Quercus faginea") {
    return(qf_crown_width(dbh, tree_height) / 2)
  }
  if (specie == "Juniperus thurifera") {
    return(jt_crown_width(dbh, tree_height) / 2)
  }
}


valdepoza <- valdepoza %>%
  rowwise() %>%
  mutate(heightLargestCrownRadius =
           compute_max_crown_width_heigt(Total_height_m,
                                         Crownbase_height_m),
         .before = species) %>%
  mutate(largestCrownRadius = compute_crown_width(species,
                                                  DBH_cm,
                                                  Total_height_m),
         .before = species) %>%
  ungroup()



colnames(valdepoza) <- c("x", "y", "dbh", "height", "heightStartCrown",
                         "heightLargestCrownRadius", "largestCrownRadius",
                         "species")

valdepoza <- ppp(valdepoza$x, valdepoza$y,
                 c(min(valdepoza$x), max(valdepoza$x)),
                 c(min(valdepoza$y), max(valdepoza$y)),
                 marks = valdepoza[, 3:8])

use_data(valdepoza, overwrite = TRUE)
