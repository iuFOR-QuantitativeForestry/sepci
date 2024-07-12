library(siplab)
library(sepci)
library(tidyverse)
library(ggforce)

data <- valdepoza

# In our case we will have 3 different mean trees
meanTrees <- valdepoza$marks |>
  group_by_if(function(x) is.factor(x) || is.character(x)) |>
  summarize_all(~mean(.x))

all <- tibble(
  i = numeric(),
  x = numeric(),
  y  = numeric(),
  index = numeric()
)

kernel <- bella
kerpar <- list(dbh_mark = "dbh",largestCrownRadiusMark = "largestCrownRadius")
select <- powlinear_sel
selpar <- list(ki=10, kj=0, p=1, r0=0, smark="largestCrownRadius")


for(i in 1:nrow(meanTrees)){
  meanTree <- meanTrees[i,]
  marksMod <- valdepoza$marks |> add_row(meanTree)

  minX <- min(valdepoza$x)-1
  maxX <- max(valdepoza$x)+1
  minY <- min(valdepoza$y)-1
  maxY <- max(valdepoza$y)+1

  for(x in seq(minX,maxX,by=.5)){
    for(y in seq(minY,maxY,by=.5)){
      distAll <- crossdist(x,y,valdepoza$x,valdepoza$y)
      competitorsIndexes <- select(meanTree,valdepoza$marks,distAll,rank(distAll),selpar)
      print(sum(competitorsIndexes))
      competitorsMarks <- valdepoza$marks[competitorsIndexes,]
      competitorsX <- valdepoza$x[competitorsIndexes]
      competitorsY <- valdepoza$y[competitorsIndexes]
      distCompetitors <- distAll[competitorsIndexes]
      index<- sum(kernel(meanTree,competitorsMarks,distCompetitors,rank(distCompetitors),kerpar))
      all <- all |> add_row(i=i,x=x,y=y,index=index)
    }
  }
}

dataf_valdepoza <- valdepoza$marks |>
  mutate(x = valdepoza$x) |>
  mutate(y = valdepoza$y)
cbind(valdepoza$x,valdepoza$y,valdepoza$marks)

# Change filter for different mean tree
ggplot() +
  geom_tile(data=all |> filter(i==3),aes(x=x, y=y,fill=index)) +
  geom_circle(data=dataf_valdepoza, aes(x0=x, y0=y, r = largestCrownRadius)) +
  geom_point(data=dataf_valdepoza, aes(x=x, y=y))
