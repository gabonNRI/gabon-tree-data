
heightRegression <- function (df) {
  df$height <- 42
  df
}

computeHeights <-
  function(input)
  {
    
    trees.str = '"tag1",30.4, 399
 "tag2", 42, 100'
    frame = read.csv(text=trees.str, header=FALSE)
    colnames(frame) <- c("tag", "diameter", "calculated_height" )
    result = heightRegression(frame)
  }

