rest2012 <- data.frame(CONAME = c("MCDONALD'S", "MCC DONALD'S", "SPSS Café", "GLM RONALDO'S", "MCMCglmm","CM DONLD'S"))
rest2012

idx <- agrep(pattern = "MCDONALD'S", x = rest2012$CONAME, ignore.case = FALSE, value = FALSE, max.distance = 3)
idx


rest2012[idx, ]
rest2012[idx, ] <- "MCDONALD'S"
rest2012

2>
  
rest2012 <- data.frame(CONAME = c("GOOD","gd","gud","MG"))
rest2012


idx <- agrep(pattern ="GOOD ", x = rest2012$CONAME, ignore.case = FALSE, value = FALSE, max.distance = 3)
idx


rest2012[idx, ]


rest2012[idx, ] <- "GOOD"

rest2012
