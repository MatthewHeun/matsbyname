a <- matrix(c(-1,0,4,5), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
b <- a + 2

DF <- data.frame(a = I(list()), b = I(list()), c = I(list()), d = I(list()))
DF[[1, "a"]] <- a
DF[[2, "a"]] <- a
DF[[1, "b"]] <- b
DF[[2, "b"]] <- b
DF$c <- c(10,11)
DF$d <- c(12,13)

zap_negs <- function(e){
  ifelse(e < 0, 0, e)
}







sumrows <- function(a, b){
  print("inside sumrows")
  print(paste("a =", a, "b =", b))
  a+b
}

Map(f = `+`, DF$a, DF$b)

Map(f = `+`, DF$c, DF$c)


Map(f = `+`, DF$a)


