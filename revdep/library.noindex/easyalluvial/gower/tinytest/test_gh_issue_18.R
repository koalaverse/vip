df <- data.frame(X = c(4L, 0L, 10L))
obj <- data.frame(X = 5L)

d1 <- gower_dist(obj, df)

df2 <- df[c(2,3,1),,drop=FALSE]
d2 <- gower_dist(obj, df2)

expect_equal(d1,d2[c(3,1,2)])


