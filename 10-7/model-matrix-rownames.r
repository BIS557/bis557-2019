
x <- mtcars[1:10,1:3]

rownames(x) <- NULL

x[1,1] <- NA

model.matrix(cyl ~ ., data = x)

rownames(x) <- letters[1:10]

m <- model.matrix(cyl ~ ., data = x)
rownames(m) <- NULL
m

iris_train <- iris[1:100,]
iris_test <- iris[101:150,]


