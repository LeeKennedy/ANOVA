library("ProjectTemplate")
load.project()

# This program expects a csv file that is comprised of:
# 1 : Item Number (sequential 1>n)
# 2 : Primary Factor
# 3 : Secondary Factor
# 4 : Tertiary Factor
# 5 : Result (value)

input <- read.csv("tylosin.csv", header = TRUE)

names <- names(input)
colnames(input) <- c("A","B","C","D","E")
names <- cbind(names, c("A","B","C","D","E"))
input$B <- as.factor(input$B)
input$C <- as.factor(input$C)
input$D <- as.factor(input$D)

model <- aov(input$E~input$B + Error(input$B/input$C/input$D))
summary(model)
names

MSd <- summary(model)[3][[1]][[1]][[3]]
MSm <- summary(model)[2][[1]][[1]][[3]]
MSc <- summary(model)[1][[1]][[1]][[3]]
MSi <- summary(model)[4][[1]][[1]][[3]]


l <- length(levels(input$B))
p <- length(levels(input$C))
n <- length(levels(input$D))
r <- length(input$E)/(length(levels(input$B))*length(levels(input$C))*length(levels(input$D)))

analytical.sd <- sqrt(MSd)
sample.sd <- sqrt((MSm-MSd)/2)
site.sd <- sqrt((MSc-MSm)/4)

Var1 <- (MSd-MSi)/r
Var2 <- (MSm-MSd)/(n*r)
Var3 <- (MSc-MSm)/(p*n*r)

library(gplots)
plotmeans(input$E~input$B, xlab="xxx", ylab="yyy", main="Title")

boxplot(input$E~input$B)

