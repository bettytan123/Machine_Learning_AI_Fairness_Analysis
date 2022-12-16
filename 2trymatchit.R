install.packages("MatchIt")
library("MatchIt")
data("lalonde", package = "MatchIt")

#1:1 NN matching w/ replacement on a logistic regression PS
m.out <- matchit(treat ~ age + educ + race + married + 
                   nodegree + re74 + re75, data = lalonde,
                 replace = TRUE)
m.out
summary(m.out, addlvariables = ~ I(age^2) + I(re74==0) + 
          I(re75==0) + educ:race)
m.sum <- summary(m.out, addlvariables = ~ I(age^2) + I(re74==0) + 
                   I(re75==0) + educ:race)
plot(m.sum, var.order = "unmatched")
#eQQ plot
plot(m.out, type = "qq", which.xs = ~age + nodegree + re74)

#eCDF plot
plot(m.out, type = "ecdf", which.xs = ~educ + married + re75)


#density plot
plot(m.out, type = "density", which.xs = ~age + educ + race)

#Subclassification on a logistic regression PS
s.out <- matchit(treat ~ age + educ + race + married + 
                   nodegree + re74 + re75, data = lalonde,
                 method = "subclass", subclass = 4)
s.out

summary(s.out)

summary(s.out, subclass = TRUE, un = FALSE)

s <- summary(s.out, subclass = TRUE)
plot(s, var.order = "unmatched", abs = FALSE)

plot(s.out, type = "density", which.xs = ~educ + married + re75,
     subclass = 1)






install.packages("cobalt")
library("cobalt")
bal.tab(m.out, un = TRUE, stats = c("m", "v", "ks"))

#Nearest neighbor (NN) matching on the PS
m.out2 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde)

#Balance on covariates after full and NN matching
bal.tab(treat ~ age + educ + race + married + 
          nodegree + re74 + re75, data = lalonde, 
        un = TRUE, weights = list(full = m.out, nn = m.out2))

love.plot(m.out, binary = "std")

love.plot(m.out, stats = c("m", "ks"), poly = 2, abs = TRUE,
          weights = list(nn = m.out2),
          drop.distance = TRUE, thresholds = c(m = .1),
          var.order = "unadjusted", binary = "std",
          shapes = c("circle filled", "triangle", "square"), 
          colors = c("red", "blue", "darkgreen"),
          sample.names = c("Original", "Full Matching", "NN Matching"),
          position = "bottom")

#Density plot for continuous variables
bal.plot(m.out, var.name = "educ", which = "both")

#Bar graph for categorical variables
bal.plot(m.out, var.name = "race", which = "both")

#Mirrored histogram
bal.plot(m.out, var.name = "distance", which = "both",
         type = "histogram", mirror = TRUE)



