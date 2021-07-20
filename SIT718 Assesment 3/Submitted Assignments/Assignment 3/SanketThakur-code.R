#Q2) 
library(lpSolveAPI)
clothFactory <- make.lp(0,9) 
#Objective function maximize profit 
lp.control(clothFactory, sense = "maximize")
#the objective function

#Order of decision variables: XC1, XC2, XC3, XW1, XW2, XW3, XS1, XS2, XS3

# max(Z)=  60(xc1+xw1+xs1)+55(xc2+xw2+xs2)+60(xc3+xw3+xs3)
#         -30(XC1+XC2+CX3)-45(XW1+XW2+XW3)-50(XS1+XS2+XS3)
#         -5(XC1+XC2+CX3)-4(XW1+XW2+XW3)-5(XS1+XS2+XS3)

set.objfn(clothFactory, c(60,55,60,30,45,30,5,4,5))

# Constraints:
# 1) xc1+xw1+xs1 <= 3800
# 2) xc2+xw2+xs2 <= 3200
# 3) xc3+xw3+xs3 <= 3500
# 4) xc1 >= 0.55(xc1+xw1+xs1)
# 5) xw1 >= 0.30(xc1+xw1+xs1)
# 6) xc2 >= 0.45(xc2+xw2+xs2)
# 7) xw2 >= 0.40(xc2+xw2+xs2)
# 8) xc3 >= 0.30(xc3+xw3+xs3)
# 9) xw3 >= 0.50(xc3+xw3+xs3)
#10)x1,x2,x3,xc1,xc2,xc2,xw1,xw2,xw3,xs1,xs2,xs3,z>=0

add.constraint(clothFactory, c(1,0,0,1,0,0,1,0,0), "<=", 3800) 
add.constraint(clothFactory, c(0,1,0,0,1,0,0,1,0), "<=", 3200)
add.constraint(clothFactory, c(0,0,1,0,0,1,0,0,1), "<=", 3500)

add.constraint(clothFactory, c(-0.45,0,0,0.55,0,0,0.55,0,0), ">=", 0) 
add.constraint(clothFactory, c(-0.3,0,0,-0.3,0,0,0.7,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,-0.55,0,0,0.45,0,0,0.45,0), ">=", 0) 
add.constraint(clothFactory, c(0,-0.4,0,0,-0.4,0,0,0.6,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,-0.7,0,0,0.3,0,0,0.3), ">=", 0)  
add.constraint(clothFactory, c(0,0,-0.5,0,0,-0.5,0,0,0.5), ">=", 0) 

add.constraint(clothFactory, c(1,0,0,0,0,0,0,0,0), ">=", 0)  
add.constraint(clothFactory, c(0,1,0,0,0,0,0,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,1,0,0,0,0,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,0,1,0,0,0,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,0,0,1,0,0,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,0,0,0,1,0,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,0,0,0,0,1,0,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,0,0,0,0,0,1,0), ">=", 0) 
add.constraint(clothFactory, c(0,0,0,0,0,0,0,0,1), ">=", 0) 

decision.variable.names <- c("XC1", "XC2", "XC3", "XW1", "XW2", "XW3","XS1", "XS2", "XS3")

solve(clothFactory)
get.objective(clothFactory)
get.variables(clothFactory)
get.constraints(clothFactory) 
clothFactory

############################################################################################################
############################################################################################################

#Q3) d)
#
#
#
# Player II's game #

library(lpSolveAPI)

lprec <- make.lp(0, 7) # y1 y2 y3 y4 y5 y6 v

lp.control(lprec, sense= "minimize") #  can change sense to  "maximize"



set.objfn(lprec, c(0, 0, 0, 0, 0, 0, 1))

add.constraint(lprec, c(1, 0, 0, 0, 0, 1, 1), ">=", 0)

add.constraint(lprec, c(1, 1, 0, 0, 1, 1, 1), ">=", 0)

add.constraint(lprec, c( 0, 1, 1, 1, 1, 0, 1), ">=", 0)

add.constraint(lprec, c(0, 0, 2, 2, 0, 0, 1), ">=", 0)

add.constraint(lprec, c( 0, 1, 1, 1, 1, 0, 1), ">=", 0)

add.constraint(lprec, c( 1, 0, 0, 0, 0, 1, 1), ">=", 0)

add.constraint(lprec, c(1, 1, 1, 1, 1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0, 0,0, 0, 0, -Inf))

RowNames <- c("Row1", "Row2", "Row3", "Row4", "Row5", "Row6", "Row7")

ColNames <- c("y1", "y2", "y3","y4", "y5", "y6", "v")

dimnames(lprec) <- list(RowNames, ColNames)



solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(lprec)

get.variables(lprec)

get.constraints(lprec)


lprec