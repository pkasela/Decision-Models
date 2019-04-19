######################################
#####################################
# Exercise c.
#non binary model
if(require(lpSolveAPI)==FALSE) install.packages("lpSolveAPI")
model = make.lp(0,4)                        # 4 Variables (A,B,C,D)
lp.control(model, sense = "max")            #Maximazing Problem
set.objfn(model,obj = c(2.14,1.13,4,2.12))  #Objective function coefficients

row.add.mode(model,"on")

add.constraint(model,
               xt = c(0.55,0.45,0.25,1),
               type = "<=",
               rhs = 1300,
               indices = c(1:4)) #Aluminium Constraint

add.constraint(model,
               xt = c(0.45,0.55,0.75,0),
               type = "<=",
               rhs = 900,
               indices = c(1:4)) #Copper Constraint

add.constraint(model,
               xt = c(1.2,1,1,1.5),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Forger Time Constraint

add.constraint(model,
               xt = c(1.75,1,1,1.5),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Reamer Time Constraint

add.constraint(model,
               xt = c(1.3,0.6,0.4,0),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Shaper Time Constraint

add.constraint(model,
               xt = c(2.5,1.2,2,1.5),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Planer Time Constraint

#lower and upper bounds
set.bounds(model,lower = c(1200,450,0,0), upper = c(Inf,650,120,150)) 

row.add.mode(model,"off")

#Give names to contraints and variables
dimnames(model)<- list(c("Aluminium","Copper","Forger","Reamer","Shaper","Planer"),
                       c("A","B","C","D"))

print(model) #To see to model


#Binary model
bin_model = make.lp(0,8)                    # 8 Variables
set.type(bin_model, c(5:8), "binary")
lp.control(bin_model, sense = "max")        #Maximazing Problem
set.objfn(bin_model,obj = c(2.141,1.125,4,2.116,-600,-500,-250,-300))   #Objective function coefficients


row.add.mode(bin_model,"on")

add.constraint(bin_model,
               xt = c(0.55,0.45,0.25,1),
               type = "<=",
               rhs = 1300,
               indices = c(1:4)) #Aluminium Constraint

add.constraint(bin_model,
               xt = c(0.45,0.55,0.75,0),
               type = "<=",
               rhs = 900,
               indices = c(1:4)) #Copper Constraint

add.constraint(bin_model,
               xt = c(1.2,1,1,1.5),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Forger Time Constraint

add.constraint(bin_model,
               xt = c(1.75,1,1,1.5),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Reamer Time Constraint

add.constraint(bin_model,
               xt = c(1.3,0.6,0.4,0),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Shaper Time Constraint

add.constraint(bin_model,
               xt = c(2.5,1.2,2,1.5),
               type = "<=",
               rhs = 3600,
               indices = c(1:4)) #Planer Time Constraint

add.constraint(bin_model,
               xt = c(1,-1440),
               type = "<=",
               rhs = 0,
               indices = c(1,5)) #linking contraint for A

add.constraint(bin_model,
               xt = c(1,-1636.36),
               type = "<=",
               rhs = 0,
               indices = c(2,6)) #linking contraint for B

add.constraint(bin_model,
               xt = c(1,-1200),
               type = "<=",
               rhs = 0,
               indices = c(3,7)) #linking contraint for C

add.constraint(bin_model,
               xt = c(1,-1300),
               type = "<=",
               rhs = 0,
               indices = c(4,8)) #linking contraint for D

#Setting lower and upper bounds
set.bounds(bin_model, lower = c(1200,450,0,0,0,0,0,0), 
           upper = c(Inf,650,120,150,1,1,1,1)) 

row.add.mode(bin_model,"off")

#Giving names to the contraints and variables
dimnames(bin_model)<- list(c("Aluminium","Copper","Forger","Reamer","Shaper","Planer",
                             "Linking A","Linking B","Linking C","Linking D"),
                           c("A","B","C","D","Y_A","Y_B","Y_C","Y_D"))
name.lp(bin_model, "Alloy Corporation Binary Problem")

print(bin_model)



################################################
################################################
#Exercise d.

#Non Binary model
solve(model) #result = 0 -> solved

solution <- get.variables(model)

fixed_cost <- 1650

cat("The optimal variables values are:","\n",
    "A <-",solution[1],", B <-",solution[2],
    ", C <-",solution[3],", D <-",solution[4])

cat("The optimal objective function value is: ",get.objective(model), "\n",
    "Thus the optimal net profit value is",get.objective(model) - fixed_cost)

# Binary model 
solve(bin_model) #result = 0 -> solved

solution <- get.variables(bin_model)
cat("The optimal variables values are:","\n",
    "A <-",solution[1],", B <-",solution[2],
    ", C <-",solution[3],", D <-",solution[4] , "\n",
    "Y_A <-",solution[5],", Y_B <-",solution[6],
    ", Y_C <-",solution[7],", Y_D <-",solution[8])

cat("The optimal objective function value is: ",get.objective(bin_model))

################################################
################################################
#Exercise e.
if(require(dplyr)==FALSE) install.packages("dplyr")
if(require(tidyr)==FALSE) install.packages("tidyr")
#two funtions for printing the sensitivity stuff
printSensitivityRHS <- function(model, numVars){
  
  arg.rhs <- get.sensitivity.rhs(model)
  arg.rhs$dualsfrom <- round(arg.rhs$dualsfrom,3)
  arg.rhs$dualstill <- round(arg.rhs$dualstill,3)
  numVars <- length(get.variables(model))
  numRows <- length(arg.rhs$duals)
  
  symb <- c() 
  for (i in c(1:numRows)) { 
    symb[i] <- paste("B", i, sep = "" ) 
  }
  
  rhs <- data.frame(rhs = symb,duals=arg.rhs$duals,arg.rhs)
  
  rhs<-rhs %>%
    mutate(dualsfrom=replace(dualsfrom, dualsfrom < -1e10, "-inf")) %>%
    mutate(dualstill=replace(dualstill, dualstill > 1e10, "inf")) %>%
    unite(col = "Sensitivity",  
          dualsfrom, rhs, dualstill , 
          sep = " <= ", remove = FALSE) %>%
    select(c("rhs","duals","Sensitivity"))
  
  colnames(rhs)[1]<-c('Rhs')
  print(rhs[1:(numRows-numVars),])
  #print(rhs)
}

printSensitivityObj <- function(model){
  arg.obj = get.sensitivity.obj(model)
  
  #rounding stuff
  arg.obj$objfrom <- round(arg.obj$objfrom,3)
  arg.obj$objtill <- round(arg.obj$objtill,3)
  
  numRows <- length(arg.obj$objfrom)
  symb <- c() 
  for (i in c(1:numRows)) { 
    symb[i] <- paste("C", i, sep = "" ) 
  }
  
  obj <- data.frame(Objs = symb, arg.obj)
  obj<-
    obj %>%
    mutate(objfrom=replace(objfrom, objfrom < -1e10, "-inf")) %>%
    mutate(objtill=replace(objtill, objtill > 1e10, "inf")) %>%
    unite(col = "Sensitivity",  
          objfrom, Objs, objtill , 
          sep = " <= ", remove = FALSE) %>%
    select(c("Objs","Sensitivity"))
  print(obj)
}
#print the sensitivity on the coefficients of objective function
printSensitivityObj(model)
#print sensitivity on the rhs of constraints
printSensitivityRHS(model)

################################################
################################################
#Exercise f.

#Check the value to see if the solution is degenerate
solve.lpExtPtr(model)

################################################
################################################
#Exercise g.
get.solutioncount(model)

################################################
################################################
#Exercise h. & i.
#Use only the last 4 elements of the array
red_cost <- tail(get.sensitivity.rhs(model)$duals,4)
print(data.frame("Variables"=c("A","B","C","D"), 
                 "Final_Quantity"=get.variables(model),
                 "Reduced_Cost"=red_cost))

################################################
################################################
#Exercise j. + shadow price from Exercise e.
get.constraints(model)

################################################
################################################
#Exercise l.
model2 <- model #We just change the objective function
set.objfn(model2,obj=c(2.141-0.25,1.125,4,2.116))
solve(model2)
cat(" New Model: ", get.variables(model2),"\n",
    "Old Model: ", get.variables(model))
cat("New value of the funtion: ", get.objective(model2))