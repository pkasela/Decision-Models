######################################
#####################################
# Exercise c.
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
               xt = c(0.25,0.55,0.75,0),
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


################################################
################################################
#Exercise d.

solve(model) #result = 0 -> solved

solution <- get.variables(model)
cat("The optimal variables values are:","\n",
    "A <-",solution[1],", B <-",solution[2],
    ", C <-",solution[3],", D <-",solution[4])

cat("The optimal objective function value is: ",get.objective(model))