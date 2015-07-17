    NAME        BOO029-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                add > multiply > inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(X,multiply(Y,multiply(X,Z))) = X 
                  add(add(multiply(X,Y),multiply(Y,Z)),Y) = Y 
                  multiply(add(X,Y),add(X,inverse(Y))) = X 
                  multiply(X,add(Y,add(X,Z))) = X 
                  multiply(multiply(add(X,Y),add(Y,Z)),Y) = Y 
                  add(multiply(X,Y),multiply(X,inverse(Y))) = X 
                  add(X,Y) = add(Y,X) 
                  multiply(X,Y) = multiply(Y,X) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION