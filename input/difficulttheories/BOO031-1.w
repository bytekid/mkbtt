    NAME        BOO031-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 n1: -> ANY
                 n0: -> ANY
    ORDERING    LPO
                add > multiply > inverse > n1 > n0
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(multiply(X,Y),add(multiply(Y,Z),multiply(Z,X))) = multiply(add(X,Y),multiply(add(Y,Z),add(Z,X))) 
                  add(X,multiply(Y,multiply(X,Z))) = X 
                  add(add(multiply(X,Y),multiply(Y,Z)),Y) = Y 
                  multiply(add(X,inverse(X)),Y) = Y 
                  multiply(X,add(Y,add(X,Z))) = X 
                  multiply(multiply(add(X,Y),add(Y,Z)),Y) = Y 
                  add(multiply(X,inverse(X)),Y) = Y 
                  add(X,inverse(X)) = n1 
                  multiply(X,inverse(X)) = n0 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION