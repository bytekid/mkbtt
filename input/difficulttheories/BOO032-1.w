    NAME        BOO032-1
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
                  multiply(add(X,inverse(X)),Y) = Y 
                  multiply(X,add(Y,add(X,Z))) = X 
                  multiply(multiply(add(X,Y),add(Y,Z)),Y) = Y 
                  add(multiply(X,inverse(X)),Y) = Y 
                  add(multiply(add(X,Y),X),multiply(X,Y)) = X 
                  add(multiply(add(X,X),Y),multiply(X,X)) = X 
                  add(multiply(add(X,Y),Y),multiply(X,Y)) = Y 
                  multiply(add(multiply(X,Y),X),add(X,Y)) = X 
                  multiply(add(multiply(X,X),Y),add(X,X)) = X 
                  multiply(add(multiply(X,Y),Y),add(X,Y)) = Y 
    CONCLUSION