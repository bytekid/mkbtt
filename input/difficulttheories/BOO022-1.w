    NAME        BOO022-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 add: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 n1: -> ANY
                 n0: -> ANY
    ORDERING    LPO
                multiply > add > inverse > n1 > n0
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(add(X,Y),Y) = Y 
                  multiply(X,add(Y,Z)) = add(multiply(Y,X),multiply(Z,X)) 
                  add(X,inverse(X)) = n1 
                  add(multiply(X,Y),Y) = Y 
                  add(X,multiply(Y,Z)) = multiply(add(Y,X),add(Z,X)) 
                  multiply(X,inverse(X)) = n0 
    CONCLUSION