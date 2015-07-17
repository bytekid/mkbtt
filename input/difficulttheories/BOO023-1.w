    NAME        BOO023-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 add: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 n1: -> ANY
                 pixley: ANY ANY ANY -> ANY
    ORDERING    LPO
                multiply > add > inverse > n1 > pixley
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(add(X,Y),Y) = Y 
                  multiply(X,add(Y,Z)) = add(multiply(Y,X),multiply(Z,X)) 
                  add(X,inverse(X)) = n1 
                  pixley(X,Y,Z) = add(multiply(X,inverse(Y)),add(multiply(X,Z),multiply(inverse(Y),Z))) 
                  pixley(X,X,Y) = Y 
                  pixley(X,Y,Y) = X 
                  pixley(X,Y,X) = X 
    CONCLUSION