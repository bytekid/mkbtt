    NAME        BOO026-1
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
    EQUATIONS   multiply(X,add(Y,Z)) = add(multiply(Y,X),multiply(Z,X)) 
                  add(X,inverse(X)) = n1 
                  add(X,multiply(Y,Z)) = multiply(add(Y,X),add(Z,X)) 
                  multiply(X,inverse(X)) = n0 
                  add(multiply(X,inverse(X)),add(multiply(X,Y),multiply(inverse(X),Y))) = Y 
                  add(multiply(X,inverse(Y)),add(multiply(X,Y),multiply(inverse(Y),Y))) = X 
                  add(multiply(X,inverse(Y)),add(multiply(X,X),multiply(inverse(Y),X))) = X 
                  multiply(add(X,inverse(X)),multiply(add(X,Y),add(inverse(X),Y))) = Y 
                  multiply(add(X,inverse(Y)),multiply(add(X,Y),add(inverse(Y),Y))) = X 
                  multiply(add(X,inverse(Y)),multiply(add(X,X),add(inverse(Y),X))) = X 
    CONCLUSION