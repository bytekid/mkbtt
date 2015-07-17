    NAME        BOO014-4
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 additive_identity: -> ANY
                 multiplicative_identity: -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                add > multiply > additive_identity > multiplicative_identity > inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(X,Y) = add(Y,X) 
                  multiply(X,Y) = multiply(Y,X) 
                  add(X,multiply(Y,Z)) = multiply(add(X,Y),add(X,Z)) 
                  multiply(X,add(Y,Z)) = add(multiply(X,Y),multiply(X,Z)) 
                  add(X,additive_identity) = X 
                  multiply(X,multiplicative_identity) = X 
                  add(X,inverse(X)) = multiplicative_identity 
                  multiply(X,inverse(X)) = additive_identity 
    CONCLUSION