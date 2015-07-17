    NAME        BOO007-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiplicative_identity: -> ANY
                 additive_identity: -> ANY
    ORDERING    LPO
                add > multiply > inverse > multiplicative_identity > additive_identity
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(X,Y) = add(Y,X) 
                  multiply(X,Y) = multiply(Y,X) 
                  add(multiply(X,Y),Z) = multiply(add(X,Z),add(Y,Z)) 
                  add(X,multiply(Y,Z)) = multiply(add(X,Y),add(X,Z)) 
                  multiply(add(X,Y),Z) = add(multiply(X,Z),multiply(Y,Z)) 
                  multiply(X,add(Y,Z)) = add(multiply(X,Y),multiply(X,Z)) 
                  add(X,inverse(X)) = multiplicative_identity 
                  add(inverse(X),X) = multiplicative_identity 
                  multiply(X,inverse(X)) = additive_identity 
                  multiply(inverse(X),X) = additive_identity 
                  multiply(X,multiplicative_identity) = X 
                  multiply(multiplicative_identity,X) = X 
                  add(X,additive_identity) = X 
                  add(additive_identity,X) = X 
    CONCLUSION