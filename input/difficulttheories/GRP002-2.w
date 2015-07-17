    NAME        GRP002-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > identity > inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(X,identity) = X 
                  multiply(X,inverse(X)) = identity 
                  multiply(X,multiply(X,X)) = identity 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION