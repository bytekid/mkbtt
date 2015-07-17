    NAME        GRP010-4
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 inverse: ANY -> ANY
                 c: -> ANY
                 b: -> ANY
    ORDERING    LPO
                multiply > identity > inverse > c > b
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(c,b) = identity 
    CONCLUSION