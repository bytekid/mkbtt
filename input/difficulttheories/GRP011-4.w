    NAME        GRP011-4
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 inverse: ANY -> ANY
                 b: -> ANY
                 c: -> ANY
                 d: -> ANY
    ORDERING    LPO
                multiply > identity > inverse > b > c > d
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(b,c) = multiply(d,c) 
    CONCLUSION