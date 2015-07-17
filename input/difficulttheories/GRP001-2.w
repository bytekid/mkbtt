    NAME        GRP001-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 inverse: ANY -> ANY
                 a: -> ANY
                 b: -> ANY
                 c: -> ANY
    ORDERING    LPO
                multiply > identity > inverse > a > b > c
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(X,identity) = X 
                  multiply(X,inverse(X)) = identity 
                  multiply(X,X) = identity 
                  multiply(a,b) = c 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION