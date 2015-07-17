    NAME        GRP469-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                divide > inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   divide(inverse(divide(A,divide(B,divide(C,D)))),divide(divide(D,C),A)) = B 
                  multiply(A,B) = divide(A,inverse(B)) 
    CONCLUSION