    NAME        GRP474-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                divide > inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   divide(divide(inverse(divide(A,B)),divide(divide(C,D),A)),divide(D,C)) = B 
                  multiply(A,B) = divide(A,inverse(B)) 
    CONCLUSION