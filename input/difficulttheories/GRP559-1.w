    NAME        GRP559-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                divide > inverse > multiply
    VARIABLES  A,B,C: ANY
    EQUATIONS   divide(A,inverse(divide(divide(B,C),divide(A,C)))) = B 
                  multiply(A,B) = divide(A,inverse(B)) 
    CONCLUSION