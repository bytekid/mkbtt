    NAME        GRP478-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                divide > inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   divide(inverse(divide(divide(divide(A,A),B),divide(C,divide(B,D)))),D) = C 
                  multiply(A,B) = divide(A,inverse(B)) 
    CONCLUSION