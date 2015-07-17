    NAME        GRP429-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   multiply(A,inverse(multiply(multiply(inverse(multiply(inverse(B),multiply(inverse(A),C))),D),inverse(multiply(B,D))))) = C 
    CONCLUSION