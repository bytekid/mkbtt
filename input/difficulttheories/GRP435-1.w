    NAME        GRP435-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   inverse(multiply(multiply(multiply(inverse(multiply(multiply(A,B),C)),A),B),multiply(D,inverse(D)))) = C 
    CONCLUSION