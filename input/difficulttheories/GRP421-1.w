    NAME        GRP421-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > multiply
    VARIABLES  A,B,C: ANY
    EQUATIONS   inverse(multiply(inverse(multiply(A,inverse(multiply(inverse(B),multiply(inverse(C),inverse(multiply(inverse(C),C))))))),multiply(A,C))) = B 
    CONCLUSION