    NAME        GRP441-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   inverse(multiply(A,multiply(B,multiply(multiply(inverse(B),C),inverse(multiply(D,multiply(A,C))))))) = D 
    CONCLUSION