    NAME        BOO068-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C,D,E,F,G: ANY
    EQUATIONS   multiply(multiply(A,inverse(A),B),inverse(multiply(multiply(C,D,E),F,multiply(C,D,G))),multiply(D,multiply(G,F,E),C)) = B 
    CONCLUSION