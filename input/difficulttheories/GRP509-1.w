    NAME        GRP509-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   multiply(multiply(multiply(A,B),C),inverse(multiply(A,C))) = B 
    CONCLUSION