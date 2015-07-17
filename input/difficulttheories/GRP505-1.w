    NAME        GRP505-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C,D,E,F: ANY
    EQUATIONS   multiply(inverse(multiply(inverse(multiply(inverse(multiply(A,B)),multiply(B,A))),multiply(inverse(multiply(C,D)),multiply(C,inverse(multiply(multiply(E,inverse(F)),inverse(D))))))),F) = E 
    CONCLUSION