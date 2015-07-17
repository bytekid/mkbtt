    NAME        BOO073-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 add: ANY ANY -> ANY
    ORDERING    LPO
                inverse > add
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   inverse(add(inverse(add(inverse(add(A,B)),C)),inverse(add(A,inverse(add(inverse(C),inverse(add(C,D)))))))) = C 
    CONCLUSION