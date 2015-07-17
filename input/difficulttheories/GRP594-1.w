    NAME        GRP594-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 double_divide: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > double_divide > multiply
    VARIABLES  A,B,C: ANY
    EQUATIONS   inverse(double_divide(double_divide(A,B),inverse(double_divide(A,inverse(double_divide(C,B)))))) = C 
                  multiply(A,B) = inverse(double_divide(B,A)) 
    CONCLUSION