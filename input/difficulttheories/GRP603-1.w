    NAME        GRP603-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 double_divide: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > double_divide > multiply
    VARIABLES  A,B,C: ANY
    EQUATIONS   inverse(double_divide(inverse(double_divide(A,inverse(double_divide(B,double_divide(A,C))))),C)) = B 
                  multiply(A,B) = inverse(double_divide(B,A)) 
    CONCLUSION