    NAME        GRP607-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   double_divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                double_divide > inverse > multiply
    VARIABLES  A,B,C: ANY
    EQUATIONS   double_divide(inverse(double_divide(A,inverse(double_divide(inverse(B),double_divide(A,C))))),C) = B 
                  multiply(A,B) = inverse(double_divide(B,A)) 
    CONCLUSION