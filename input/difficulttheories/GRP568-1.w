    NAME        GRP568-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   double_divide: ANY ANY -> ANY
                 identity: -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                double_divide > identity > multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   double_divide(double_divide(A,double_divide(double_divide(B,double_divide(A,C)),double_divide(identity,C))),double_divide(identity,identity)) = B 
                  multiply(A,B) = double_divide(double_divide(B,A),identity) 
                  inverse(A) = double_divide(A,identity) 
                  identity = double_divide(A,inverse(A)) 
    CONCLUSION