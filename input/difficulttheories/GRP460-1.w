    NAME        GRP460-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 identity: -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                divide > identity > multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   divide(A,divide(divide(divide(identity,B),C),divide(divide(divide(A,A),A),C))) = B 
                  multiply(A,B) = divide(A,divide(identity,B)) 
                  inverse(A) = divide(identity,A) 
                  identity = divide(A,A) 
    CONCLUSION