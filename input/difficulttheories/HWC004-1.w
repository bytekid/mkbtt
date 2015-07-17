    NAME        HWC004-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   and: ANY ANY -> ANY
                 n0: -> ANY
                 n1: -> ANY
                 or: ANY ANY -> ANY
                 not: ANY -> ANY
    ORDERING    LPO
                and > n0 > n1 > or > not
    VARIABLES  : ANY
    EQUATIONS   and(n0,n0) = n0 
                  and(n0,n1) = n0 
                  and(n1,n0) = n0 
                  and(n1,n1) = n1 
                  or(n0,n0) = n0 
                  or(n0,n1) = n1 
                  or(n1,n0) = n1 
                  or(n1,n1) = n1 
                  not(n0) = n1 
                  not(n1) = n0 
    CONCLUSION