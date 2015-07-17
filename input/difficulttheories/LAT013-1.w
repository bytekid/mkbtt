    NAME        LAT013-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
    ORDERING    LPO
                join > meet
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   join(X,meet(Y,meet(X,Z))) = X 
                  meet(X,join(Y,join(X,Z))) = X 
                  join(join(meet(X,Y),meet(Y,Z)),Y) = Y 
                  meet(meet(join(X,Y),join(Y,Z)),Y) = Y 
    CONCLUSION