    NAME        LAT026-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet: ANY ANY -> ANY
                 join: ANY ANY -> ANY
    ORDERING    LPO
                meet > join
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   meet(X,join(Y,join(X,Z))) = X 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(join(X,Y),join(Z,Y)),Y) = Y 
                  join(join(meet(X,Y),meet(Z,Y)),Y) = Y 
    CONCLUSION