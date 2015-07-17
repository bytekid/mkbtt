    NAME        LAT008-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet: ANY ANY -> ANY
                 join: ANY ANY -> ANY
    ORDERING    LPO
                meet > join
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   meet(X,join(X,Y)) = X 
                  meet(X,join(Y,Z)) = join(meet(Z,X),meet(Y,X)) 
    CONCLUSION