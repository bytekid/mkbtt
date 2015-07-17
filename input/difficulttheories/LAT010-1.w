    NAME        LAT010-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet: ANY ANY -> ANY
                 join: ANY ANY -> ANY
    ORDERING    LPO
                meet > join
    VARIABLES  X,Y,Z,U: ANY
    EQUATIONS   meet(X,join(Y,meet(Z,join(X,U)))) = join(meet(X,join(Y,meet(X,Z))),meet(X,join(meet(X,Y),meet(Z,U)))) 
                  join(X,meet(Y,join(Z,meet(X,U)))) = meet(join(X,meet(Y,join(X,Z))),join(X,meet(join(X,Y),join(Z,U)))) 
                  meet(join(X,meet(Y,Z)),join(Z,meet(X,Y))) = join(meet(Z,join(X,meet(Y,Z))),meet(X,join(Y,Z))) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION