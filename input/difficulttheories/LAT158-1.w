    NAME        LAT158-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet: ANY ANY -> ANY
                 join: ANY ANY -> ANY
    ORDERING    LPO
                meet > join
    VARIABLES  X,Y,Z,U: ANY
    EQUATIONS   meet(X,join(Y,meet(Z,join(X,U)))) = meet(X,join(Y,meet(Z,join(X,meet(Z,join(Y,U)))))) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION