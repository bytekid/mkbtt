    NAME        LAT137-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet: ANY ANY -> ANY
                 join: ANY ANY -> ANY
    ORDERING    LPO
                meet > join
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   meet(X,join(Y,Z)) = join(meet(X,join(Z,meet(X,Y))),meet(X,join(Y,meet(X,Z)))) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION