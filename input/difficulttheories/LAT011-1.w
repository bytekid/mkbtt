    NAME        LAT011-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet2: ANY ANY -> ANY
                 join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
    ORDERING    LPO
                meet2 > join > meet
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   meet2(X,X) = X 
                  meet2(X,Y) = meet2(Y,X) 
                  meet2(X,join(X,Y)) = X 
                  join(X,meet2(X,Y)) = X 
                  meet2(meet2(X,Y),Z) = meet2(X,meet2(Y,Z)) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION