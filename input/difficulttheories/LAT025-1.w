    NAME        LAT025-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   meet: ANY ANY -> ANY
                 join: ANY ANY -> ANY
                 meet2: ANY ANY -> ANY
    ORDERING    LPO
                meet > join > meet2
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  join(X,meet(Y,meet(X,Z))) = X 
                  meet(X,join(Y,join(X,Z))) = X 
                  meet2(X,X) = X 
                  meet2(X,join(X,Y)) = X 
                  join(X,meet2(X,Y)) = X 
                  meet2(X,Y) = meet2(Y,X) 
                  join(X,meet2(Y,meet2(X,Z))) = X 
                  meet2(X,join(Y,join(X,Z))) = X 
    CONCLUSION