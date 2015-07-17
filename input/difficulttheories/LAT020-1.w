    NAME        LAT020-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
    ORDERING    LPO
                join > meet
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   join(meet(join(meet(X,Y),Z),Y),meet(Z,X)) = meet(join(meet(join(X,Y),Z),Y),join(Z,X)) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
                  join(meet(X,join(Y,Z)),meet(X,Y)) = meet(X,join(Y,Z)) 
                  meet(join(X,meet(Y,Z)),join(X,Y)) = join(X,meet(Y,Z)) 
    CONCLUSION