    NAME        LAT124-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
    ORDERING    LPO
                join > meet
    VARIABLES  X,Y,Z,U: ANY
    EQUATIONS   join(X,meet(Y,join(X,join(Z,U)))) = join(X,meet(Y,join(Z,meet(join(X,U),join(Y,U))))) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION