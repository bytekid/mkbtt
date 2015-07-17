    NAME        LAT040-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
                 xx: -> ANY
                 yy: -> ANY
                 zz: -> ANY
    ORDERING    LPO
                join > meet > xx > yy > zz
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   join(X,meet(Y,Z)) = meet(join(X,Y),join(X,Z)) 
                  meet(X,join(Y,Z)) = join(meet(X,Y),meet(X,Z)) 
                  join(xx,yy) = join(xx,zz) 
                  meet(xx,yy) = meet(xx,zz) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION