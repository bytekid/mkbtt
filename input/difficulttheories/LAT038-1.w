    NAME        LAT038-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
                 f: ANY ANY -> ANY
                 n0: -> ANY
                 aa: -> ANY
                 bb: -> ANY
                 dd: -> ANY
                 cc: -> ANY
    ORDERING    LPO
                join > meet > f > n0 > aa > bb > dd > cc
    VARIABLES  X,Y,Z,U,V,W: ANY
    EQUATIONS   join(X,meet(Y,Z)) = meet(join(X,Y),join(X,Z)) 
                  meet(X,join(Y,Z)) = join(meet(X,Y),meet(X,Z)) 
                  f(join(U,V),W) = join(f(U,W),f(V,W)) 
                  f(n0,W) = n0 
                  f(W,join(U,V)) = join(f(W,U),f(W,V)) 
                  f(W,n0) = n0 
                  f(join(aa,bb),dd) = f(join(cc,bb),dd) 
                  meet(f(aa,dd),f(bb,dd)) = meet(f(cc,dd),f(bb,dd)) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION