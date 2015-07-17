    NAME        COL056-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   response: ANY ANY -> ANY
                 compose: ANY ANY -> ANY
                 a: -> ANY
                 b: -> ANY
                 c: -> ANY
    ORDERING    LPO
                response > compose > a > b > c
    VARIABLES  X,Y,W: ANY
    EQUATIONS   response(compose(X,Y),W) = response(X,response(Y,W)) 
                  response(a,b) = c 
                  response(a,c) = b 
    CONCLUSION