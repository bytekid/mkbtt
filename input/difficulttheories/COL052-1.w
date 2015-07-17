    NAME        COL052-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   response: ANY ANY -> ANY
                 compose: ANY ANY -> ANY
                 c: -> ANY
                 common_bird: ANY -> ANY
                 a: -> ANY
                 b: -> ANY
    ORDERING    LPO
                response > compose > c > common_bird > a > b
    VARIABLES  X,Y,W: ANY
    EQUATIONS   response(compose(X,Y),W) = response(X,response(Y,W)) 
                  response(c,common_bird(X)) = response(X,common_bird(X)) 
                  c = compose(a,b) 
    CONCLUSION