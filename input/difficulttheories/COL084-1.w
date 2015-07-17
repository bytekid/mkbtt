    NAME        COL084-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   response: ANY ANY -> ANY
                 mocking_bird: -> ANY
                 compose: ANY ANY -> ANY
    ORDERING    LPO
                response > mocking_bird > compose
    VARIABLES  A,B,C: ANY
    EQUATIONS   response(mocking_bird,A) = response(A,A) 
                  response(compose(A,B),C) = response(A,response(B,C)) 
    CONCLUSION