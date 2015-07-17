    NAME        COL059-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   response: ANY ANY -> ANY
                 kestrel: -> ANY
                 lark: -> ANY
                 x2: -> ANY
                 l2: -> ANY
                 l3: -> ANY
    ORDERING    LPO
                response > kestrel > lark > x2 > l2 > l3
    VARIABLES  X1,X2: ANY
    EQUATIONS   response(response(kestrel,X1),X2) = X1 
                  response(response(lark,X1),X2) = response(X1,response(X2,X2)) 
                  response(response(response(lark,lark),X1),X2) = response(response(X1,X1),response(X2,X2)) 
                  response(response(response(response(lark,lark),lark),X1),X2) = response(response(response(X1,X1),response(X1,X1)),response(x2,x2)) 
                   response(l2,l2) != l2 
                  response(lark,lark) = l2 
                  response(l2,lark) = l3 
    CONCLUSION