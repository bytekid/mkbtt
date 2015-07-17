    NAME        COL043-3
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 h: -> ANY
                 strong_fixed_point: -> ANY
    ORDERING    LPO
                apply > b > h > strong_fixed_point
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(h,X),Y),Z) = apply(apply(apply(X,Y),Z),Y) 
                  strong_fixed_point = apply(apply(b,apply(apply(b,apply(apply(h,apply(apply(b,apply(apply(b,h),apply(b,b))),apply(h,apply(apply(b,h),apply(b,b))))),h)),b)),b) 
    CONCLUSION