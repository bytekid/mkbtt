    NAME        COM010-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   c_UNITY_Omk__program: ANY -> ANY
                 c_Pair: ANY ANY ANY ANY -> ANY
                 c_UNITY_OInit: ANY ANY -> ANY
                 y: -> ANY
                 a: -> ANY
                 c_UNITY_OActs: ANY ANY -> ANY
                 c_UNITY_OAllowedActs: ANY ANY -> ANY
                 tc_set: ANY -> ANY
                 tc_prod: ANY ANY -> ANY
    ORDERING    LPO
                c_UNITY_Omk__program > c_Pair > c_UNITY_OInit > y > a > c_UNITY_OActs > c_UNITY_OAllowedActs > tc_set > tc_prod
    VARIABLES  UNITY_Omk__program,Pair,UNITY_OInit,V_y,T_a,UNITY_OActs,UNITY_OAllowedActs,T: ANY
    EQUATIONS   c_UNITY_Omk__program(c_Pair(c_UNITY_OInit(V_y,T_a),c_Pair(c_UNITY_OActs(V_y,T_a),c_UNITY_OAllowedActs(V_y,T_a),tc_set(tc_set(tc_prod(T_a,T_a))),tc_set(tc_set(tc_prod(T_a,T_a)))),tc_set(T_a),tc_prod(tc_set(tc_set(tc_prod(T_a,T_a))),tc_set(tc_set(tc_prod(T
    CONCLUSION