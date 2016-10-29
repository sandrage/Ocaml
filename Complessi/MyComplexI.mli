module type MyComplexI=sig 
 	type complex
    
    val create : ?r:float -> float -> complex 

    val (+/) : complex -> complex -> complex 
    
    val (-/) : complex -> complex -> complex 
    
    val ( */ ) : complex -> complex -> complex
    
    val (//) : complex -> complex -> complex
    
    val tostring : complex -> string
end;;