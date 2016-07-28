`((lambda ((x : (inr Boolean))) : (Boolean -> Integer) (case x of (inl (lambda ((x1 : Integer)) : Integer (sub1 x1)))
                      or (inr (lambda ((x2 : Boolean)) : Integer 2)))) (inr #f))