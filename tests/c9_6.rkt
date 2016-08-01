`((lambda ((x : (Integer + Boolean)))
    (case x of ((inl (lambda ((x1 : Integer)) (sub1 x1))) as (Integer + Boolean))
            or ((inr (lambda ((x2 : Boolean)) 2)) as (Integer + Boolean))))
  ((inr #f) as (Integer + Boolean)))