(case ((inr 1) as (Boolean + Integer))
    of ((inl (lambda ((x : Boolean)) x)) as (Boolean + Integer))
    or ((inr (lambda ((x : Integer)) x)) as (Boolean + Integer)))