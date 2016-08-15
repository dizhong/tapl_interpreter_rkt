# small-step. to be updated

RECORDS

SUMS (with unique typing)

Syntactic forms

terms
```
t ::= ...                        
      ((inl t) as (T1 + T2))
	  ((inr t) as (T1 + T2))
```
values	
```
v ::= ...                       
      ((inl v) as (T1 + T2))
	  ((inr v) as (T1 + T2))
```	  

Evaluation rule

E-CASEINL/R
```
case ((inl t0) as (T1 + T2)) of ((inl t1) as (T1 + T2))
                             or ((inr t2) as (T1 + T2))
							 -> [x1 |-> v0]t1
							 -> [x2 |-> v0]t2
```
E-INL
```
            t1 -> t1'
---------------------------------
(inl t1) as T2 -> (inl t1') as T2
```
E-INR
```
            t1 -> t1'
---------------------------------
(inr t1) as T2 -> (inr t1') as T2
```
(so why are those both T2s????????)


Typing rule

T-INL
```
            Tao |- t1 : T1
----------------------------------------
Tao |- (inl t1) as (T1 + T2) : (T1 + T2)
```
T-INR
```
            Tao |- t1 : T2
----------------------------------------
Tao |- (inr t1) as (T1 + T2) : (T1 + T2)
```
T-CASE
```
               Tao |- t0 : T1 + T2
Tao, x1 : T1 |- t1 : T     Tao, x2 : T2 |- t2 : T
--------------------------------------------------
Tao |- case t0 of inl x1 => t1 | inr x2 => t2 : T
