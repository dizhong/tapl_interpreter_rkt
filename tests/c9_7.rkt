(let ([x1 (pair {record (price = #f) (sum = (sub1 10))} (279923 as Integer))])
   (if (record-ref ((x1 . 1) price))
       (record-ref ((x1 . 1) sum))
       (x1 . 2)))