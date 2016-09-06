(let ([p1 (pair #f 1)])
   (let ([p2 (pair (* 5 6) (if (p1 . 1) 3 279923))])
     (p2 . 2)))