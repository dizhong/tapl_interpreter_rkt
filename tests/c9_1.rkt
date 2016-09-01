(let ([p1 (pair #f 2)])
   (let ([p2 (pair (* 5 6) (if (p1 . 1) 3 4))])
     (p2 . 2)))