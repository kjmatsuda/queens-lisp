;; B.4.3 let
(lambda (x) ((lambda (a b)
             (+ (* a x) b)) 2 3))

;; B.4.4 引数の書式
((lambda (x y . z)
   (+ (* x x) (* y y) (* 5 5)))
 2 3)
