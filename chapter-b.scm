;;;;;;;; B.4. 特殊形式 ;;;;;;;;
;;;; B.4.3 let

;; ax+b の a,b を定数とする関数の定義を考える
;; lambda で書くと、以下のとおり
(define f_2x+3
  (lambda (x)
    ((lambda (a b)
       (+ (* a x) b)) 2 3)))

(f_2x+3 4)

;; let で書くと、lambda で書くより分かりやすい
(define f_2x+4
  (lambda (x)
    (let ((a 2)
          (b 4))
      (+ (* a x) b))))

(f_2x+4 4)

;; クロージャ
(lambda (x) ((lambda (a b)
             (+ (* a x) b)) 2 3))

;;;; B.4.4 引数の書式
;; 引数にドットをつけることでオプショナル引数にできる
;; ドット記法の右側はリストとして処理される
((lambda (x y . z)
   (+ (* x x) (* y y) (* 5 5)))
 2 3)

;; だが、上記の場合、zに何を指定しても無視されるだけなので、
;; 真の意味でオプショナル引数とは呼べない。

;;;; B.4.5 引数のない関数
(lambda () (* 2 3))

;; 括弧でくくれば評価できる
((lambda () (* 2 3)))

;; late は括弧をつければ評価される
(define late (lambda () (* 2 3)))
(late)

;; now は括弧なしで評価される
(define now (* 2 3))
now

;;;; 遅延評価
;; 評価値を求める作業を遅延させる
;; この段階では評価値を出力せず、保留する
(delay (* 2 3))

;; force によって、初めて評価値を出力する
(force (delay (* 2 3)))

;;;; B.4.7 quote
;; 式をデータとして評価を保留する
(quote (+ 2 3))

;; eval で quote したデータを評価できる
(eval (quote (+ 2 3)) (interaction-environment))


(cons '(a b) '(c d))

(append '((a b)) '(c d))

;;;; B.4.8 内部と外部
(define scope 'external)

;; 外部に定義された変数を参照する
((lambda () scope))

;; 内部に定義された変数を参照する
;; つまり変数は内部から順に探索される
((lambda ()
   (let ((scope 'internal))
     scope)))

;; 左から右へ順序を限定して設定する let*
(let* ((x 2)
       (y 3)
       (z (* x y)))
  z)

;;
(letrec ((z (lambda () (* x y)))
         (x 2)
         (y 3))
  z)

;;;; B.4.9 set!
;; 大域変数
(define z 5)

((lambda ()
   (let ((z 0))
     (set! z (* 2 4)))
   z))

((lambda ()
   (set! z (* 3 5))))

;; 大域環境によるカウンタ
(define n 0)
;; 以下を繰り返し実行することでカウントアップされる
(set! n (+ n 1))

;; 内部環境によるカウンタ
(define count
  (let ((n 0))
    (lambda ()
    (set! n (+ n 1)))))
(count)

;; さらに一般的にすると
(define generator
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1))))))

;; それぞれ独立にカウントアップする counter1, counter2 ができる
(define counter1 (generator))
(counter1)

(define counter2 (generator))
(counter2)

;;;; B.4.11 特殊形式の意味
;; 絶対値関数を cond を使って書く
(define abs
  (lambda (x)
    (cond ((positive? x) x)
          ((zero? x) 0)
          ((negative? x) (- x)))))

;;;;;;;; B.5 型の確認と構文の拡張 ;;;;;;;;
;;;; B.5.1 非数値データの型
;; 型チェックの手続を定義する
(define type-check
  (lambda (x)
    (define form
      (lambda (str)
        (display "This is ") (display str)))
    (cond ((procedure? x) (form "a procedure: ") x)
          ((number?    x) (form "a number: ") x)
          ((pair?      x) (form "a pair: ") x)
          ((null?      x) (form "the empty: ") x)
          ((symbol?    x) (form "a symbol: ") x)
          ((string?    x) (form "a string: ") x)
          ((char?      x) (form "a charactor: ") x)
          ((boolean?   x) (form "a boolean: ") x)
          ((vector?    x) (form "a vector: ") x)
          (else (display
                 "may be a special form: ") x)
          )))

(type-check 9)
(type-check "test")
(type-check (list 1 2 3))
(type-check '(1 2 3))
(type-check '())
(type-check 'test)
(type-check #\a)
(type-check #t)
(type-check #(1 2))

;;;; B.5.2 数値データの型
;; 数値データの型チェックを定義する
(define type-of
  (lambda (x)
    (define form
      (lambda (str) (display str) (display "/ ")))
    (display "This is ")
    (cond ((number? x) (display "a number: ")
           (cond ((and (real? x) (not (negative? x)))
                  (form "real/ nonnegative"))
                 ((and (real? x) (negative? x))
                  (form "real/ negative"))
                 (else
                  (form "complex")))
           (cond ((and (integer? x) (odd? x))
                  (form "integer/ odd"))
                 ((and (integer? x) (even? x))
                  (form "integer/ even"))
                 (else
                  (form "noninteger")))
           (cond ((exact? x) (form "exact"))
                 (else
                  (form "inexact"))) x)
          (else (display "a string: ") x))
    ))

(type-of 2+5i)
(type-of 'symbol)
(type-of -2/5)
(type-of -0.4)
(type-of 5)
(type-of 0)

;;;; B.5.3 構文の拡張 
;; define-syntax でマクロを定義できる
;; まずは if に then else を追加して、new-if を定義する
(define-syntax new-if
  (syntax-rules (then else)
    ((new-if predi then consq else altna)
     (if predi consq altna))
    ((new-if predi then consq)
     (if predi consq #f))
    ((new-if predi else altna)
     (if predi #f altna))
    ))

(new-if #f then 1 else 0)

(new-if #f then 1)

(new-if #t else 0)

;; 次に nand を定義する
(define-syntax nand
  (syntax-rules ()
    ;; _ は nand の略記
    ((_) #f)
    ;; ...(三点ドット) は手続定義の末尾ドットと同様に多引数の為の表記
    ((_ p q ...) (not (and p q ...)))))

;; 遅延リストを作成するマクロ
(define-syntax s-cons
  (syntax-rules ()
    ((_ x y) (cons x (delay y)))))

(s-cons 'a 'b)

;; 遅延リストから値を取り出すのは以下の関数
(define s-car (lambda (x) (car x)))
(define s-cdr (lambda (x) (force (cdr x))))

(s-car (s-cons 'a 'b))
(s-cdr (s-cons 'a 'b))

;;;;;;;; B.6 継続 ;;;;;;;;
;;;; B.6.1 継続渡し形式(continuous passing style)
((lambda (k) (+ 3 k)) 5)

((lambda (x) (x 5)) (lambda (k) (+ 3 k)))

((lambda (x) (x 5)) (lambda (k) (* 3 k)))

;;;; B.6.2 継続の生成
(* 7 (call/cc (lambda (x) (x (+ 3 5)))))

;;;; B.6.3 継続の機能
(define opp/cc '())
(* 7 (call/cc (lambda (x) (set! opp/cc x) (x (+ 3 5)))))
(opp/cc 0)
(opp/cc 1)
(opp/cc (+ 3 5))

;; opp/cc 後に続く加算を無視している
(+ 1 (opp/cc 8)) ; => 56
(+ 1 ((lambda (k) (* 7 k)) 8)) ; => 57


(define (return x)
  x)

(define (k+ a b k)
  (k (+ a b)))

(define (k* a b k)
  (k (* a b)))

;;;; 継続がよく分からないので、ネットの情報で勉強 2021/02/22(月)
;; 継続渡しスタイル - umeajiのブログ
;; https://umeaji.hatenablog.com/entry/2019/09/17/220259
;; まずは普通の関数型で書く
(define (f) (* 3 (+ 1 2)))
(f)

;; これを CPS で書くと
(define (return x)
  x)

(define (k+ a b k)
  (k (+ a b)))

(define (k* a b k)
  (k (* a b)))

(k+ 1 2 (lambda (x) (k* x 3 return)))

;; Scheme 入門 16. 継続
;; http://www.shido.info/lisp/scheme_cc.html

