;;;;;;;; C.1 函数を作る ;;;;;;;
(define ++ (lambda (i) (+ i 1)))

(define -- (lambda (i) (- i 1)))

(define ** (lambda (a b) (expt a b)))

(define swap-test
  (lambda (a b)
    (let* ((dummy a) (a b) (b dummy))
      (** a b))))

(define adjust-of
  (lambda (x)
    (let ((digit 1000)
          (slide (if (positive? x) 1/2 -1/2)))
      (/ (truncate (+ slide (* digit x))) digit))))

;;;; C.1.1 数のリスト (p473)
(define iota
  (lambda (min max)
    (if (> min max)
        '()
        (cons min (iota (++ min) max)))))

(iota 1 9)

(iota -5 5)

(define iota-reverse
  (lambda (min max)
    (if (> min max)
        '()
        (cons max (iota-reverse min (-- max))))))

(iota-reverse 1 9)

(iota-reverse -5 5)

;; 名前つき let (p475)
(define iota
  (lambda (min max)
    (let iota-loop ((i max) (tmp '()))
      (if (< i min)
          tmp
          (iota-loop (-- i) (cons i tmp))))))

(define iota-reverse
  (lambda (min max)
    (let iota-loop ((i min) (tmp '()))
      (if (> i max)
          tmp
          (iota-loop (++ i) (cons i tmp))))))

;;;; C.1.2 選択肢のある iota (p478)
(define iota
  (lambda (max . opt)
    (let* ((min (if (null? opt) 1 (car opt)))
           (step (if (or (null? opt) (null? (cdr opt)))
                     1 (cadr opt)))
           (dummy max)
           (max (if (null? opt) max min))
           (min (if (null? opt) min dummy)))
      (let loop ((i (- min step)) (tmp '()))
        (if (< (- max step) i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of (+ i step)) tmp)) )))))
           
;; 引数の処理をさらに工夫した iota の別定義
(define iota
  (lambda lst
    (let* ((x (length lst))
           (max (if (= 1 x) (car lst) (cadr lst)))
           (min (if (< 1 x) (car lst) 1))
           (step (if (= 3 x) (caddr lst) 1)))
      (let loop ((i (- min step)) (tmp '()))
        (if (< (- max step) i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of (+ i step)) tmp)) )))))

;;;; C.1.3 再帰による加算・乗算 (p481)
(define succ (lambda (list) (cons 1 list)))
(define pred (lambda (list) (cdr list)))

;; 加算 plus
(define plus
  (lambda (x y)
    (if (null? y)
        x
        (succ (plus x (pred y))))))

(plus '(1 1) '(1 1 1))

;; 乗算 mult
(define mult
  (lambda (x y)
    (if (null? y)
        '()
        (plus x (mult x (pred y))))))

;; 冪乗 pows
(define pows
  (lambda (x y)
    (if (null? y)
        '(1)
        (mult x (pows x (pred y))))))

;; 無限集合との関係
(define kakko
  (lambda (i)
    (if (zero? i)
        (list)
        (append (list (kakko (-- i)))
                (kakko (-- i))))))

(kakko 0)

(kakko 1)

(kakko 2)

(kakko 3)

;;;;;;;; C.2 ループ不変表明 ;;;;;;;
;; まずは 再帰による加算、乗算、冪乗のコードを通常の演算記号 +, *, を用いて書き直す
;; 加算 plus
(define plus
  (lambda (x y)
    (if (zero? y)
        x
        (+ 1 (plus x (- y 1))))))

;; 乗算 mult
(define mult
  (lambda (x y)
    (if (zero? y)
        0
        (+ x (mult x (- y 1))))))

;; 冪乗 pows
(define pows
  (lambda (x y)
    (if (zero? y)
        1
        (* x (pows x (- y 1))))))

;; 累積変数 p を導入する (p487)
;; ひとつ前の再帰呼出による方法では使用するメモリー量や計算時間が不明確である。
;; 累積変数を用いた方法では、それがより明確になる。
;; 加算の末尾再帰形式
(define plus-iter
  (lambda (x y p)
    (if (zero? y)
        (+ x p)
        (plus-iter x (- y 1) (+ p 1)))))

(plus-iter 2 3 0)
;; 上記の方法を末尾再帰というらしい。
;; だが、よく分からないので、末尾再帰について少しネットで調べる。-> org に調査結果を

;; 以下は外側を二変数関数で包んで使いやすくした版
(define plus-tailrec
  (lambda (x y)
    (define puls-iter
      (lambda (x y p)
        (if (zero? y)
            (+ x p)
            (plus-iter x (- y 1) (+ p 1)))))
    (plus-iter x y 0)))

(plus-tailrec 2 3)

;; mult の末尾再帰形式
(define mult-tailrec
  (lambda (x y)
    (define mult-iter
      (lambda (x y p)
        (if (zero? y)
            p
            (mult-iter x (- y 1) (+ x p)))))
    (mult-iter x y 0)))

(mult-tailrec 3 5)

;; pows の末尾再帰形式
(define pows-tailrec
  (lambda (x y)
    (define pows-iter
      (lambda (x y p)
        (if (zero? y)
            p
            (pows-iter x (- y 1) (* x p)))))
    (pows-iter x y 1)))

(pows-tailrec 2 10)

;;;; C.2.2 逐次平方による冪乗計算
(define sq
  (lambda (x)
    (* x x)))

(define sq**
  (lambda (b n)
    (cond ((zero? n) 1)
          ((even? n) (sq (sq** b (/ n 2))))
          ((odd? n) (* b (sq** b (- n 1)))))))

;; 末尾再帰への変換 (p492)
(define sq**-tailrec
  (lambda (b n)
    (define sq**-iter
      (lambda (b n p)
        (cond ((zero? n) p)
              ((even? n) (sq**-iter (sq b) (/ n 2) p))
              ((odd? n) (sq**-iter b (- n 1) (* b p))))))
    (sq**-iter b n 1)))

;;;; C.2.3 階乗の計算 (p494)
(define fact
  (lambda (n)
    (if(zero? n)
       1
       (* n (fact (- n 1))))))

;; 末尾再帰版
(define fact-tailrec
  (lambda (n)
    (define fact-iter
      (lambda (y p)
        (if (zero? y)
            p
            (fact-iter (- y 1) (* y p)))))
    (fact-iter n 1)))

;; 繰り返しの書法 (p495)
;; 先程の末尾再帰版の階乗計算を名前つき let を用いて記述する
(define fact-let
  (lambda (n)
    (let countdown ((y n) (p 1))
      (if (zero? y)
          p
          (countdown (- y 1) (* y p))))))

;; 増加方向に y を変化させる方法では
(define fact-let+
  (lambda (n)
    (let countup ((y 1) (p 1))
      (if (> y n)
          p
          (countup (+ y 1) (* y p))))))

;; 特殊形式 do を用いれば、以下のように書ける
(define fact-do
  (lambda (n)
    (do ((y n (- y 1)) (p 1 (* y p)))
        ((zero? y) p))))

;;;;;;;; C.3 リストを調べる ;;;;;;;
;;;; C.3.1 恒等関数 (p496)
;; 以下はリストを受け取って、cdr-down(分解) し、cons-up(構築) 再構築する関数。
;; これ自体は何の意味もないが、これから出てくる内容の基礎となる。
;; これをリストを操る手続きの雛形として使う。
(define id-updown
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (id-updown (cdr lst))))))

;; 以下が各種リスト操作手続き
;; (define append
;;   (lambda (lst lst+)
;;     (if (null? lst)
;;         lst+
;;         (cons (car lst)
;;               (append (cdr lst) lst+)))))

(define length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))

;; 最初の n 要素を除いた部分リストの取得
(define list-tail
  (lambda (lst n)
    (if (zero? n)
        lst
        (list-tail (cdr lst) (- n 1)))))

;; 第 n 要素の取得
(define list-ref
  (lambda (lst n)
    (if (zero? n)
        (car lst)
        (list-ref (cdr lst) (- n 1)))))

;; 最初の n 要素を取得
(define list-head
  (lambda (lst n)
    (if (zero? n)
        '()
        (cons (car lst)
              (list-head (cdr lst) (- n 1))))))

;; これらを組合せて、末尾要素をペアとして取り出す手続き last-pair を定義する
(define last-pair
  (lambda (lst)
    (list (list-ref lst (- (length lst) 1)))))

;; リストの要素を逆順に並べる
(define reverse
  (lambda (lst)
    (if (null? lst)
        '()
        (append (reverse (cdr lst))
                (list (car lst))))))

;;;; C.3.3 二重再帰 (p500)
;; id-updown では cdr 部を分解していった。
;; だが以下のような car 部がリストになったリストを扱うと、
;; car 部は分解されないままになる。
;; '(((1 2 ) 3) 4)
;; そこで 次に示すような id-all を考える
(define nonpair?
  (lambda (x)
    (not (pair? x))))

(define id-all
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (cons (id-all (car lst))
              (id-all (cdr lst))))))

;; length-all の 間違い版。'()も数えてしまった
(define length-all
  (lambda (lst)
    (if (nonpair? lst)
        1
        (+ (length-all (car lst))
           (length-all (cdr lst))))))

(define length-all
  (lambda (lst)
    (cond ((null? lst) 0)
          ((nonpair? lst) 1)
          (else (+ (length-all (car lst))
                   (length-all (cdr lst)))))))

;; reverse-all の間違い版。
(define reverse-all
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (append (reverse-all (cdr lst))
                (reverse-all (car lst))))))

(define reverse-all
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (append (reverse-all (cdr lst))
                (list (reverse-all (car lst)))))))

;; リストの平坦化

;; やってみたが、結局、id-all と同じものを書いてしまった 😟
;; cons するからダメなのか？ cons を入れる場所がまずいのか？
(define flatten
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (cons (flatten (car lst))
              (flatten (cdr lst))))))

;; 答えは...
(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((pair? lst) (append (flatten (car lst))
                               (flatten (cdr lst))))
          (else (list lst)))))

;; リスト内の括弧を一層分だけ除去する手続き
(define flat-in
  (lambda (lst)
    (if (null? lst)
        '()
        (append (car lst)
                (flat-in (cdr lst))))))

;;;;;;;; C.4 高階手続 (p506) ;;;;;;;
(iota 9)

(define num0-9 (iota 0 9))

(define num1-9 (iota 9))

num1-9

;;;; C.4.1 apply (p506)
(apply + num1-9)
(apply - num1-9)
(apply * num1-9)
(apply / num1-9)

;;;; C.4.2 map による手続の分配 (p508)
(map - num1-9)
(map / num1-9)

;; 対応する要素同士の計算 (p509)
(map list '(1 2) '(3 4) '(5 6))

;; 続いて数値計算
(map * '(1 2) '(3 4) '(5 6))

;; 以下のようにすれば冪乗を計算できる
;; 2乗
(map * num1-9 num1-9)

;; 3乗
(map * num1-9 num1-9 num1-9)

;; 以下のように語数を数える関数も高階関数を使って定義できる
(define check-of
  (lambda (k)
    (if (eq? k k) 1 0)))

(define words
  (lambda (n)
    (apply + (map check-of n))))

(define fruits
  (list 'apple 'orange 'kiwi 'grape 'tomato))

(words fruits)

;; map の再定義 (p511)
;; id-updown を参考にして、map を再定義することができる
(define map-unit
  (lambda (proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
              (map-unit proc (cdr lst))))))

;; この定義だとこれはできる
(map-unit - '(1 2 3 4 5))

;; だが、以下のような複数のリストの組合せの計算できない
;; (map-unit * '(1 2) '(3 4) '(5 6))

;; 複数のリストの組合せには以下の方法で対応できる
(define map-mult
  (lambda (proc rest)
    (if (member '() rest)
        '()
        ;; (map-unit car rest) で各々のリストの先頭要素を集めたリストを作る
        (cons (apply proc (map-unit car rest)) 
              (apply map proc (map-unit cdr rest))))))

;; これらをまとめて
(define map
  (lambda (proc . rest)
    (if (null? (cdr rest))
        (map-unit proc (car rest))
        (map-mult proc rest))))


(map even? '(1 2 3 4 5 6 7))
(map * '(2 3 5) '(7 11 13) '(17 19 23))
(map * '(2 3 5) '(7 11 13) '(17 19))

;; for-each は 結果をリストとして残さない map である (p512)
(define (for-each proc lst)
  (if (null? lst)
      '<unspecified>
      (begin (proc (car lst))
             (for-each proc (cdr lst)))))

;; 要素の取捨選択 (p513)
;; まず、先頭から n 要素が除かれたリストは、先に示した list-tail より
(list-tail num1-9 5)
(list-tail fruits 3)

;; また、第 n 要素を取り出す方法は
(list-ref num1-9 5)
(list-ref fruits 3)

;; 条件を満たす要素のみに篩にかける filter を考える
;; まずは自分で考えてみる (id-updown を参考にした)
(define filter
  (lambda (lst predi)
    (if (null? lst)
        '()
        (if (predi (car lst))
            (cons (car lst)
                  (filter (cdr lst) predi))
            (filter (cdr lst) predi)))))

;; 参考書に書いてあるもの
(define filter
  (lambda (predi lst)
    (cond ((null? lst) '())
          ((predi (car lst))
           (cons (car lst)
                 (filter predi (cdr lst) )))
          (else (filter predi (cdr lst))))))

(filter even? num0-9)
(filter odd? num0-9)

;; filter の処理判断を逆転させれば remove を定義できる
(define remove
  (lambda (predi lst)
    (cond ((null? lst) '())
          ((predi (car lst))
           (remove predi (cdr lst)))
          (else (cons (car lst)
                 (remove predi (cdr lst)))))))

(remove even? num0-9)
(remove odd? num0-9)

;; 以下の target 関数と組合せれば、条件を柔軟に指定できるようになる
(define target?
  (lambda (proc x)
    (lambda (y) (proc y x))))

(filter (target? = 5) num0-9)
(remove (target? = 5) num0-9)
(filter (target? < 5) num0-9)
(remove (target? < 5) num0-9)

;;;; C.4.3 map による手続の入れ子 (p514)
;; 二要素を単位として要素とするリストの生成
((lambda (i)
   ((lambda (j) (list i j)) '1))
 'a)
;; (a 1)

;; データをリストに変え、その適用に map を用いれば
(map (lambda (i)
   (map (lambda (j) (list i j)) '(1 2)))
     '(a b))
;; (((a 1) (a 2)) ((b 1) (b 2)))

;; これを解(ほぐ)して、二要素を単位とするリストを得るには
(apply append
       (map (lambda (i)
              (map (lambda (j)
                     (list i j)) '(1 2))) '(a b)))
;; ((a 1) (a 2) (b 1) (b 2))

;; 段々難しくなってきた... 参考書を写すのみ
;; 以下のようにリストを平坦化する手続きを append と map を組合せて作れる
(define flatmap
  (lambda (proc lst)
    (apply append (map proc lst))))

(flatmap (lambda (i)
           (map (lambda (j)
                  (list i j)) '(1 2))) '(a b))

;; 多くの問題において、大小関係が規定された数のリストが必要となる
;; 以下は指定した自然数 n と n より小さい自然数からなるリストを構成する
(define (double n)
  (apply append
         (map (lambda (i)
                (map (lambda (j) (list i j))
                     (iota (- i 1)) ))
              (iota n))))

(double 4)
;; output ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))
;; 参考書(p516)通りにやったつもりだが、なぜか実行結果がエラーになる...
;; -> 273行目あたりで定義した append が有効になっていたのが原因だったようだ
;;    それを無効にして、組込みの append を参照するようにしたらエラー出なくなった

;; 2乗
(define sq (lambda (x) (* x x)))

;; 2引数の二乗和
(define sq+ (lambda (i j) (+ (sq i) (sq j))))

;; 三平方の定理の解のヒントとなるようなリストを生成する手続き
(define (triple n)
  (apply append
         (map (lambda (i)
                (map (lambda (j)
                       (list (sq+ i j) i j))
                     (iota (- i 1)) ))
              (iota n) )))

(triple 4)
;; output ((5 2 1) (10 3 1) (13 3 2) (17 4 1) (20 4 2) (25 4 3))
;; 先頭要素が自然数 5 の二乗である (25 4 3) がピタゴラス数 (5 4 3) の存在を示す

;; 二重リストに対して、内部の各要素に手続きを分配するには
;; 以下の手続きを使う
(define dismap
  (lambda (proc dlst)
    (map (lambda (x) (map proc x)) dlst)))

(dismap sq '((2 3) (5 7)))

(dismap (lambda (x) (adjust-of (sqrt x))) '((2 3) (5 7)))

;;;; C.4.4 要素の並べ方 (p517)
;; 順列を考える上で、まずはリストからある要素を削除する手続きを考える。
;; 参考書より、以下、書き写した。やはり、「継続」の概念がよく分からない...
(define del-obj
  (lambda (lst obj)
    (call/cc
     (lambda (k)
       ((cond ((null? lst) '())
              ((equal? (car lst) obj) (k (cdr lst)))
              (else (cons (car lst)
                          (del-obj (cdr lst) obj)))))))))

(del-obj '(1 2 3 4) 1)
;; output (2 3 4)

;; この del-obj を用いれば、先程の map の入れ子形式と合わせて、permutation (順列) を定義できる

;; まずは自分で考えてみる
;; う~ん、ちょっと全然手が進まない
;; (define permutation
;;   (lambda (lst)
;;     )

;; 参考書を写す
(define (permutations lst)
  (if (null? lst)
      (list '())
      (apply append
             (map (lambda (i)
                    (map (lambda (j) (cons i j))
                         (permutations (del-obj lst i))))
                  lst))))

(permutations '(1 2 3))
;; なぜか、エラー終了してしまう...

(length (permutations '(1)))
;; ここから下はエラーになる...
(length (permutations '(1 2)))
(length (permutations '(1 2 3)))
(length (permutations '(1 2 3 4)))
(length (permutations '(1 2 3 4 5)))

;; 先程は、実際に順列を求めて、その要素数から順列の組合せ数を求めた。
;; 次は式計算で求める。perm, comb, rept
(define perm
  (lambda (n r)
    (cond ((= r 0) 1)
          ((= r 1) n)
          (else (* n (perm (- n 1) (- r 1)))))))

(define comb
  (lambda (n r)
    (cond ((= r 0) 1)
          ((= r n) 1)
          (else (+ (comb (- n 1) (- r 1))
                   (comb (- n 1) r))))))

(perm 4 0)
(perm 4 1)
(perm 4 2)
(perm 4 3)
(perm 4 4)

(comb 4 0)
(comb 4 1)
(comb 4 2)
(comb 4 3)
(comb 4 4)

;; perm と comb は自分で考えた。重複組合せrept は参考書を写す
(define rept
  (lambda (n r)
    (if (or (= r 0) (= n 1))
        1
        (+ (rept n (- r 1))
           (rept (- n 1) r)))))


;; また、順列を用いて、組合せ、重複組合せ、階乗を定義できる
(define comb
  (lambda (n r)
    (/ (perm n r)
       (perm r r))))

(define rept
  (lambda (n r)
    (/ (perm (- (+ n r) 1) r)
       (perm r r))))

(define !n (lambda (n) (perm n n)))

(!n 4)
;; output 24

;;;; C.4.5 リストによる数値計算 (p521)
;; 引数が偶数なら +1,奇数なら -1 を返す parity-of を定義する
(define parity-of
  (lambda (n)
    (if (even? n)
        1
        -1)))

;; par1-9 は 偶奇を +1, -1 で表わしたリストになる
(define par1-9 (map parity-of num1-9))

;; このリストを足し合わせると、偶数なら 0、奇数なら -1 になる
(apply + par1-9)

;; 確定数と限定数
;; まずは二乗の逆数のリストを作る
(map / (map * num1-9 num1-9))
;; (1 1/4 1/9 1/16 1/25 1/36 1/49 1/64 1/81)

;; これらの総和を求める
(apply + (map / (map * num1-9 num1-9)))
;; out (1 1/4 1/9 1/16 1/25 1/36 1/49 1/64 1/81)

;; 確定数を限定数に変換するには
(exact->inexact (apply + (map / (map * num1-9 num1-9))))
;; out 1.5397677311665408

;; 以下でもよい
(* 1.0 (apply + (map / (map * num1-9 num1-9))))
;; out 1.5397677311665408

;; ゼータの値を求める (p522)
;; ゼータ関数は自然数の逆羃の和によって定義される。
;; ここでは最初の9項の和として求める

(* 1.0 (apply + (map / (map * num1-9 num1-9))))
;; out 1.5397677311665408

(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9))))
;; out 1.1965319856741932

(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9 ))))
;; out 1.0819365834937567

(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9 num1-9))))
;; out 1.0368973413446934

(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9 num1-9 num1-9))))
;; out 1.0173405124414314

;; 別のアプローチ。引数を -n 乗(ここでは-6乗)する手続きを定義し、
;; それを各要素に適用するという方法でも計算できる。
(define exp-6 (lambda (x) (** x -6)))

(* 1.0 (apply + (map exp-6 num1-9)))
;; out 1.0173405124414314

;;;; C.4.6 総和と積 (p523)
;; 項毎に符号の変わる数値を加算する交代級数を考える。
;; そのためには逆数リストと parity リストをで掛け合わせ、その後に apply を取ればよい
;; ここでは第9項までの和を求める
(begin
  (define num-x (iota 9))
  (define parity-of (lambda (p) (if (odd? p) -1 1)))
  (define parity-x (map parity-of num-x))
  (define terms (map / (map * (map - parity-x) num-x)))
  (* 1.0 (apply + terms)))

;; out 0.7456349206349207

;; この級数の和は ln2 である
(log 2)
;; 0.6931471805599453

;; 第100項までの和を求める。第9項までの場合より近似精度がよくなる。
(begin
  (define num-x (iota 100))
  (define parity-of (lambda (p) (if (odd? p) -1 1)))
  (define parity-x (map parity-of num-x))
  (define terms (map / (map * (map - parity-x) num-x)))
  (* 1.0 (apply + terms)))
;; out 0.6881721793101953

;; 総和記号のコード化 (p525)
;; 交代級数を一般項より求める。
(define prototype
  (lambda (n k)
    (if (> n k)
        0
        (+ (/ 1.0 (* (** -1 (- n 1)) n))
           (prototype (++ n) k)))))

;; 上記の手続きより、総和部分を抽象化して取り出すと
(define sum
  (lambda (initial final body)
    (if (> initial final)
        0
        (+ (body initial)
           (sum (++ initial) final body)))))

;; body 部分となるような手続きを定義する
(define log2
  (lambda (n) (/ (** -1 (- n 1)) n)))

;; これらを用いて
(* 1.0 (sum 1 1000 log2))
;; out 0.6926474305598204

;; 無限級数と無限乗積 (p527)
;; これだけの準備をすれば、あとは公式集にある一般項を sum に与えてやれば、
;; 級数を求められる。
;; ライプニッツの級数は π/4 となることが知られている。
;; その一般項を手続き leibniz として定義する
(define leibniz
  (lambda (n) (/ (** -1 n) (+ (* 2 n) 1))))

(* 4.0 (sum 0 1000 leibniz))
;; out 3.142591654339543

;; またゼータ関数 ζ(2) を以下に定義する
(define zeta2
  (lambda (n) (/ (** n 2))))

(* 1.0 (sum 1 1000 zeta2))
;; out 1.64393456668156

;; また、無限乗積についても無限級数同様、抽象化手続きを定義する
(define product
  (lambda (initial final body)
    (if (> initial final)
        1
        (* (body initial)
           (product (++ initial) final body)))))

(define pi/4
  (lambda (n)
    (* (/ (* 2 n) (+ (* 2 n) 1))
       (/ (+ (* 2 n) 2) (+ (* 2 n) 1)))))

(* 4.0 (product 1 1000 pi/4))
;; out 3.142377365093878

;; product を用いて階乗の計算もできる
(define fact
  (lambda (i)
    (if (= i 0)
        1
        i)))

(product 0 10 fact)
;; out 3628800

;; accumulate (p528)
;; sum、product には「何かを集める」という似通った部分がある。
;; そこを accumulate として抽象化し、sum、product を再定義する。
(define accumulate
  (lambda (op ini seqs)
    (if (null? seqs)
        ini
        (op (car seqs)
            (accumulate op ini (cdr seqs))))))

(define sum
  (lambda (ini fin body)
    (accumulate + 0 (map body (iota ini fin)))))

(define product
  (lambda (ini fin body)
    (accumulate * 1 (map body (iota ini fin)))))

;; これらを用いて、0 から 100 までの自然数の和 を求める
(define num (lambda (i) i))
(sum 0 100 num)
;; out 5050

;; ネイピア数 (p529)
;; 階乗計算と総和の手続きを組合わせてネイピア数を求める。
(define napier
  (lambda (n)
    (/ 1 (product 0 n fact))))

(* 1.0 (sum 0 100 napier))
;; out 2.718281828459045

;; 冪乗数を抽出する (p530)
;; 与えられたリストの中から冪乗数を抽出することを考える。フィルタリングである
;; そのために、まず判定用の述語を定義する。まずは2乗羃の判定用の述語
(define pow2?
  (lambda (x)
    (let ((y (inexact->exact (round (** x 1/2)))))
      (if (= x (** y 2))
          #t
          #f))))

(filter pow2? (iota 100))

;; 3乗羃も同様にして
(define pow3?
  (lambda (x)
    (let ((y (inexact->exact (round (** x 1/3)))))
      (if (= x (** y 3))
          #t
          #f))))

(filter pow3? (iota 1000))

;;;; C.4.7 文字と数字 (p531)
;; 数を数字に変換することを考える
;; 以下の手続きで数字の ASCII コードを取得できる
(char->integer #\0)
;; out 48

;; よって、数字への変換は
(define trans48
  (lambda (x) (+ x 48)))

(define ten (iota 0 9))
(map trans48 ten)
;; out (48 49 50 51 52 53 54 55 56 57)

;; これを数値を文字に変換する組込手続きに渡すと
(map integer->char (map trans48 ten))
;; out (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)

;; これらと、文字リストを文字列に変換する組込手続 list->string を組合せて
(define digit->string
  (lambda (lst)
    (list->string
     (map integer->char (map trans48 lst)))))

(digit->string ten)

;; 文字を数値に (p533)
;; 次は文字の数値化を試みる
(char->integer #\A)
;; out 65
(char->integer #\Z)
;; out 90

;; 一方、小文字は 97~122 である。
;; つまり、大文字コード +32 で小文字に変換できる
(define ul-exchange
  (lambda (charactor)
    (let ((x (char->integer charactor)))
      (cond ((and (<= 65 x) (>= 90 x))
             (integer->char (+ x 32)))
            ((and (<= 97 x) (>= 122 x))
             (integer->char (- x 32)))
            (else 'again!)))))

(ul-exchange #\A)
;; out #\a

;; シーザー暗号 (p534)
;; これだけ道具が揃えば、9.4節(p318)のシーザー暗号の符号化、復号化もできる。
;; まずは符号化
(define shift-encode
  (lambda (str k)
    (define (e-engine lst k)
      (if (null? lst)
          '()
          (cons (+ k (char->integer (car lst)))
                (e-engine (cdr lst) k))))
    (e-engine (string->list str) k)))

(shift-encode "Cross the Rubicon!" 3)

;; 次に復号化。まずは自分で考えてみる
(define shift-decode
  (lambda (e-lst k)
    (define (d-engine lst k)
      (if (null? lst)
          '()
          (cons (integer->char (+ k (car lst)))
                (d-engine (cdr lst) k))))
    (list->string (d-engine e-lst k))))

(shift-decode (shift-encode "Cross the Rubicon!" 3) -3)
;; out "Cross the Rubicon!"

;; これらは map を用いれば簡潔に書ける。
(define map-encode
  (lambda (str k)
    (map (lambda (x) (+ x k))
         (map char->integer (string->list str)))))

(map-encode "Cross the Rubicon!" 3)

(define map-decode
  (lambda (num k)
    (list->string (map integer->char
                       (map (lambda (x) (+ x k)) num)))))

(map-decode (map-encode "Cross the Rubicon!" 3) -3)
