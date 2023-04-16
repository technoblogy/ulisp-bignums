; Examples for the uLisp bignums Extension
; see http://www.ulisp.com/show?282B


; Factorial

(defun $fact (n)
  (let (($result ($bignum 1)))
  (dotimes (i n $result)
    (setq $result ($* $result ($bignum (1+ i)))))))

; Square root

(defun $sqrt ($a)
  (let* (($1 ($bignum 1))
         ($high $a)
         ($low ($bignum 0))
         ($mid ($+ ($ash $high -1) $1)))
    (loop
     (unless ($> $high $low) (return))
     (if ($> ($* $mid $mid) $a)
         (setq $high ($- $mid $1))
       (setq $low $mid))
     (setq $mid ($+ ($+ $low ($ash ($- $high $low) -1)) $1)))
    $low))

; Integer exponent

(defun $expt (x y)
  (let (($e ($bignum 1))
        ($f ($bignum x)))
    (loop
     (when (zerop y) (return $e))
     (when (oddp y) (setq $e ($* $e $f)))
     (setq $f ($* $f $f) y (ash y -1)))))

; Print the first 16 Mersenne primes

(defun mersenne ()
  (dolist (m '(2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203))
    (let (($p ($- ($expt 2 m) ($bignum 1))))
      (format t "~4a ~a~%" m ($bignum-string $p)))))

; Pollard's Rho Algorithm for factorising large numbers

(defun $gcd (a b)
  (let (temp)
    (loop
     (when ($zerop b) (return a))
     (setq temp b b ($mod a b) a temp))))

(defun $pollard-rho (n)
  (let* (($1 ($bignum 1))
         (x ($bignum 2))
         (y ($bignum 2))
         (d ($bignum 1))
         ($g (lambda (x) ($mod ($+ ($* x x) $1) n))))
    (loop
     (unless ($= d $1) (return))
     (setq x ($g x))
     (setq y ($g ($g y)))
     (setq d ($gcd (if ($> x y) ($- x y) ($- y x)) n)))
    (if ($= d n) nil d)))

(defun $fermat (n) ($+ ($expt 2 (expt 2 n)) ($bignum 1)))


