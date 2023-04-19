; Arbitrary Precision uLisp Extension - Tests
; See http://www.ulisp.com/show?282B

(defun aeq (tst x y)
  (unless (equal x y)
    (incf ers)
    (format t \"~a=~a/~a~%\" tst x y)))

(defun $expt (x y)
  (let (($e ($bignum 1))
        ($f ($bignum x)))
    (loop
     (when (zerop y) (return $e))
     (when (oddp y) (setq $e ($* $e $f)))
     (setq $f ($* $f $f) y (ash y -1)))))

#| bignum-string |#

(aeq '$bignum-string "2147483647" ($bignum-string '(2147483647)))
(aeq '$bignum-string "2147483648" ($bignum-string '(-2147483648)))
(aeq '$bignum-string "1000000000" ($bignum-string '(1000000000)))
(aeq '$bignum-string "7fffffff" ($bignum-string '(2147483647) 16))
(aeq '$bignum-string "80000000" ($bignum-string '(-2147483648) 16))
(aeq '$bignum-string "3b9aca00" ($bignum-string '(1000000000) 16))

#| Multiply |#

(aeq '$* "4764984380238568507752444984131552966909" ($bignum-string ($* ($string-bignum "66405897020462343733") ($string-bignum "71755440315342536873"))))

#| Divide |#

(aeq '$/ "71755440315342536873" ($bignum-string ($/ ($string-bignum "4764984380238568507752444984131552966909") ($string-bignum "66405897020462343733"))))
(aeq '$/ "3203431780337" ($bignum-string ($/ ($string-bignum "576460752303423487") ($string-bignum "179951"))))

#| Mod |#

(aeq '$mod "3487" ($bignum-string ($mod ($string-bignum "576460752303423487") ($string-bignum "18000"))))
(aeq '$mod 7 ($integer ($mod ($string-bignum "576460752303423487") ($bignum 10))))

#| $expt |#

(aeq '$expt "131071" ($bignum-string ($- ($expt 2 17) ($bignum 1))))
(aeq '$expt "524287" ($bignum-string ($- ($expt 2 19) ($bignum 1))))
(aeq '$expt "2147483647" ($bignum-string ($- ($expt 2 31) ($bignum 1))))
(aeq '$expt "2305843009213693951" ($bignum-string ($- ($expt 2 61) ($bignum 1))))

#| Comparisons |#

(aeq '$< t ($< '(1) '(0 0 0 1)))
(aeq '$< t ($< '(1 0 0 0) '(0 0 0 1)))
(aeq '$= t ($= '(1 0 0 0) '(1)))
(aeq '$> nil ($> '(#xffffffff) '(0 1)))

#| Logical |#

(aeq '$logior "ffffffffffffffff" ($bignum-string ($logior ($string-bignum "aaaaaaaaaaaaaaaa" 16) ($string-bignum "5555555555555555" 16)) 16))
(aeq '$logxor "aaaaaaaaaaaaaaaa" ($bignum-string ($logxor ($string-bignum "ffffffffffffffff" 16) ($string-bignum "5555555555555555" 16)) 16))
(aeq '$logand "aaaaaaaa" ($bignum-string ($logand ($string-bignum "aaaaaaaaaaaaaaaa" 16) ($string-bignum "55555555ffffffff" 16)) 16))
(aeq '$logand "0" ($bignum-string ($logand ($string-bignum "0" 16) ($string-bignum "5555555555555555" 16)) 16))

#| Shifts |#

(aeq '$ash "1111222233334444" ($bignum-string ($ash ($string-bignum "11112222333344445555666677778888" 16) -64) 16))
(aeq '$ash "111122223333444455556666777788880000000000000000" ($bignum-string ($ash ($string-bignum "11112222333344445555666677778888" 16) 64) 16))