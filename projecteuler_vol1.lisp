 (defmacro timing (&body forms)
    (let ((real1 (gensym))
	    (real2 (gensym))
	    (run1 (gensym))
	    (run2 (gensym))
	    (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
	      (,run1 (get-internal-run-time))
	      (,result (progn ,@forms))
	      (,run2 (get-internal-run-time))
	      (,real2 (get-internal-real-time)))
	 (format t ";;; Computation took:~%")
	 (format t ";;;  ~f seconds of real time~%"
		 (/ (- ,real2 ,real1) internal-time-units-per-second))
	 (format t ";;;  ~f seconds of run time~%"
		 (/ (- ,run2 ,run1) internal-time-units-per-second))
	 ,result)))

(defun problem (x)
  (timing (funcall #'(intern (concatenate 'string
					  "problem"
					  (princ-to-string x))))))

;;; Code from sbcl 0.7.0
;;; Is X is a positive prime integer?
(defun positive-primep (x)
  ;; This happens to be called only from one place in sbcl-0.7.0, and
  ;; only for fixnums, we can limit it to fixnums for efficiency. (And
  ;; if we didn't limit it to fixnums, we should use a cleverer
  ;; algorithm, since this one scales pretty badly for huge X.)
  (declare (fixnum x))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (evenp x))
           (not (zerop (rem x 3)))
           (do ((q 6)
                (r 1)
                (inc 2 (logxor inc 6)) ;; 2,4,2,4...
                (d 5 (+ d inc)))
               ((or (= r 0) (> d q)) (/= r 0))
             (declare (fixnum inc))
             (multiple-value-setq (q r) (truncate x d))))))


;;; problems begin.

(defun problem1 ()
  (loop for i below 1000
       summing (if (or (= (mod i 3) 0) (= (mod i 5) 0)) i 0)))

(defun problem2 ()
  (loop for a = 0 then b
        and  b = 1 then (+ b a)
        summing (if (= (mod a 2) 0) a 0)
        while (<= a 4000000)))


(defun problem3 ()
  (let* ((sqrt-num 600851475143)
	 (loop-end (isqrt sqrt-num)))
    (loop for x from loop-end downto 2
	 until (and (zerop (mod sqrt-num x))
		    (if (<= x 5)
			(and (>= x 2) (/= x 4))
			(and (not (evenp x))
			     (not (zerop (rem x 3)))
			     (do ((q 6)
				  (r 1)
				  (inc 2 (logxor inc 6)) ;; 2,4,2,4...
				  (d 5 (+ d inc)))
				 ((or (= r 0) (> d q)) (/= r 0))
			       (declare (fixnum inc))
			       (multiple-value-setq (q r) (truncate x d))))))
	 finally (return x))))


(defun problem4 ()
  (flet ((generate-parlindrome (x)
	   (let* ((a (floor (/ x 100)))
		    (b (floor (mod (/ x 10) 10)))
		    (c (floor (mod x 10))))
	     (+ (* x 1000) (* c 100) (* b 10) a))))
    (loop for x from 999 downto 100
;;       with a = (floor (/ x 100))
;;       with b = (floor (mod (/ x 10) 10))
;;       with c = (floor (mod x 10))
;;       with v = (+ (* x 1000) (* c 100) (* b 10) a)
       until (let* ((v (generate-parlindrome x)))
	       (loop for y from (isqrt v) downto 100
		  thereis (and (zerop (mod v y))
			       (>= (/ v y) 100)
			       (<= (/ v y) 999))))
       finally (return (generate-parlindrome x)))))
		 

(defun problem5 ()
  (let ((v 1))
    (loop for x from 2 to 20
	 do (let ((z v))
	      (loop while (not (zerop (mod v x)))
		   do (setf v (+ v z)))))
    v))

(defun problem6 ()
  (let ((v 100))
    (abs (- (loop for x from 1 to v
	    summing (* x x))
       (expt (loop for x from 1 to v summing x) 2)))))

(defun problem7 ()
  (let ((target-count 10001)
	(prime-count 0)
	(final-value nil))
    (progn (incf prime-count)  ;; 2
	   (incf prime-count)  ;; 3
	   (incf prime-count)  ;; 5
	   (flet ((is-prime-p (x)
		    (do ((q 6)
			 (r 1)
			 (inc 2 (logxor inc 6)) ;; 2,4,2,4...
			 (d 5 (+ d inc)))
			((or (= r 0) (> d q)) (/= r 0))
		      (declare (fixnum inc))
		      (multiple-value-setq (q r) (truncate x d)))))
	     (loop for n from 6 by 6
		  do (progn (if (is-prime-p (+ n 1))
				(progn (incf prime-count)
				       (if (= prime-count target-count)
					   (setf final-value (+ n 1)))))
			    (if (is-prime-p (+ n 5))
				(progn (incf prime-count)
				       (if (= prime-count target-count)
					   (setf final-value (+ n 5))))))
		  until (>= prime-count target-count))))
    final-value))
		  
(defun problem8 ()
  (let* ((target-string
	  (concatenate 'string
		       "73167176531330624919225119674426574742355349194934"
		       "96983520312774506326239578318016984801869478851843"
		       "85861560789112949495459501737958331952853208805511"
		       "12540698747158523863050715693290963295227443043557"
		       "66896648950445244523161731856403098711121722383113"
		       "62229893423380308135336276614282806444486645238749"
		       "30358907296290491560440772390713810515859307960866"
		       "70172427121883998797908792274921901699720888093776"
		       "65727333001053367881220235421809751254540594752243"
		       "52584907711670556013604839586446706324415722155397"
		       "53697817977846174064955149290862569321978468622482"
		       "83972241375657056057490261407972968652414535100474"
		       "82166370484403199890008895243450658541227588666881"
		       "16427171479924442928230863465674813919123162824586"
		       "17866458359124566529476545682848912883142607690042"
		       "24219022671055626321111109370544217506941658960408"
		       "07198403850962455444362981230987879927244284909188"
		       "84580156166097919133875499200524063689912560717606"
		       "05886116467109405077541002256983155200055935729725"
		       "71636269561882670428252483600823257530420752963450"))
	 (target-string-length (length target-string))
	 (value (parse-integer target-string))
	 (match-length 5))
    (loop for i from target-string-length downto match-length
	 maximizing (flet ((calculate-value (n)
			   (let ((result-value 1))
			     (loop for j from 1 to match-length
				  do (progn
				       (setf result-value
					     (* result-value (mod n 10)))
				       (setf n (floor (/ n 10)))))
			     result-value)))
		    (calculate-value (mod (floor (/ value (expt 10
							 (- i match-length))))
					  (expt 10 match-length)))))))
		

(defun problem9 ()
  (let* ((sum-value 1000)
	 (c-max (expt (isqrt (/ sum-value 2)) 2))
	 (result-value nil))
    (loop for c from c-max downto 3
	 do (flet ((is-sqr (n)
		     (= n (expt (isqrt n) 2)))
		   (div (a b)
		     (floor (/ a b))))
	      (loop for a from (min (div (- sum-value c) 2) (- c 1)) downto 1
		   do (if (and (is-sqr (- (expt c 2) (expt a 2)))
			       (= sum-value (+ a c (isqrt (- (expt c 2)
						   (expt a 2))))))
;;			  (progn
;;			    (print (list a (- sum-value a c) c))
			  (setf result-value (* a (- sum-value a c) c))) ;;)
		   until result-value))
	 until result-value)
    result-value))

(defun problem10 ()
  (let ((target-limit 2000000)
	(prime-sum 0))
    (progn (incf prime-sum 2)  ;; 2
	   (incf prime-sum 3)  ;; 3
	   (incf prime-sum 5)  ;; 5
	   (flet ((is-prime-p (x)
		    (do ((q 6)
			 (r 1)
			 (inc 2 (logxor inc 6)) ;; 2,4,2,4...
			 (d 5 (+ d inc)))
			((or (= r 0) (> d q)) (/= r 0))
		      (declare (fixnum inc))
		      (multiple-value-setq (q r) (truncate x d)))))
	     (loop for n from 6 to target-limit by 6
		  do (progn (if (and (is-prime-p (+ n 1))
				     (<= (+ n 1) target-limit))
				(incf prime-sum (+ n 1)))
			    (if (and (is-prime-p (+ n 5))
				     (<= (+ n 5) target-limit))
				(incf prime-sum (+ n 5)))
			    ))))
    prime-sum))


(defun problem11 ()
  (let* ((data-table
	  #(
	    #(08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
	    #(49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
	    #(81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
	    #(52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
	    #(22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
	    #(24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
	    #(32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
	    #(67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
	    #(24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
	    #(21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
	    #(78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
	    #(16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
	    #(86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
	    #(19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
	    #(04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
	    #(88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
	    #(04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
	    #(20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
	    #(20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
	    #(01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)
	    ))
	 (data-table-dimension 20)
	 (calc-item-count 4))
    (labels ((calc-value (i j i_inc j_inc)
	     (let ((result-value 1))
		  (dotimes (x calc-item-count result-value)
		    (setf result-value (* result-value
					  (elt (elt data-table
						    (+ i (* i_inc x)))
					       (+ j (* j_inc x))))))))
	     (max-value-from-pos (i j)
;;	       (print (cons i j)) (print 
	       (max (if (<= i (- data-table-dimension calc-item-count))
			(calc-value i j 1 0)
			0)
		    (if (<= j (- data-table-dimension calc-item-count))
			(calc-value i j 0 1)
			0)
		    (if (and (<= i (- data-table-dimension calc-item-count))
			     (<= j (- data-table-dimension calc-item-count)))
			(calc-value i j 1 1)
			0)
		    (if (and (>= i (- calc-item-count 1))
			     (<=  j (- data-table-dimension calc-item-count)))
			(calc-value i j -1 1)
			0)))) ;; )
			      
      (loop for i from 1 to data-table-dimension
	   maximizing (loop for j from 1 to data-table-dimension
			 maximizing (max-value-from-pos (- i 1) (- j 1)))))))

(defun problem12 ()
  (let ((check-limit 500)
	(current-number 0))
	
    (flet ((check-factor-count (n)
	     (> (loop for j from (- (isqrt n) 1) downto 1
		   summing (if (zerop (mod n j)) (if (= (* j j) n) 1 2) 0))
		check-limit)))
      (loop for i from 1
	 do (incf current-number i)
	 until (check-factor-count current-number)
	 finally (return current-number)))))


(defun problem13 ()
  (let* ((check-limit-length 10)
	 (check-limit (expt 10 check-limit-length))
	 (sum-value (+    
		     37107287533902102798797998220837590246510135740250
		     46376937677490009712648124896970078050417018260538
		     74324986199524741059474233309513058123726617309629
		     91942213363574161572522430563301811072406154908250
		     23067588207539346171171980310421047513778063246676
		     89261670696623633820136378418383684178734361726757
		     28112879812849979408065481931592621691275889832738
		     44274228917432520321923589422876796487670272189318
		     47451445736001306439091167216856844588711603153276
		     70386486105843025439939619828917593665686757934951
		     62176457141856560629502157223196586755079324193331
		     64906352462741904929101432445813822663347944758178
		     92575867718337217661963751590579239728245598838407
		     58203565325359399008402633568948830189458628227828
		     80181199384826282014278194139940567587151170094390
		     35398664372827112653829987240784473053190104293586
		     86515506006295864861532075273371959191420517255829
		     71693888707715466499115593487603532921714970056938
		     54370070576826684624621495650076471787294438377604
		     53282654108756828443191190634694037855217779295145
		     36123272525000296071075082563815656710885258350721
		     45876576172410976447339110607218265236877223636045
		     17423706905851860660448207621209813287860733969412
		     81142660418086830619328460811191061556940512689692
		     51934325451728388641918047049293215058642563049483
		     62467221648435076201727918039944693004732956340691
		     15732444386908125794514089057706229429197107928209
		     55037687525678773091862540744969844508330393682126
		     18336384825330154686196124348767681297534375946515
		     80386287592878490201521685554828717201219257766954
		     78182833757993103614740356856449095527097864797581
		     16726320100436897842553539920931837441497806860984
		     48403098129077791799088218795327364475675590848030
		     87086987551392711854517078544161852424320693150332
		     59959406895756536782107074926966537676326235447210
		     69793950679652694742597709739166693763042633987085
		     41052684708299085211399427365734116182760315001271
		     65378607361501080857009149939512557028198746004375
		     35829035317434717326932123578154982629742552737307
		     94953759765105305946966067683156574377167401875275
		     88902802571733229619176668713819931811048770190271
		     25267680276078003013678680992525463401061632866526
		     36270218540497705585629946580636237993140746255962
		     24074486908231174977792365466257246923322810917141
		     91430288197103288597806669760892938638285025333403
		     34413065578016127815921815005561868836468420090470
		     23053081172816430487623791969842487255036638784583
		     11487696932154902810424020138335124462181441773470
		     63783299490636259666498587618221225225512486764533
		     67720186971698544312419572409913959008952310058822
		     95548255300263520781532296796249481641953868218774
		     76085327132285723110424803456124867697064507995236
		     37774242535411291684276865538926205024910326572967
		     23701913275725675285653248258265463092207058596522
		     29798860272258331913126375147341994889534765745501
		     18495701454879288984856827726077713721403798879715
		     38298203783031473527721580348144513491373226651381
		     34829543829199918180278916522431027392251122869539
		     40957953066405232632538044100059654939159879593635
		     29746152185502371307642255121183693803580388584903
		     41698116222072977186158236678424689157993532961922
		     62467957194401269043877107275048102390895523597457
		     23189706772547915061505504953922979530901129967519
		     86188088225875314529584099251203829009407770775672
		     11306739708304724483816533873502340845647058077308
		     82959174767140363198008187129011875491310547126581
		     97623331044818386269515456334926366572897563400500
		     42846280183517070527831839425882145521227251250327
		     55121603546981200581762165212827652751691296897789
		     32238195734329339946437501907836945765883352399886
		     75506164965184775180738168837861091527357929701337
		     62177842752192623401942399639168044983993173312731
		     32924185707147349566916674687634660915035914677504
		     99518671430235219628894890102423325116913619626622
		     73267460800591547471830798392868535206946944540724
		     76841822524674417161514036427982273348055556214818
		     97142617910342598647204516893989422179826088076852
		     87783646182799346313767754307809363333018982642090
		     10848802521674670883215120185883543223812876952786
		     71329612474782464538636993009049310363619763878039
		     62184073572399794223406235393808339651327408011116
		     66627891981488087797941876876144230030984490851411
		     60661826293682836764744779239180335110989069790714
		     85786944089552990653640447425576083659976645795096
		     66024396409905389607120198219976047599490197230297
		     64913982680032973156037120041377903785566085089252
		     16730939319872750275468906903707539413042652315011
		     94809377245048795150954100921645863754710598436791
		     78639167021187492431995700641917969777599028300699
		     15368713711936614952811305876380278410754449733078
		     40789923115535562561142322423255033685442488917353
		     44889911501440648020369068063960672322193204149535
		     41503128880339536053299340368006977710650566631954
		     81234880673210146739058568557934581403627822703280
		     82616570773948327592232845941706525094512325230608
		     22918802058777319719839450180888072429661980811197
		     77158542502016545090413245809786882778948721859617
		     72107838435069186155435662884062257473692284509516
		     20849603980134001723930671666823555245252804609722
		     53503534226472524250874054075591789781264330331690)))
    (flet ((div10 (n) (floor (/ n 10))))
      (loop while (>= sum-value check-limit)
	   do (setf sum-value (div10 sum-value))
	   finally (return sum-value)))))

(defun problem14 ()
  (let* ((buffer-size 1000000)
	 (check-limit (- 1000000 1))
	 (buffer (make-array buffer-size :initial-element nil))
	 (cur-max-item 1)
	 (cur-max-value 1))
    (labels ((query-value (n)
	     (if (< n buffer-size)
		 (progn (if (null (elt buffer n))
			    (setf (elt buffer n)
				  (if (evenp n)
				      (cons n (1+ (cdr (query-value (/ n 2)))))
				      (cons n (1+ (cdr (query-value (+ (* n 3)
								       1))))))))
			(elt buffer n))
		 (if (evenp n)
		     (cons n (1+ (cdr (query-value (/ n 2)))))
		     (cons n (1+ (cdr (query-value (+ (* n 3) 1)))))))))
      (setf (elt buffer 1) (cons 1 1))
      (loop for i from 2 to check-limit
	   do (let ((item (query-value i)))
		(if (> (cdr item) cur-max-value)
		    (progn (setf cur-max-item (car item))
			   (setf cur-max-value (cdr item))))))
      cur-max-item cur-max-value)))

(defun problem15 ()
  (let* ((buffer-border 20)
	 (buffer (make-array (list (1+ buffer-border) (1+ buffer-border))
			     :initial-element nil)))
    (setf (aref buffer buffer-border buffer-border) 1)
    (labels ((query-item (i j)
	     (if (null (aref buffer i j))
		 (setf (aref buffer i j)
		       (+ (if (< i buffer-border)
			      (query-item (1+ i) j)
			      0)
			  (if (< j buffer-border)
			      (query-item i (1+ j))
			      0))))
	     (aref buffer i j)))
      (query-item 0 0))))


(defun problem16 ()
  (let ((expt-value (expt 2 1000))
	(cur-sum 0))
    (loop while (not (zerop expt-value))
	 do (progn (incf cur-sum (mod expt-value 10))
		   (setf expt-value (floor (/ expt-value 10))))
	 finally (return cur-sum))))

(defun problem17 ()
  (let ((smallnum-alist '((zero  . 4)  (one   . 3)  (two  . 3)
			  (three . 5)  (four  . 4)  (five . 4)
			  (six   . 3)  (seven . 5)  (eight . 5)
			  (nine  . 4)  (ten   . 3)  (eleven . 6)
			  (twelve   . 6) (thirteen . 8) (fourteen  . 8)
			  (fifteen  . 7) (sixteen  . 7) (seventeen . 9)
			  (eighteen . 8) (nineteen . 8) (twenty    . 6)
			  (thirty   . 6) (forty    . 5) (fifty     . 5)
			  (sixty    . 5) (seventy  . 7) (eighty    . 6)
			  (ninety   . 6) (hundred  . 7) (thousand  . 8)
			  (and      . 3) (-        . 0))))
    (labels ((word-generator (n)
	       (cond
		 ((<= n 20) (list (car (nth n smallnum-alist))))
		 ((< n 100) (cons (car (nth (+ (floor (/ n 10)) 18)
					    smallnum-alist))
				  (if (zerop (mod n 10))
				      nil
				      (cons '- (word-generator (mod n 10))))))
		 ((< n 1000) (append (append (word-generator (floor (/ n 100)))
					     '(hundred))
				     (if (zerop (mod n 100))
					 nil
					 (cons 'and
					       (word-generator (mod n 100))))))
		 ((= n 1000) (list 'one 'thousand))
		 (t nil)))
	     (letter-count (l)
	       (reduce #'+ (mapcar #'(lambda (x)
				       (cdr (assoc x smallnum-alist))) l))))
      (loop for i from 1 to 1000
	 summing (letter-count (word-generator i))))))
	  
(defun problem18 (&optional (external-input nil external-input-p))
  (let* ((input-data (if external-input-p
			 external-input
			 #(
			   #(75)
			   #(95 64)
			   #(17 47 82)
			   #(18 35 87 10)
			   #(20 04 82 47 65)
			   #(19 01 23 75 03 34)
			   #(88 02 77 73 07 63 67)
			   #(99 65 04 28 06 16 70 92)
			   #(41 41 26 56 83 40 80 70 33)
			   #(41 48 72 33 47 32 37 16 94 29)
			   #(53 71 44 65 25 43 91 52 97 51 14)
			   #(70 11 33 28 77 73 17 78 39 68 17 57)
			   #(91 71 52 38 17 14 91 43 58 50 27 29 48)
			   #(63 66 04 68 89 53 67 30 73 16 69 87 40 31)
			   #(04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)
			   )))
	 (input-row-count (length input-data))
	 (current-line   (elt input-data (1- input-row-count))))
    (loop for n from (1- (1- input-row-count)) downto 0
	 do (let ((new-line (copy-seq (elt input-data n))))
	      (loop for p from (1- (length new-line)) downto 0
		   do (incf (elt new-line p)
			    (max (elt current-line p)
				 (elt current-line (1+ p)))))
	      (setf current-line new-line)))
    (elt current-line 0)))

(defun problem19 ()
  (labels ((day-of-week  (day month year)
    "Returns the day of the week as an integer.
Monday is 0."
    (nth-value
     6
     (decode-universal-time
      (encode-universal-time 0 0 0 day month year 0)
      0))))
    (loop for yy from 1901 to 2000
	 summing (loop for mm from 1 to 12
		      summing (if (= (day-of-week 1 mm yy) 6)
				  1
				  0)))))

(defun problem20 ()
  (labels ((fact (n)
	     (let ((result 1))
	       (loop for x from 2 to n
		    do (setf result (* result x))
		    finally (return result)))))
    (let ((number (fact 100)))
      (loop while (> number 0)
	   summing (mod number 10)
	   do (setf number (floor (/ number 10)))))))
	   
			 
  
    
