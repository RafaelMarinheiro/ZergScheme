(begin
	(define isDivisor
		(lambda (d n)
			(if (eqv? (mod n d) 0) #t
				#f
				)
			)
		)
	(define primeAux
		(lambda (c n)
			(if (lt? n (* c c)) #t
				(if (isDivisor c n) #f
					(primeAux (+ c 1) n)
					)
				)
			)
		)
	(define isPrime
		(lambda (n) (primeAux 2 n))
		)
	(isPrime 1009)
	)