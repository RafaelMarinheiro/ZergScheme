(begin
	(define gcd
		(lambda (a b)
			(if (eqv? b 0) a
				(gcd b (mod a b))
				)
			)
		)
	(gcd 12 16)
	)