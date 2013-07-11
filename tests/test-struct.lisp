(begin 
	(define a 
		(create-struct '(atributo1 atributo2 atributo3))
		)
	(define b (set-attr! a 'atributo1 (+ 2 1)))
	(define fb (get-attr b 'atributo1))
	(set-attr! b 'atributo5 "Um erro deve ocorrer.")          
	)