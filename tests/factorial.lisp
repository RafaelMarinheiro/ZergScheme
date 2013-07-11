(begin
	(define factorial
		(lambda (n)
			(if (eqv? n 0) 1
				(* n (factorial (- n 1)))
				)
			)
		)
	(factorial 6)
	)

; Testando
; * Recursao
; * if 
; * eqv?
; * Aplicação de Funcao
