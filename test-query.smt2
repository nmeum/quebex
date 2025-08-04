; success
(set-option :produce-models true)
; success
(set-logic QF_BV)
; success
(declare-fun cond1 () (_ BitVec 32))
; success
(declare-fun cond2 () (_ BitVec 32))
; success
(pop 0)
; success
(push 1)
; success
(assert (= cond1 #b00000000000000000000000000000000))
; success
(check-sat)
; sat
(get-value (cond1 cond2))
; ((cond1 #x00000000) (cond2 #x00000000))
(pop 1)
; success
(push 1)
; success
(assert (not (= cond1 #b00000000000000000000000000000000)))
; success
(push 1)
; success
(assert (= cond2 #b00000000000000000000000000000000))
; success
(check-sat)
; sat
(get-value (cond1 cond2))
; ((cond1 #x00000001) (cond2 #x00000000))
(pop 2)
; success
(push 1)
; success
(assert (= cond1 #b00000000000000000000000000000000))
; success
(push 1)
; success
(assert (not (= cond2 #b00000000000000000000000000000000)))
; success
(check-sat)
; sat
(get-value (cond1 cond2))
; ((cond1 #x00000000) (cond2 #x00000001))
