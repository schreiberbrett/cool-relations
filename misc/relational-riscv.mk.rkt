#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

;; Consider the following simple RISC-V code which computes x0 = 2*(x1 + x2)
#;(add x0 x1 x2)
#;(add x0 x0 x0)

;; We can say that it is a basic block composed of two smaller basic blocks appended together.
;; But suppose we had some metadata added to the basic block that states 