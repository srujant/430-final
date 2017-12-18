#lang racket

(require "utils.rkt")
(require "desugar.rkt")
(require "cps.rkt")
(require "closure-convert.rkt")
(require "top-level.rkt")

(provide create-binary)

(define (create-binary e)
	(proc->llvm (closure-convert (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar (top-level e)))))))))
)