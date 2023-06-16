(use-modules (guix gexp)
             (guix store)
             (guix monads)
             (guix modules)
             (guix packages)
             (guix download)
             (guix derivations)
             (guix build-system trivial)
             (guix licenses)
             (gnu packages)
             (gnu packages base))

;; functor instance

;; (define (fmap f fa)
;;   (lambda (state)
;;     (let (;; create the derivation for ma "(a, s')"
;;           (v (runState store ma-name ma state)))
;;       #~(begin
;;           ;; TODO: apply some function on the main output to make a derivation
;;           ;; producing a new main output
;;           #$(f v)

;;           ;; carry the state through
;;           (mkdir #$output:state)
;;           (copy-recursively #$v:state #$output:state)))))

;; monad instance

(define (run-state store name ma s)
  (run-with-store store
    (mbegin %store-monad
      (gexp->derivation name (ma s)))))

;; gexp lifting form
(define (return x)
  (lambda (state)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          #$x

          (mkdir #$output:state)
          (copy-recursively #$state:state #$output:state)))))

(define (bind store ma ma-name fmb)
  (lambda (state)
    (let* (;; create the derivation for ma "(a, s')
           (v (run-state store ma-name ma state))
           ;; (x (derivation-output-path (assoc-ref (derivation-outputs v) "out")))
           ;; (s (derivation-output-path (assoc-ref (derivation-outputs v) "state")))
           )
      ;; run fmb with the derivation v to get a gexp, which would produce a
      ;; derivation building (b, s). that gexp is parameterized by v through
      ;; which it can modify both
      ((fmb v) v))))

(define (get)
  (lambda (state)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (mkdir #$output)
          (copy-recursively #$state:state #$output)

          (mkdir #$output:state)
          (copy-recursively #$state:state #$output:state)))))

(define (put x)
  (lambda (state)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (mkdir #$output)

          (mkdir #$output:state)
          (copy-recursively #$x:out #$output:state)))))

;; examples

(define store (open-connection))

(define initial-state-deriv
  (run-with-store store
    (mbegin %store-monad
      (gexp->derivation "initial-state"
                        #~(begin
                            (mkdir #$output)
                            (mkdir #$output:state)
                            (call-with-output-file (string-append #$output:state "/state")
                              (lambda (p)
                                (display 0 p))))))))

(define new-state-deriv
  (run-with-store store
    (mbegin %store-monad
      (gexp->derivation "new-state"
                        #~(begin
                            (mkdir #$output)
                            (mkdir #$output:state)
                            (call-with-output-file (string-append #$output:state "/state")
                              (lambda (p)
                                (display 3 p))))))))

;; (define (increment a num)
;;   #~(begin
;;       (mkdir #$output)
;;       (call-with-output-file (string-append #$output "/state")
;;         (lambda (p)
;;           (display 3 p)))))

(define act12
  (bind store (get) "act1"
        (lambda (s) (put new-state-deriv))))

(define act12-deriv (run-state store "act12" act12 initial-state-deriv))

(define act123
  (bind store
        act12
        "act2"
        (lambda (s) (return #~(begin
                                (mkdir #$output)
                                (call-with-output-file (string-append #$output "/out.txt")
                                  (lambda (p)
                                    (display "hello" p))))))))

(define act123-deriv (run-state store "act123" act123 initial-state-deriv))


;; initial-state-deriv
;; new-state-deriv
;; act12-deriv
act123-deriv
