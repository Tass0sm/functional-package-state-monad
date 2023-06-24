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
             (gnu packages base)

             (ice-9 match))

(define (stateful-package-return value)
  "My Return"
  (lambda (state)
    (with-monad %store-monad
      (return (cons value state)))))

(define (stateful-package-bind mvalue mproc)
  "Bind MVALUE, a value in the state monad, and pass it to MPROC."
  (lambda (state)
    (mlet* %store-monad
        ((mpair -> (mvalue state))
         (pair mpair)
         (x -> (car pair))
         (s -> (cdr pair))
         (act2 -> (mproc x)))
      (act2 s))))

;; (define (stateful-package-lift ma)
;;   "My Lift"
;;   (with-monad %state-monad
;;     (return ma)))

(define-monad %stateful-package-monad
  (bind stateful-package-bind)
  (return stateful-package-return))

(define (get)
  "Return the current state as a monadic value."
  (lambda (state)
    (with-monad %store-monad
      (return (cons state state)))))

(define (put value)
  "Set the current state to VALUE and return the previous state as a monadic
value."
  (lambda (state)
    (with-monad %store-monad
      (return (cons state value)))))

(define initial-state
  (gexp->derivation "initial-state"
                    #~(begin
                        (mkdir #$output)
                        (call-with-output-file (string-append #$output "/state")
                          (lambda (p)
                            (display 0 p))))))

(define new-state
  (gexp->derivation "new-state"
                    #~(begin
                        (mkdir #$output)
                        (call-with-output-file (string-append #$output "/state")
                          (lambda (p)
                            (display 3 p))))))

(define store (open-connection))

(define mmthing1
  (lambda (state)
    (mlet* %store-monad
        ((drv (gexp->derivation
               "thing1"
               (with-imported-modules '((guix build utils))
                 #~(begin
                     (mkdir #$output)
                     (call-with-output-file (string-append #$output "/out.txt")
                       (lambda (p)
                         (display 1 p)))

                     (mkdir #$output:state)
                     (call-with-output-file (string-append #$output:state "/state.txt")
                       (lambda (p)
                         (display 3 p))))))))
      (return (cons drv (gexp (ungexp drv "state")))))))

(define mmthing2
  (lambda (state)
    (mlet* %store-monad
        ((drv (gexp->derivation
               "thing2"
               (with-imported-modules '((guix build utils))
                 #~(begin
                     (use-modules (guix build utils))
                     (mkdir #$output)
                     (call-with-output-file (string-append #$output "/out.txt")
                       (lambda (p)
                         (display 1 p)))

                     (mkdir #$output:state)
                     (copy-recursively #$state #$output:state))))))
      (return (cons drv (gexp (ungexp drv "state")))))))

(define mmthings
  (mbegin %stateful-package-monad
    mmthing1
    mmthing2))


(car (run-with-store store (run-with-state mmthings initial-state)))
