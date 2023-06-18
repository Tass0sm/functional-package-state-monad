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

(define (stateful-package-return x)
  "My Return

  if mstate-input is an m derivation, put it in the mlet and get a derivation
  if mstate-input is an m (derivation output), put it in the mlet and get a (derivation output)
  ungexp either thing in the body
  "
  (match-lambda ((mgexp-input . name)
     (mlet %store-monad ((x-drv x)
                         (gexp-input mgexp-input))
       (gexp->derivation
        (derivation-name x-drv)
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))

              (mkdir #$output)
              (copy-recursively #$x-drv #$output)

              (mkdir #$output:state)
              (copy-recursively #$gexp-input #$output:state))))))))

;; m m derivation -> (derivation -> m m derivation) -> m m derivation
(define (stateful-package-bind mma fmmb)
  "My Bind"
  (match-lambda ((mgexp-input . name)
    (mlet* %store-monad ((a (mma (cons mgexp-input name)))
                         (act2 -> (fmmb a)))
      (act2 (cons (return (gexp (ungexp a "state"))) (derivation-name a)))))))

(define (get)
  "My Get"
  (match-lambda ((mgexp-input . name)
                 (mlet %store-monad ((gexp-input mgexp-input))
                   (gexp->derivation
                    (string-append name "-get")
                    (with-imported-modules '((guix build utils))
                      #~(begin
                          (use-modules (guix build utils))

                          (mkdir #$output)
                          (copy-recursively #$gexp-input #$output)

                          (mkdir #$output:state)
                          (copy-recursively #$gexp-input #$output:state))))))))

(define (put x)
  "My Put"
  (match-lambda ((mgexp-input . name)
                 (mlet %store-monad ((x-drv x)
                                     (gexp-input mgexp-input))
                   (gexp->derivation
                    (string-append (derivation-name x-drv) "-put")
                    (with-imported-modules '((guix build utils))
                      #~(begin
                          (use-modules (guix build utils))

                          (mkdir #$output)

                          (mkdir #$output:state)
                          (copy-recursively #$x-drv #$output:state))))))))

(define store (open-connection))

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

(define act1
  (get))

(define act12
  (stateful-package-bind (get) (lambda (x) (put new-state))))

(define act123
  (stateful-package-bind act12 (lambda (x) (stateful-package-return
                           (gexp->derivation
                            "act3"
                            #~(begin
                                (mkdir #$output)
                                (call-with-output-file (string-append #$output "/out.txt")
                                  (lambda (p)
                                    (display "hello" p)))))))))

;; (run-with-store store initial-state)
;; (run-with-store store new-state)
;; (run-with-store store ((stateful-package-return initial-state) (cons initial-state "initial-state")))
;; (run-with-store store ((stateful-package-return (package->derivation hello)) (cons initial-state "initial-state")))
;; (run-with-store store ((put new-state) (cons initial-state "initial-state")))

;; (run-with-store store (act1 (cons initial-state "initial-state")))
;; (run-with-store store (act12 (cons initial-state "initial-state")))
(run-with-store store (act123 (cons initial-state "initial-state")))
