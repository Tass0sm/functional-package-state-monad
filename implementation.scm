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
  (lambda (state)
    (with-monad %store-monad
      (return (cons value state)))))

(define (stateful-package-bind mvalue mproc)
  "Bind MVALUE, a value in the stateful-package monad, and pass it to MPROC."
  (lambda (state)
    (mlet* %store-monad
        ((mpair -> (mvalue state))
         (pair mpair)
         (x -> (car pair))
         (s -> (cdr pair))
         (act2 -> (mproc x)))
      (act2 s))))

(define-monad %stateful-package-monad
  (bind stateful-package-bind)
  (return stateful-package-return))

(define (stateful-package-lift mvalue)
  (lambda (state)
    (mlet %store-monad
        ((value mvalue))
      (return (cons value state)))))

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
  (computed-file "initial-state"
                 #~(begin
                     (mkdir #$output)
                     (call-with-output-file (string-append #$output "/state")
                       (lambda (p)
                         (display 0 p))))))

(define new-state
  (computed-file "new-state"
                 #~(begin
                     (mkdir #$output)
                     (call-with-output-file (string-append #$output "/state")
                       (lambda (p)
                         (display 3 p))))))

(define store (open-connection))

(define mmthing1
  (lambda (state)
    (let ((pkg (package
                 (name "thing1")
                 (outputs (list "out" "state"))
                 (version "0.1")
                 (source #f)
                 (build-system trivial-build-system)
                 (arguments
                  (list
                   #:modules '((guix build utils))
                   #:builder
                   '(let ((out (assoc-ref %outputs "out"))
                          (out-state (assoc-ref %outputs "state"))
                          (state-input (assoc-ref %build-inputs "state")))
                      (use-modules (guix build utils))

                      (mkdir out)    ; create /gnu/store/...-goo
                      (call-with-output-file (string-append out "/test")
                        (lambda (p)
                          (display '(hello guix) p)))

                      (mkdir out-state)    ; create /gnu/store/...-goo
                      (copy-recursively state-input out-state))))
                 (inputs
                  `(("state" ,@state)))
                 (synopsis "Hello, GNU world: An example GNU package")
                 (description "Guess what GNU Hello prints!")
                 (home-page "https://www.gnu.org/software/hello/")
                 (license gpl3+))))
      (values pkg (list pkg "state")))))

(define mmthing2
  (lambda (state)
    (let ((pkg (package
                 (name "thing2")
                 (outputs (list "out" "state"))
                 (version "0.1")
                 (source #f)
                 (build-system trivial-build-system)
                 (arguments
                  (list
                   #:modules '((guix build utils))
                   #:builder
                   '(let ((out (assoc-ref %outputs "out"))
                          (out-state (assoc-ref %outputs "state"))
                          (state-input (assoc-ref %build-inputs "state")))
                      (use-modules (guix build utils))

                      (mkdir out)    ; create /gnu/store/...-goo
                      (call-with-output-file (string-append out "/test")
                        (lambda (p)
                          (display '(hello thing2) p)))

                      (mkdir out-state)    ; create /gnu/store/...-goo
                      (copy-recursively state-input out-state))))
                 (inputs
                  `(("state" ,@state)))
                 (synopsis "Hello, GNU world: An example GNU package")
                 (description "Guess what GNU Hello prints!")
                 (home-page "https://www.gnu.org/software/hello/")
                 (license gpl3+))))
      (values pkg (list pkg "state")))))

(define mmthings
  (mbegin %state-monad
    mmthing1
    mmthing2))

;; (run-with-store store (package->derivation (run-with-state mmthing1 initial-state)))
;; (run-with-store store (package->derivation (run-with-state mmthings (list initial-state))))
(run-with-state mmthings (list initial-state))
