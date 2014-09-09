;;;
;;; Copyright (c) Alexei Matveev
;;;
;;; guile -s guile/amber.scm
;;;
(use-modules (system base lalr))
(use-modules (srfi srfi-1))
(use-modules (ice-9 pretty-print))

;;;
;;; Slurps the whole file into a list:
;;;
(define (slurp)
  (let loop ((acc (list)))
    (let ((sexp (read)))
      (if (eof-object? sexp)
          (reverse acc)
          (loop (cons sexp acc))))))

;;;
;;; List  of  entries  with   *.prmtop  (force  field  parameters  and
;;; topology),  *.crd (coordinates) and  *.mol2 (coordinates  an more)
;;; files in the database.
;;;
(define entries
  (with-input-from-file "./guile/entries.scm" slurp))

(define (prmtop-path entry)
  (string-append "./prmcrd/" entry ".prmtop"))

(define (mol2-path entry)
  (string-append "./charged_mol2files/" entry ".mol2"))

(define (get-gaff)
  (with-input-from-file "./guile/gaff-vdw.scm" slurp))

(if #f
    (let ((gaff (get-gaff)))
      (pretty-print (map first gaff))
      (pretty-print (length gaff))))


;;;
;;; This succeds,  both for *.prmtop and *.mol2  files. Though because
;;; of  symbols such  as #{5E16.8}#  and @<TRIPOS>MOLECULE  it  is not
;;; clear if it should:
;;;
(if #f
    (let ((contents (map (lambda (entry)
                           (with-input-from-file
                               ;; (prmtop-path entry)
                               (mol2-path entry)
                             slurp))
                         entries)))
      (pretty-print contents)
      (exit 0)))


;;
;; FIXME: slurps the whole input into a list, yileds that elementwise:
;;
(define (make-greedy-tokenizer make-token)
  (let ((*buf* (slurp)))
    ;; (pretty-print *buf*)
    (lambda ()
      (if (null? *buf*)
          '*eoi*                 ; end of input convention of lalr.scm
          (let ((token (car *buf*)))
            (set! *buf* (cdr *buf*))
            (make-token token))))))

(define (location)
  'undefined)

(define (prmtop-keyword? token)
  (and (symbol? token)
       (let ((string (symbol->string token)))
         (string-prefix? "%" string))))

(define (mol2-keyword? token)
  (and (symbol? token)
       (let ((string (symbol->string token)))
         (string-prefix? "@<TRIPOS>" string))))

;;;
;;; FIXME: mol2 and prmtop parsers share this, they should not:
;;;
(define (make-prmtop-token token)
  ;; (pk (list 'TOKEN: token))
  (cond
   ((eq? token '*eoi) '*eoi*)
   ((prmtop-keyword? token)
    (make-lexical-token token (location) token)) ; %FLAG, %FORMAT
   ((mol2-keyword? token)
    (make-lexical-token token (location) token)) ; @<TRIPOS>MOLECULE
   ((symbol? token)
    (make-lexical-token 'SYMBOL (location) token))
   ((string? token)
    (make-lexical-token 'STRING (location) token)) ; quoted string
   ((and (number? token) (exact? token))
    (make-lexical-token 'EXACT (location) token)) ; integers
   ((and (number? token) (inexact? token))
    (make-lexical-token 'INEXACT (location) token)) ; real numbers
   (#t
    (make-lexical-token 'DATA (location) token)) ; %FORMAT list
   ))

;;;
;;; www.tripos.com/data/support/mol2.pdf
;;;

;;
;; Reverse engineered grammar:
;;
(define (make-prmtop-parser)
  (lalr-parser
   ;;
   ;; Terminals:
   ;;
   (%VERSION %FLAG %FORMAT SYMBOL STRING EXACT INEXACT DATA)

   ;;
   ;; Productions:
   ;;

   ;; Version info is of no interest here:
   (input
    (version sections+): (reverse $2))

   ;; %VERSION  VERSION_STAMP = V0001.000  DATE = 03/27/08  09:03:31
   (version
    (%VERSION data+) : (cons $1 $2))

   (sections+
    (section) : (list $1)
    (sections+ section) : (cons $2 $1))

   ;; Some sections have no data,  just an empty line. Format is of no
   ;; interest here:
   (section
    (%FLAG SYMBOL format data+): (cons $2 $4)
    (%FLAG SYMBOL format): (cons $2 '()))

   ;; Sections seem to be uniform arrays:
   (data+
    (integers+): (reverse $1)
    (doubles+): (reverse $1)
    (symbols+): (reverse $1))

   ;; The second field is a list:
   (format
    (%FORMAT DATA): $2)

   (symbols+
    (SYMBOL): (list $1)
    (symbols+ SYMBOL): (cons $2 $1))

   (integers+
    (EXACT): (list $1)
    (integers+ EXACT): (cons $2 $1))

   (doubles+
    (INEXACT): (list $1)
    (doubles+ INEXACT): (cons $2 $1))
   ))

(define (make-mol2-parser)
  (lalr-parser
   ;;
   ;; Terminals:
   ;;
   (@<TRIPOS>MOLECULE
    @<TRIPOS>ATOM
    @<TRIPOS>BOND
    @<TRIPOS>SUBSTRUCTURE
    @<TRIPOS>COMMENT
    SYMBOL STRING EXACT INEXACT DATA)

   ;;
   ;; Productions:
   ;;

   ;; Version info is of no interest here:
   (input
    (sections+): (reverse $1))

   (sections+
    (section) : (list $1)
    (sections+ section) : (cons $2 $1))

   ;; Some sections have no data,  just an empty line. Format is of no
   ;; interest here:
   (section
    (@<TRIPOS>MOLECULE SYMBOL integers+ SYMBOL SYMBOL): (list $1 $2 (reverse $3) $4 $5)
    (@<TRIPOS>ATOM atom-rows+): (cons $1 (reverse $2))
    (@<TRIPOS>BOND bond-rows+): (cons $1 (reverse $2))
    (@<TRIPOS>SUBSTRUCTURE subst-rows+): (cons $1 (reverse $2))
    (@<TRIPOS>COMMENT symbols*): (cons $1 (reverse $2)))

   (atom-rows+
    (atom-row): (list $1)
    (atom-rows+ atom-row): (cons $2 $1))

   (atom-row
    (EXACT SYMBOL doubles+ SYMBOL EXACT SYMBOL INEXACT): (list $1 $2 (reverse $3) $4 $5 $6 $7))

   (bond-rows+
    (bond-row): (list $1)
    (bond-rows+ bond-row): (cons $2 $1))

   (bond-row
    (EXACT EXACT EXACT bond-type): (list $1 $2 $3 $4))

   (bond-type
    (EXACT): $1
    (SYMBOL): $1)

   (subst-rows+
    (subst-row): (list $1)
    (subst-rows+ subst-row): (cons $2 $1))

   (subst-row
    (EXACT SYMBOL EXACT): (list $1 $2 $3))

   ;; Sections seem to be uniform arrays:
   (data+
    (integers+): (reverse $1)
    (doubles+): (reverse $1)
    (symbols+): (reverse $1))

   (symbols*
    (symbols+): $1
    (): '())

   (symbols+
    (SYMBOL): (list $1)
    (symbols+ SYMBOL): (cons $2 $1))

   (integers+
    (EXACT): (list $1)
    (integers+ EXACT): (cons $2 $1))

   (doubles+
    (INEXACT): (list $1)
    (doubles+ INEXACT): (cons $2 $1))
   ))

;;;
;;; Reader for *.prmtop files from DB:
;;;
(define (prmtop-read)
  (let ((one-shot-parser (make-prmtop-parser))
        (stateful-tokenizer (make-greedy-tokenizer make-prmtop-token)))
    (one-shot-parser stateful-tokenizer error)))

;;;
;;; Reader for MOL2 files from DB:
;;;
(define (mol2-read)
  (let ((one-shot-parser (make-mol2-parser))
        (stateful-tokenizer (make-greedy-tokenizer make-prmtop-token)))
    (one-shot-parser stateful-tokenizer error)))

;;;
;;; Get MOL2 contents for the entry:
;;;
(define (mol2-get entry)
  (with-input-from-file (mol2-path entry) mol2-read))

;;;
;;; Get *.prmtop contents for the entry:
;;;
(define (prmtop-get entry)
  (with-input-from-file (prmtop-path entry) prmtop-read))

(if #f
    (let ((parsed (map (lambda (entry) (mol2-get entry))
                       entries)))
      (pretty-print (length parsed))))


(define (max-charge-diff entry)
  (let ((mol2-atoms (assoc-ref (mol2-get entry) '@<TRIPOS>ATOM))
        (prmtop-charges (assoc-ref (prmtop-get entry) 'CHARGE)))
    (let ((mol2-charges (map (lambda (row)
                               (list-ref row 6))
                             mol2-atoms))
          (prmtop-charges (map (lambda (amber-charge)
                                 (/ amber-charge 18.2223))
                               prmtop-charges)))
      ;; (pretty-print mol2-charges)
      ;; (pretty-print prmtop-charges)
      (let ((diff (apply max (map abs
                                  (map - mol2-charges prmtop-charges)))))
        (when (> diff 1.0e-5)
              (pretty-print (list entry diff))
              (pretty-print (map cons mol2-charges prmtop-charges)))
        diff))))

(if #f
    (let ((max-diff (apply max (map (lambda (entry)
                                      (max-charge-diff entry))
                                    entries))))
      (pretty-print max-diff)))

;;;
;;; FIXME: inefficient:
;;;
(define (get-unique-symbols get-one)
  (let ((selection (delete-duplicates (append-map get-one entries))))
    (map string->symbol
         (sort (map symbol->string selection)
               string<))))

(if #f
    (let ((selection (get-unique-symbols (lambda (entry)
                                           (let ((parsed (prmtop-get entry)))
                                             (assoc-ref parsed 'AMBER_ATOM_TYPE))))))
      (pretty-print selection)
      (let ((gaff (get-gaff)))
        (pretty-print
         (map (lambda (atom-type)
                (assoc atom-type gaff))
              selection)))))

(if #f
    (let ((selection (get-unique-symbols (lambda (entry)
                                           (let* ((parsed (mol2-get entry))
                                                  (atoms (assoc-ref parsed '@<TRIPOS>ATOM)))
                                             (map (lambda (row) (list-ref row 3)) atoms))))))
      (pretty-print selection)
      (pretty-print (length selection))))

;;;
;;; With input as this
;;;
;;; ((C C.1 C.2 C.3)
;;;  (N N.1 N.2 N.3))
;;;
;;; this will return a function that return either C or N for all of
;;; the alternative names.
;;;
(define (make-translator rows)
  (let ((alist (let lp1 ((alist '())
                         (rows rows))
                 (if (null? rows)
                     alist
                     (lp1 (let* ((row (car rows))
                                 (canonical (first row))
                                 (aliases (cdr row)))
                            (let lp2 ((alist alist)
                                      (aliases aliases))
                              (if (null? aliases)
                                  alist
                                  (lp2 (acons (car aliases) canonical alist)
                                       (cdr aliases)))))
                          (cdr rows))))))
    (lambda (symbol)
      (assoc-ref alist symbol))))

;;;
;;; Translation SYBYL -> PG:
;;;
(define from-sybyl
  (make-translator
   '((Br Br)
     (C C.1 C.2 C.3)
     (Cl Cl)
     (F F)
     (H H)
     (I I)
     (N N.1 N.2 N.3)
     (O O.2 O.3)
     (P P.3)
     (S S.1 S.2 S.3 S.O2))))

(define atomic-number
  (make-translator
   '((35 Br)
     (6 C)
     (17 Cl)
     (9 F)
     (1 H)
     (53 I)
     (7 N)
     (8 O)
     (15 P)
     (16 S))))


(let ((sybyl-symbols '(Br C.1 C.2 C.3 Cl F H I N.1 N.2 N.3 O.2 O.3 P.3 S.1 S.2 S.3 S.O2)))
  (pretty-print (map from-sybyl sybyl-symbols))
  (pretty-print (map atomic-number (map from-sybyl sybyl-symbols))))


(define (make-input chemical-symbols cartesian-coordinates solvation)
  (define (make-ua sym pos)
    `(,(symbol->string sym) ,pos (z ,(atomic-number sym))))
  (define (make-bas sym)
    (let ((bas (if (equal? sym 'H) 'bas 'ecp)))
      `(,bas "nwchem" ,(symbol->string sym) "crenbl_ecp" "ahlrichs_coulomb_fitting")))
  `((operations
     (operations_symm #t)
     (operations_integral #t)
     (operations_scf #t)
     (operations_dipole #t)
     (operations_properties #f)
     ,@(if solvation
           '((operations-solvation-effect #t))
           '()))
    (main_options
     (integrals_on_file #f)             ; This is faster
     (relativistic "false")             ; This is an ECP calculation
     (spin_restricted #t))              ; FIXME: do we have radiacals?
    ;; (geo
    ;;  (units angstrom)
    ;;  ("C" (-0.748 -0.015 0.024))
    ;;  ("HC1" (-1.293 -0.202 -0.901) (z 1))
    ;;  ("HC2" (-1.263 0.754 0.6) (z 1))
    ;;  ("HC3" (-0.699 -0.934 0.609) (z 1))
    ;;  ("O" (0.558 0.42 -0.278))
    ;;  ("OH" (0.716 1.404 0.137) (z 1)))
    (geo
     (units angstrom)
     ,@(map make-ua chemical-symbols cartesian-coordinates))
    (mixing (chmix 0.5) (start_after_cycle 5))
    (grid (sym_reduce #t) (weight_grads #t))
    ;; (rep 6 (gridatom (nrad 30) (nang 131)))
    (rep ,(length chemical-symbols)
         (gridatom (nrad 30) (nang 131)))
    (xc_control (xc "pbe"))
    ;; (ecp "nwchem" "C" "crenbl_ecp" "ahlrichs_coulomb_fitting")
    ;; (bas "nwchem" "H" "crenbl_ecp" "ahlrichs_coulomb_fitting")
    ;; (bas "nwchem" "H" "crenbl_ecp" "ahlrichs_coulomb_fitting")
    ;; (bas "nwchem" "H" "crenbl_ecp" "ahlrichs_coulomb_fitting")
    ;; (ecp "nwchem" "O" "crenbl_ecp" "ahlrichs_coulomb_fitting")
    ;; (bas "nwchem" "H" "crenbl_ecp" "ahlrichs_coulomb_fitting")
    ,@(map make-bas chemical-symbols)
    ,@(if solvation
          '((solvation
             (smoothing "fixpva")
             (sol-start-cycle 10)
             (correction-type "None")))
          '())))


(define (write-pg-input entry)
  (let* ((parsed (mol2-get entry))
         (atoms (assoc-ref parsed '@<TRIPOS>ATOM))
         (coords (map third atoms))
         (sybyl-symbols (map fourth atoms))
         (names (map from-sybyl sybyl-symbols)))
    (pretty-print (make-input names coords #t))))


;;; This will produce ~500 *.scm files in the directory:
(if #t
    (for-each (lambda (entry)
                (with-output-to-file (string-append entry ".scm")
                  (lambda () (write-pg-input entry))))
              entries))