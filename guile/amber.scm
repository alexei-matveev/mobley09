;;;
;;; Copyright (c) Alexei Matveev
;;;
;;; guile -s guile/amber.scm
;;;
(use-modules (system base lalr))
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
;;; List of *.prmtop files with force field parameters and topology:
;;;
(define prmtop
  (with-input-from-file "./guile/prmtop.scm" slurp))

;;;
;;; This succeds, though  because of symbols such as  #{5E16.8}# it is
;;; not clear if it should:
;;;
(if #f
    (let ((contents (map (lambda (path)
                           (with-input-from-file (string-append "./prmcrd/" path) slurp))
                         prmtop)))
      (pretty-print (length contents))))


;;
;; FIXME: slurps the whole input into a list, yileds that elementwise:
;;
(define (make-greedy-tokenizer)
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

(define (keyword? token)
  (and (symbol? token)
       (let ((string (symbol->string token)))
         (string-prefix? "%" string))))

(define (make-token token)
  ;; (pretty-print (list 'TOKEN: token))
  (cond
   ((eq? token '*eoi) '*eoi*)
   ((keyword? token)
    (make-lexical-token token (location) token)) ; %FLAG, %FORMAT
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


(define (prmtop-read)
  ((make-prmtop-parser) (make-greedy-tokenizer) error))

(for-each
 (lambda (path)
   (let ((parsed (with-input-from-file (string-append "./prmcrd/" path)
                   prmtop-read)))
     (pretty-print (cons path parsed))))
 prmtop)
