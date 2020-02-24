(defpackage :collox.scanner
  (:use :cl)
  (:import-from :alexandria
                #:define-constant)
  (:import-from :collox.util
                #:define-printer
                #:source-location
                #:source-path
                #:source-start
                #:source-finish)
  (:export))
(in-package :collox.scanner)

(define-constant +simple-tokens+
    '(:left-paren :right-paren
      :left-brace :right-brace
      :comma :dot :minus :plus
      :semicolon :slash :star)
  :test #'equal
  :documentation "A list of simple tokens.")

(define-constant +comparable-tokens+
    '(:bang :bang-equal
      :equal :double-equal
      :greater :greater-equal
      :less :less-equal)
  :test #'equal
  :documentation "A list of 1 or 2 character tokens.")

(define-constant +literal-tokens+
    '(:identifier :string :number)
  :test #'equal
  :documentation "Tokens for arbitrary length literals.")

(define-constant +keyword-tokens+
    '(:and :class :else :false :fun :for :if :nil :or
      :print :return :super :this :true :var :while)
  :test #'equal
  :documentation "Tokens for language keywords.")

(defclass token ()
  ((type
    :initarg :type :initform (error 'unrecognized-token)
    :reader token-type)
   (value
    :initarg :value :initform nil
    :reader token-value)
   (location
    :initarg :location :initform (error 'no-location)
    :reader token-location)))

(define-printer token (type value location)
  "~A (~A) ~A" type value location)
