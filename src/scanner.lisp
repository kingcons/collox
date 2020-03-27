(defpackage :collox.scanner
  (:use :cl)
  (:import-from :alexandria
                #:define-constant
                #:if-let
                #:when-let)
  (:import-from :collox.util
                #:define-printer
                #:make-location
                #:*source-path*)
  (:export #:tokenize))
(in-package :collox.scanner)

;;;; Lox Tokens

(define-constant +token-types+
    '(:left-paren :right-paren
      :left-brace :right-brace
      :comma :dot :minus :plus
      :semicolon :slash :star
      :bang :bang-equal
      :equal :double-equal
      :greater :greater-equal
      :less :less-equal
      :identifier :string :number
      :and :class :else :false :fun
      :for :if :nil :or :print :return
      :super :this :true :var :while)
  :test #'equal
  :documentation "A list of symbols for legal classes of Lox tokens.")

(deftype lox-token ()
  `(member ,@+token-types+))

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

(defun make-token (location type &optional value)
  (make-instance 'token :location location :type type :value value))

(define-printer token (type value location)
  "~A (~A) ~A" type value location)

;;;; Syntax Errors

(define-condition syntax-error (program-error)
  ((path :initform *source-path* :reader source-path)
   (line :initarg :line :reader source-line)
   (fragment :initarg :fragment :reader source-fragment)
   (addendum :initform nil :initarg :addendum :reader source-addendum))
  (:documentation "Parser encountered invalid syntax.")
  (:report
   (lambda (condition stream)
     (format stream "Collox has found a syntax error...
Path: `~S`, Line: ~D,
Fragment: ~S ~@[ ~%~A ~]"
             (source-path condition)
             (source-line condition)
             (source-fragment condition)
             (source-addendum condition)))))

;;;; The Collox Scanner interface

(defclass scanner ()
  ((source
    :initarg :source :reader scanner-source)
   (tokens
    :initform '() :reader scanner-tokens)
   (start :initform 0)
   (current :initform 0)
   (line :initform 1 :accessor scanner-line)))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

(define-printer scanner (source tokens)
                "~{~A, ~}~%" tokens)

(defun is-done? (scanner)
  (with-slots (source current) scanner
    (>= current (length source))))

(defun advance (scanner)
  (with-slots (source start current) scanner
    (incf current)
    (char source (1- current))))

(defun next-line (scanner)
  (with-slots (source current) scanner
    (let ((end-of-line (position #\Newline source :start current)))
      (if end-of-line
          (setf current (1+ end-of-line))
          (setf current (length source))))))

(defun complete-string (scanner)
  (with-slots (source current line) scanner
    (if-let (end (position #\" source :start current))
      (let ((newlines (count #\Newline source :start current :end end))
            (contents (subseq source current end)))
        (setf line (incf line newlines)
              current (1+ end))
        contents)
      (error 'syntax-error
             :line line
             :fragment "\"... "
             :addendum "String is missing closing double quote."))))

(defun match (scanner test)
  (with-slots (source current) scanner
    (when (is-done? scanner)
      (return-from match nil))
    (when-let (found (char= (char source current) test))
      (incf current)
      found)))

(defun add-token (scanner type &optional value)
  (with-slots (tokens line start current) scanner
    (let* ((location (make-location line start current))
           (token (make-token location type value)))
      (push token tokens))))

(defun scan-token (scanner)
  (let ((next-char (advance scanner)))
    (cond ((char= next-char #\()
           (add-token scanner :left-paren))
          ((char= next-char #\))
           (add-token scanner :right-paren))
          ((char= next-char #\{)
           (add-token scanner :left-brace))
          ((char= next-char #\})
           (add-token scanner :right-brace))
          ((char= next-char #\,)
           (add-token scanner :comma))
          ((char= next-char #\.)
           (add-token scanner :dot))
          ((char= next-char #\-)
           (add-token scanner :minus))
          ((char= next-char #\+)
           (add-token scanner :plus))
          ((char= next-char #\;)
           (add-token scanner :semicolon))
          ((char= next-char #\*)
           (add-token scanner :star))
          ((char= next-char #\!)
           (let ((token (if (match scanner #\=) :bang-equal :bang)))
             (add-token scanner token)))
          ((char= next-char #\=)
           (let ((token (if (match scanner #\=) :double-equal :equal)))
             (add-token scanner token)))
          ((char= next-char #\>)
           (let ((token (if (match scanner #\=) :greater-equal :greater)))
             (add-token scanner token)))
          ((char= next-char #\<)
           (let ((token (if (match scanner #\=) :less-equal :less)))
             (add-token scanner token)))
          ((char= next-char #\/)
           (with-slots (current) scanner
             (if (match scanner #\/)
                 (next-line scanner)
                 (add-token scanner :slash))))
          ((member next-char '(#\Space #\Return #\Tab))
           nil)
          ((char= next-char #\Newline)
           (incf (scanner-line scanner)))
          ((char= next-char #\")
           (add-token scanner :string (complete-string scanner)))
          (t
           (with-slots (source line start current) scanner
             (let ((fragment (subseq source start current)))
               (error 'syntax-error :line line :fragment fragment)))))))

(defun tokenize (source)
  "Take a SOURCE as input and return a list of Lox tokens."
  (let ((scanner (make-instance 'scanner :source source)))
    (labels ((iter ()
               (with-slots (start current tokens) scanner
                 (if (is-done? scanner)
                     (reverse tokens)
                     (progn
                       (setf start current)
                       (scan-token scanner)
                       (iter))))))
      (iter))))
