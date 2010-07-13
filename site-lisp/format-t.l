;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 21 12:45:22 2004
;;;; Contains: Tests of the ~T format directive
;;;; Modified for xyzzy: NANRI

(require "loop")
(require "rt/rt")

;;; (rt::rem-all-tests)
;;; (rt::do-tests)

(in-package "rt")

(defun make-string (size &key (initial-element #\NUL)
                         (element-type 'character))
  (if (subtypep element-type 'character)
      (make-array size
                  :initial-element initial-element
                  :element-type element-type)
    (error ":ELEMENT-TYPE argument must be a subtype of CHARACTER")))

(defun coin (&optional (n 2))
  "Flip an n-sided coin."
  (eql (random n) 0))

(deftest format.t.1
  (format nil "~0,0T")
  "")

(deftest format.t.2
  (format nil "~1,0T")
  " ")

(deftest format.t.3
  (format nil "~0,1T")
  " ")

(deftest format.t.4
  (loop for i from 0 to 20
        for s = (format nil "~0,vT" i)
        unless (string= s (make-string i :initial-element #\SPC))
        collect (list i s))
  nil)

(deftest format.t.5
  (loop for i from 0 to 20
        for s = (format nil "~v,0T" i)
        unless (string= s (make-string i :initial-element #\SPC))
        collect (list i s))
  nil)

(deftest format.t.6
  (loop for n1 = (random 30)
        for s1 = (make-string n1 :initial-element #\X)
        for n2 = (random 30)
        for inc = (random 20)
        for s2 = (cond
                  ((< n1 n2)
                   (concatenate 'string s1 (make-string (- n2 n1)
                                                        :initial-element #\SPC)))
                  ((= inc 0) s1)
                  (t (loop do (incf n2 inc) while (<= n2 n1))
                     (concatenate 'string s1 (make-string (- n2 n1)
                                                          :initial-element #\SPC))))
        for pretty = (coin)
        for result = (let ((*print-pretty* pretty))
                       (format nil (format nil "~A~~~D,~DT" s1 n2 inc)))
        repeat 100
        unless (string= s2 result)
        collect (list n1 n2 inc pretty s2 result))
  nil)

(deftest format.t.7
  (loop for n1 = (random 30)
        for s1 = (make-string n1 :initial-element #\X)
        for n2 = (random 30)
        for inc = (random 20)
        for s2 = (cond
                  ((< n1 n2)
                   (concatenate 'string s1 (make-string (- n2 n1)
                                                        :initial-element #\SPC)))
                  ((= inc 0) s1)
                  (t (loop do (incf n2 inc) while (<= n2 n1))
                     (concatenate 'string s1 (make-string (- n2 n1)
                                                          :initial-element #\SPC))))
        for pretty = (coin)
        for result = (let ((*print-pretty* pretty))
                       (format nil "~A~v,vt" s1 n2 inc))
        repeat 100
        unless (string= s2 result)
        collect (list n1 n2 inc pretty s2 result))
  nil)

(deftest format.t.8
  (loop for i from 1 to 20
        for s = (format nil " ~v,vT" nil i)
        unless (string= s (make-string (1+ i) :initial-element #\SPC))
        collect (list i s))
  nil)

(deftest format.t.9
  (loop for i from 1 to 20
        for s = (format nil "~v,vT" i nil)
        unless (string= s (make-string i :initial-element #\SPC))
        collect (list i s))
  nil)

(deftest format.t.10
  (format nil "XXXXX~2,0T")
  "XXXXX")

;;; @t

(deftest format.@t.1
  (format nil "~1,1@t")
  " ")

(deftest format.@t.2
  (loop for colnum from 0 to 20
        for s1 = (format nil "~v,1@t" colnum)
        for s2 = (make-string colnum :initial-element #\SPC)
        unless (string= s1 s2)
        collect (list colnum s1 s2))
  nil)

(deftest format.@t.3
  (loop for colnum = (random 50)
        for colinc = (1+ (random 20))
        for s1 = (format nil "~v,v@t" colnum colinc)
        for s2 = (make-string (* colinc (ceiling colnum colinc))
                              :initial-element #\SPC)
        repeat 100
        unless (string= s1 s2)
        collect (list colnum colinc s1 s2))
  nil)

(deftest format.@t.4
  (loop for colnum = (random 50)
        for colinc = (1+ (random 20))
        for s1 = (format nil "~v,1@T~0,v@t" colnum colinc)
        for s2 = (make-string (* colinc (ceiling colnum colinc))
                              :initial-element #\SPC)
        repeat 100
        unless (string= s1 s2)
        collect (list colnum colinc s1 s2))
  nil)

(deftest format.@t.5
  (loop for colnum = (random 50)
        for colinc = (1+ (random 20))
        for pretty = (coin)
        for s1 = (let ((*pretty* pretty))
                   (format nil (format nil "~~~d,~d@t" colnum colinc)))
        for s2 = (make-string (* colinc (ceiling colnum colinc))
                              :initial-element #\SPC)
        repeat 100
        unless (string= s1 s2)
        collect (list colnum colinc pretty s1 s2))
  nil)
