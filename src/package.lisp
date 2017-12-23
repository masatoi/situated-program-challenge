(in-package :cl-user)

(defpackage situated-program-challenge
  (:use :cl :mito :sxql :anaphora)
  (:export :*app* :*handler*
           :start :stop :re-start
           :connect-db :create-all-table :clear-all-table :drop-all-table))
