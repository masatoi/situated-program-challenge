(in-package :situated-program-challenge)

(defun connect-db ()
  (connect-toplevel :postgres :database-name "meetup" :username "meetup" :password "password123"))

(defmacro deftable (table-name superclass-list &body column-type-pairs)
  `(defclass ,table-name (,@superclass-list)
     ,(mapcar (lambda (col)
		(let* ((col-symbol (if (listp col) (car col) col))
		       (col-name (symbol-name col-symbol))
                       (col-type (if (listp col) (cadr col)))
                       (col-primary (if (find :primary-key col) t nil)))
		  (list col-symbol
			:accessor (intern (concatenate 'string (symbol-name table-name) "-" col-name))
			:initarg (intern col-name :keyword)
                        :col-type col-type
                        :primary-key col-primary)))
       column-type-pairs)
     (:metaclass dao-table-class)))

;;; Define tables

(deftable groups ()
  (name :text))

(deftable members ()
  (first-name :text)
  (last-name  :text)
  (email      :text))

(deftable groups-members ()
  (group-ref  groups)
  (member-ref members)
  (admin  :boolean))

(deftable meetups ()
  (title    :text)
  (start-at :timestamp)
  (end-at   :timestamp)
  (venue-id :integer)
  (group-id :integer))

(deftable meetups-members ()
  (meetup-ref meetups)
  (member-ref members))

(deftable venues ()
  (name        :text)
  (postal-code :text)
  (prefecture  :text)
  (city        :text)
  (street1     :text)
  (street2     :text)
  (group-id    :integer))

(defparameter *table-list*
  '(groups groups-members meetups meetups-members members venues))

(defun create-all-table ()
  (dolist (table *table-list*)
    (execute-sql (car (table-definition table)))))

(defun clear-all-table ()
  (dolist (table *table-list*)
    (mapcar #'delete-dao (select-dao table))))

(defparameter *table_list*
  '(groups groups_members meetups meetups_members members venues))

(defun drop-all-table ()
  (loop for table in *table_list* do
    (execute-sql (sxql:drop-table table))))

(defun efind-dao (class &rest fields-and-values)
  (aif (apply #'find-dao class fields-and-values)
       it
       (error "Not exist ~A" class)))
