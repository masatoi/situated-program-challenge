(in-package :situated-program-challenge)

(defun asc (key alist)
  (cdr (assoc key alist :test #'string=)))

(defmacro with-try-to-json ((result-var &body body-form) &body no-error-body)
  `(handler-case
       (progn ,@body-form)
     (error (e)
       `(500 (:content-type "application/json")
             (,(jojo:to-json (list :|error| (format nil "~A" e))))))
     (:no-error (,result-var)
       (declare (ignorable ,result-var))
       `(200 (:content-type "application/json")
             (,(jojo:to-json (progn ,@no-error-body)))))))

;;; Members ;;;

(defun members-dao->plist (dao)
  (list :|member-id|  (object-id dao)
        :|first-name| (members-first-name dao)
        :|last-name|  (members-last-name dao)
        :|email|      (members-email dao)))

(defroute "/members" (params :method :get)
  (with-try-to-json (dao-list (select-dao 'members))
    (mapcar #'members-dao->plist dao-list)))

;; (dex:get "http://localhost:5000/members")

(defroute "/members" (params :method :post)
  (with-try-to-json
      (dao (insert-dao
            (make-instance 'members
                           :first-name (asc "first-name" params)
                           :last-name  (asc "last-name" params)
                           :email      (asc "email" params))))
    (members-dao->plist dao)))

;; (defparameter members-input-example
;;   (jojo:to-json '(:|first-name| "Satoshi"
;;                   :|last-name|  "Imai"
;;                   :|email|      "satoshi.imai@gmail.com")))
;; (dex:post "http://localhost:5000/members"
;;           :content members-input-example :headers '(("content-type" . "application/json")))

(defroute "/members/:member-id" (params :method :get)
  (with-try-to-json
      (dao (aif (find-dao 'members :id (parse-integer (asc :member-id params)))
                it
                (error "Not exist member")))
    (members-dao->plist dao)))

;; (dex:get "http://localhost:5000/members/1")

(defroute "/members/:member-id/groups/:group-id" (params :method :post)
  (let* ((admin (asc "admin" params))
         (member-id (parse-integer (asc :member-id params)))
         (group-id  (parse-integer (asc :group-id  params)))
         (groups-members-dao (make-instance 'groups-members
                                            :admin admin :member-id member-id :group-id group-id)))
    (with-try-to-json
        (group-dao
          (cond ((null (find-dao 'members :id member-id))
                 (error "Not exist member"))
                ((null (find-dao 'groups  :id group-id))
                 (error "Not exist group"))
                (t (insert-dao groups-members-dao)))
          (find-dao 'groups :id group-id))
      (groups-dao->plist* group-dao))))

;; (defparameter members-join-example
;; "{
;;   \"admin\": true
;; }")

;; (dex:post "http://localhost:5000/members/5/groups/19"
;;           :content members-join-example :headers '(("content-type" . "application/json")))

;;; Groups ;;;

(defun groups-dao->plist* (groups-dao)
  (let* ((group-id (object-id groups-dao))
         (groups-members-list (select-dao 'groups-members (where (:= :group-id group-id))))
         (groups-members-ids (mapcar #'groups-members-member-id groups-members-list))
         (admin-members-ids (mapcar #'groups-members-member-id
                                    (remove-if-not #'groups-members-admin groups-members-list)))
         (members-list (select-dao 'members (where (:in :id groups-members-ids))))
         (admin-members-list (select-dao 'members (where (:in :id admin-members-ids))))
         (venues-list  (select-dao 'venues  (where (:= :group-id group-id))))
         (meetups-list (select-dao 'meetups (where (:= :group-id group-id)))))

    (list :|group-id|   group-id
          :|group-name| (groups-name groups-dao)
          :|admin|      (mapcar #'members-dao->plist admin-members-list)
          :|venues|     (mapcar #'venues-dao->plist  venues-list)
          :|meetups|    (mapcar #'meetups-dao->plist meetups-list)
          :|members|    (mapcar #'members-dao->plist members-list))))

(defroute "/groups" (params :method :get)
  (with-try-to-json
      (groups-dao-list (select-dao 'groups))
    (mapcar #'groups-dao->plist* groups-dao-list)))

;; (dex:get "http://localhost:5000/groups")

(defun groups-dao->plist (groups-dao admin-members-dao-list)
  (list :|group-id|   (object-id groups-dao)
        :|group-name| (groups-name groups-dao)
        :|admin|      (mapcar #'members-dao->plist admin-members-dao-list)))

(defroute "/groups" (params :method :post)
  (let* ((admin-member-ids (asc "admin-member-ids" params))
         (groups-dao (make-instance 'groups :name (asc "group-name" params)))
         (groups-members-dao-list
           (mapcar (lambda (id) (make-instance 'groups-members :admin t :member-id id))
                   admin-member-ids)))
    (with-try-to-json
        (member-dao-list
          (let ((members (select-dao 'members (where (:in :id admin-member-ids)))))
            (when (set-difference admin-member-ids (mapcar #'object-id members))
              (error "Not exist member"))
            (insert-dao groups-dao)
            (dolist (groups-members-dao groups-members-dao-list)
              (setf (groups-members-group-id groups-members-dao) (object-id groups-dao))
              (insert-dao groups-members-dao))
            members))
      (groups-dao->plist groups-dao member-dao-list))))

;; (defparameter groups-input-example
;;   "{ \"group-name\": \"fugagroup\", \"admin-member-ids\": [ 30, 2 ] }")

;; (dex:post "http://localhost:5000/groups"
;;           :content groups-input-example :headers '(("content-type" . "application/json")))

;;; Venues ;;;

(defun venues-dao->plist (dao)
  (list :|venue-id|   (object-id dao)
        :|venue-name| (venues-name dao)
        :|address| (list :|postal-code| (venues-postal-code dao)
                         :|prefecture|  (venues-prefecture dao)
                         :|city|        (venues-city dao)
                         :|address1|    (venues-street1 dao)
                         :|address2|    (venues-street2 dao))))

(defroute "/groups/:group-id/venues" (params :method :get)
  (with-try-to-json
      (dao-list (select-dao 'venues
                  (where (:= :group-id (parse-integer (asc :group-id params))))))
    (mapcar #'venues-dao->plist dao-list)))

;; (dex:get "http://localhost:5000/groups/1/venues")

(defroute "/groups/:group-id/venues" (params :method :post)
  (let ((dao (make-instance 'venues
                            :name        (asc "venue-name" params)
                            :postal-code (asc "postal-code" (asc "address" params))
                            :prefecture  (asc "prefecture"  (asc "address" params))
                            :city        (asc "city"        (asc "address" params))
                            :street1     (asc "address1"    (asc "address" params))
                            :street2     (asc "address2"    (asc "address" params))
                            :group-id    (parse-integer (asc :group-id params)))))
    (with-try-to-json (result-ignored (insert-dao dao))
      (venues-dao->plist dao))))

;; (defparameter venues-input-example
;;   "{ \"venue-name\": \"string\", \"address\": { \"postal-code\": \"string\", \"prefecture\": \"string\", \"city\": \"string\", \"address1\": \"string\", \"address2\": \"string\" } }")

;; (dex:post "http://localhost:5000/groups/1/venues"
;;           :content venues-input-example :headers '(("content-type" . "application/json")))

;;; Meetups ;;;

(defun meetups-dao->plist (meetups-dao)
  (let* ((venues-dao (find-dao 'venues :id (meetups-venue-id meetups-dao)))
         (meetups-members-dao-list
           (select-dao 'meetups-members
             (where (:= :meetup-id (object-id meetups-dao)))))
         (members-dao-list
           (mapcar (lambda (meetups-members-dao)
                     (find-dao 'members :id (meetups-members-member-id meetups-members-dao)))
                   meetups-members-dao-list)))
    (list :|event-id| (object-id   meetups-dao)
          :|title|    (meetups-title    meetups-dao)
          :|start-at| (local-time:format-rfc3339-timestring nil (meetups-start-at meetups-dao))
          :|end-at|   (local-time:format-rfc3339-timestring nil (meetups-end-at meetups-dao))
          :|venue|    (venues-dao->plist venues-dao)
          :|members|  (mapcar #'members-dao->plist members-dao-list))))

(defroute "/groups/:group-id/meetups" (params :method :get)
  (with-try-to-json (meetups-dao-list
                      (select-dao 'meetups
                        (where (:= :group-id (parse-integer (asc :group-id params))))))
    (mapcar #'meetups-dao->plist meetups-dao-list)))

;; (dex:get "http://localhost:5000/groups/1/meetups")

(defroute "/groups/:group-id/meetups" (params :method :post)
  (let ((dao (make-instance 'meetups
                            :group-id (parse-integer (asc :group-id params))
                            :venue-id (asc "venue-id" params)
                            :end-at (local-time:parse-rfc3339-timestring (asc "end-at" params))
                            :start-at (local-time:parse-rfc3339-timestring (asc "start-at" params))
                            :title (asc "title" params))))
    (with-try-to-json (result-ignored
                        (if (find-dao 'venues :id (meetups-venue-id dao))
                            (insert-dao dao)
                            (error "Not exist venue id")))
      (meetups-dao->plist dao))))

;; (defparameter meetups-example
;;   "{
;;   \"title\": \"string\",
;;   \"start-at\": \"2017-12-22T11:27:58.515Z\",
;;   \"end-at\": \"2017-12-22T11:27:58.515Z\",
;;   \"venue-id\": 50
;; }")

;; (dex:post "http://localhost:5000/groups/1/meetups"
;;           :content meetups-example :headers '(("content-type" . "application/json")))

(defroute "/groups/:group-id/meetups/:event-id" (params :method :get)
  (with-try-to-json
      (dao (aif (find-dao 'meetups :id (parse-integer (asc :event-id params)))
                it
                (error "Not exist meetup")))
    (meetups-dao->plist dao)))

;; (dex:get "http://localhost:5000/groups/1/meetups/2")

;;; Meetups-members ;;;

(defroute "/members/:member-id/meetups/:event-id" (params :method :post)
  (let ((dao (make-instance 'meetups-members
                            :meetup-id (parse-integer (asc :event-id params))
                            :member-id (parse-integer (asc :member-id params)))))
    (with-try-to-json
        (meetups-dao
          (let ((member (find-dao 'meetups :id (meetups-members-member-id dao)))
                (meetup (find-dao 'meetups :id (meetups-members-meetup-id dao))))
            (cond ((null member) (error "Not exist member"))
                  ((null meetup) (error "Not exist meetup"))
                  (t (insert-dao dao) meetup))))
      (meetups-dao->plist meetups-dao))))

;; (dex:post "http://localhost:5000/members/3/meetups/2")
