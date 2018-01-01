(in-package :situated-program-challenge)

(defun asc (key alist)
  (cdr (assoc key alist :test #'string=)))

(defmacro with-protect-to-json (&body body)
  `(handler-case
       `(200 (:content-type "application/json")
             (,(jojo:to-json (progn ,@body))))
     (error (e)
       `(500 (:content-type "application/json")
             (,(jojo:to-json (list :|error| (format nil "~A" e))))))))

;;; Members ;;;

(defun members-dao->plist (dao)
  (list :|member-id|  (object-id dao)
        :|first-name| (members-first-name dao)
        :|last-name|  (members-last-name dao)
        :|email|      (members-email dao)))

(defroute "/members" (params :method :get)
  (with-protect-to-json
    (mapcar #'members-dao->plist (select-dao 'members))))

(defroute "/members" (params :method :post)
  (with-protect-to-json
    (let ((dao (make-instance 'members
                              :first-name (asc "first-name" params)
                              :last-name  (asc "last-name" params)
                              :email      (asc "email" params))))
      (insert-dao dao)
      (members-dao->plist dao))))

(defroute "/members/:member-id" (params :method :get)
  (with-protect-to-json
    (let ((dao (efind-dao 'members :id (parse-integer (asc :member-id params)))))
      (members-dao->plist dao))))

(defroute "/members/:member-id/meetups/:event-id" (params :method :post)
  (let ((member-id (parse-integer (asc :member-id params)))
        (meetup-id (parse-integer (asc :event-id params))))
    (with-protect-to-json
      (let* ((member (efind-dao 'members :id member-id))
             (meetup (efind-dao 'meetups :id meetup-id))
             (meetups-members-dao
               (make-instance 'meetups-members :member-ref member :meetup-ref meetup)))
        (insert-dao meetups-members-dao)
        (meetups-dao->plist meetup)))))

(defroute "/members/:member-id/groups/:group-id" (params :method :post)
  (let ((admin (asc "admin" params))
        (member-id (parse-integer (asc :member-id params)))
        (group-id  (parse-integer (asc :group-id  params))))
    (with-protect-to-json
      (let* ((member-dao (efind-dao 'members :id member-id))
             (group-dao  (efind-dao 'groups  :id group-id))
             (groups-members-dao
               (make-instance 'groups-members
                              :admin admin :member-ref member-dao :group-ref group-dao)))
        (insert-dao groups-members-dao)
        (groups-dao->plist* group-dao)))))

;;; Groups ;;;

(defun groups-dao->plist* (groups-dao)
  (let* ((group-id (object-id groups-dao))
         (groups-members-list
           (select-dao 'groups-members (includes 'members)
             (where (:= :group-ref-id group-id))))
         (members-list (mapcar #'groups-members-member-ref groups-members-list))
         (admin-members-list
           (mapcar #'groups-members-member-ref
                   (remove-if-not #'groups-members-admin groups-members-list)))
         (venues-list  (select-dao 'venues  (where (:= :group-id group-id))))
         (meetups-list (select-dao 'meetups (where (:= :group-id group-id)))))
    (list :|group-id|   group-id
          :|group-name| (groups-name groups-dao)
          :|admin|      (mapcar #'members-dao->plist admin-members-list)
          :|venues|     (mapcar #'venues-dao->plist  venues-list)
          :|meetups|    (mapcar #'meetups-dao->plist meetups-list)
          :|members|    (mapcar #'members-dao->plist members-list))))

(defroute "/groups" (params :method :get)
  (with-protect-to-json
    (mapcar #'groups-dao->plist* (select-dao 'groups))))

(defun groups-dao->plist (groups-dao admin-members-dao-list)
  (list :|group-id|   (object-id groups-dao)
        :|group-name| (groups-name groups-dao)
        :|admin|      (mapcar #'members-dao->plist admin-members-dao-list)))

(defroute "/groups" (params :method :post)
  (let* ((admin-member-ids (asc "admin-member-ids" params))
         (group-name (asc "group-name" params))
         (group-dao (make-instance 'groups :name group-name)))
    (with-protect-to-json
      (let* ((members (if admin-member-ids (select-dao 'members (where (:in :id admin-member-ids)))))
             (groups-members-dao-list
               (mapcar (lambda (member-ref)
                         (make-instance 'groups-members :admin t :member-ref member-ref))
                       members)))
        (when (set-difference admin-member-ids (mapcar #'object-id members))
          (error "Not exist member"))
        (insert-dao group-dao)
        (dolist (dao groups-members-dao-list)
          (setf (groups-members-group-ref dao) group-dao)
          (insert-dao dao))
        (groups-dao->plist group-dao members)))))

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
  (with-protect-to-json
    (mapcar #'venues-dao->plist
            (select-dao 'venues
              (where (:= :group-id (parse-integer (asc :group-id params))))))))

(defroute "/groups/:group-id/venues" (params :method :post)
  (let ((dao (make-instance 'venues
                            :name        (asc "venue-name" params)
                            :postal-code (asc "postal-code" (asc "address" params))
                            :prefecture  (asc "prefecture"  (asc "address" params))
                            :city        (asc "city"        (asc "address" params))
                            :street1     (asc "address1"    (asc "address" params))
                            :street2     (asc "address2"    (asc "address" params))
                            :group-id    (parse-integer (asc :group-id params)))))
    (with-protect-to-json
      (insert-dao dao)
      (venues-dao->plist dao))))

;;; Meetups ;;;

(defun meetups-dao->plist (meetups-dao)
  (let* ((venues-dao (find-dao 'venues :id (meetups-venue-id meetups-dao)))
         (meetups-members-dao-list
           (select-dao 'meetups-members (includes 'members)
             (where (:= :meetup-ref-id (object-id meetups-dao)))))
         (members-dao-list
           (mapcar #'meetups-members-member-ref meetups-members-dao-list)))
    (list :|event-id| (object-id   meetups-dao)
          :|title|    (meetups-title    meetups-dao)
          :|start-at| (local-time:format-rfc3339-timestring nil (meetups-start-at meetups-dao))
          :|end-at|   (local-time:format-rfc3339-timestring nil (meetups-end-at meetups-dao))
          :|venue|    (venues-dao->plist venues-dao)
          :|members|  (mapcar #'members-dao->plist members-dao-list))))

(defroute "/groups/:group-id/meetups" (params :method :get)
  (with-protect-to-json
    (mapcar #'meetups-dao->plist
            (select-dao 'meetups
              (where (:= :group-id (parse-integer (asc :group-id params))))))))

(defroute "/groups/:group-id/meetups" (params :method :post)
  (let ((dao (make-instance 'meetups
                            :group-id (parse-integer (asc :group-id params))
                            :venue-id (asc "venue-id" params)
                            :end-at (local-time:parse-rfc3339-timestring (asc "end-at" params))
                            :start-at (local-time:parse-rfc3339-timestring (asc "start-at" params))
                            :title (asc "title" params))))
    (with-protect-to-json
      (efind-dao 'venues :id (meetups-venue-id dao))
      (insert-dao dao)
      (meetups-dao->plist dao))))

(defroute "/groups/:group-id/meetups/:event-id" (params :method :get)
  (with-protect-to-json
    (meetups-dao->plist
     (efind-dao 'meetups :id (parse-integer (asc :event-id params))))))
