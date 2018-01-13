(defpackage situated-program-challenge-test
  (:use :cl
        :situated-program-challenge
        :prove))
(in-package :situated-program-challenge-test)

;; NOTE: To run this test file, execute `(asdf:test-system :situated-program-challenge)' in your Lisp.

(plan nil)

(format t ";;; Connect to DB~%")

(connect-db)
(drop-all-table)
(create-all-table)

(start)

(format t ";;; GET /members (empty)~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/members")
      (and (string= str "[]")
           (= status 200))))

(defparameter *members1*
  (jojo:to-json '(:|first-name| "Satoshi"
                  :|last-name|  "Imai"
                  :|email|      "satoshi.imai@gmail.com")))

(defparameter *members2*
  (jojo:to-json '(:|first-name| "Taro"
                  :|last-name|  "Yamada"
                  :|email|      "taro.yamada@hoge.com")))

(defparameter *members3*
  (jojo:to-json '(:|first-name| "Hanako"
                  :|last-name|  "Yamada"
                  :|email|      "hanako.yamada@hoge.com")))

(format t ";;; POST /members~%")

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/members"
                  :content *members1*
                  :headers '(("content-type" . "application/json")))
      (and (string= str "{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}")
           (= status 200))))

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/members"
                  :content *members2*
                  :headers '(("content-type" . "application/json")))
      (and (string= str "{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"}")
           (= status 200))))

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/members"
                  :content *members3*
                  :headers '(("content-type" . "application/json")))
      (and (string= str "{\"member-id\":3,\"first-name\":\"Hanako\",\"last-name\":\"Yamada\",\"email\":\"hanako.yamada@hoge.com\"}")
           (= status 200))))

(format t ";;; GET /members~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/members")
      (and (string= str "[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"},{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"},{\"member-id\":3,\"first-name\":\"Hanako\",\"last-name\":\"Yamada\",\"email\":\"hanako.yamada@hoge.com\"}]")
           (= status 200))))

(format t ";;; GET /groups (empty)~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups")
      (and (string= str "[]")
           (= status 200))))

(format t ";;; POST /groups~%")

(defparameter *groups1*
  "{ \"group-name\": \"group1\", \"admin-member-ids\": [] }")

(defparameter *groups2*
  "{ \"group-name\": \"group2\", \"admin-member-ids\": [1] }")

(defparameter *groups3*
  "{ \"group-name\": \"group3\", \"admin-member-ids\": [1,2,3] }")

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/groups"
                  :content *groups1*
                  :headers '(("content-type" . "application/json")))
      (and (string= str "{\"group-id\":1,\"group-name\":\"group1\",\"admin\":[]}")
           (= status 200))))

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/groups"
                  :content *groups2*
                  :headers '(("content-type" . "application/json")))
      (and (string= str "{\"group-id\":2,\"group-name\":\"group2\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}]}")
           (= status 200))))

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/groups"
                  :content *groups3*
                  :headers '(("content-type" . "application/json")))
      (and (string= str "{\"group-id\":3,\"group-name\":\"group3\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"},{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"},{\"member-id\":3,\"first-name\":\"Hanako\",\"last-name\":\"Yamada\",\"email\":\"hanako.yamada@hoge.com\"}]}")
           (= status 200))))

(format t ";;; POST /members/:member-id/groups/:group-id~%")

(defparameter *members-join1*
  "{ \"admin\": true }")

(defparameter *members-join2*
  "{ \"admin\": false }")

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/members/1/groups/1"
                  :content *members-join1* :headers '(("content-type" . "application/json")))
      (and (string= str "{\"group-id\":1,\"group-name\":\"group1\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}],\"venues\":[],\"meetups\":[],\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}]}")
           (= status 200))))

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/members/2/groups/1"
                  :content *members-join2* :headers '(("content-type" . "application/json")))
      (and (string= str "{\"group-id\":1,\"group-name\":\"group1\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}],\"venues\":[],\"meetups\":[],\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"},{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"}]}")
           (= status 200))))

(format t ";;; GET /groups/:group-id/venues (empty)~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups/1/venues")
      (and (string= str "[]")
           (= status 200))))

(format t ";;; GET /groups/:group-id/meetups (empty)~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups/1/meetups")
      (and (string= str "[]")
           (= status 200))))

(format t ";;; POST /groups/:group-id/venues~%")

(defparameter *venues-join1*
  "{ \"venue-name\": \"venue1\", \"address\": { \"postal-code\": \"123\", \"prefecture\": \"Tokyo\", \"city\": \"Shibuya\", \"address1\": \"hoge\", \"address2\": \"mage\" } }")

(defparameter *venues-join2*
  "{ \"venue-name\": \"venue2\", \"address\": { \"postal-code\": \"321\", \"prefecture\": \"Tokyo\", \"city\": \"Suginami\", \"address1\": \"wada\", \"address2\": \"piyo\" } }")

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/groups/1/venues"
                  :content *venues-join1* :headers '(("content-type" . "application/json")))
      (and (string= str "{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}}")
           (= status 200))))

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/groups/1/venues"
                  :content *venues-join2* :headers '(("content-type" . "application/json")))
      (and (string= str "{\"venue-id\":2,\"venue-name\":\"venue2\",\"address\":{\"postal-code\":\"321\",\"prefecture\":\"Tokyo\",\"city\":\"Suginami\",\"address1\":\"wada\",\"address2\":\"piyo\"}}")
           (= status 200))))

(format t ";;; GET /groups/:group-id/venues~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups/1/venues")
      (and (string= str "[{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},{\"venue-id\":2,\"venue-name\":\"venue2\",\"address\":{\"postal-code\":\"321\",\"prefecture\":\"Tokyo\",\"city\":\"Suginami\",\"address1\":\"wada\",\"address2\":\"piyo\"}}]")
           (= status 200))))

(format t ";;; POST /groups/:group-id/meetups~%")

(defparameter *meetups1*
  "{
  \"title\": \"meetup1\",
  \"start-at\": \"2017-12-22T11:27:58.515Z\",
  \"end-at\": \"2017-12-22T11:27:58.515Z\",
  \"venue-id\": 1
}")

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/groups/1/meetups"
                  :content *meetups1* :headers '(("content-type" . "application/json")))
      (and (string= str "{\"event-id\":1,\"title\":\"meetup1\",\"start-at\":\"2017-12-22T20:27:58.515000+09:00\",\"end-at\":\"2017-12-22T20:27:58.515000+09:00\",\"venue\":{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},\"members\":[]}")
           (= status 200))))

(format t ";;; GET /groups/:group-id/meetups~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups/1/meetups")
      (and (string= str "[{\"event-id\":1,\"title\":\"meetup1\",\"start-at\":\"2017-12-23T05:27:58.000000+09:00\",\"end-at\":\"2017-12-23T05:27:58.000000+09:00\",\"venue\":{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},\"members\":[]}]")
           (= status 200))))

(format t ";;; GET /groups/:group-id/meetups/:event-id~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups/1/meetups/1")
      (and (string= str "{\"event-id\":1,\"title\":\"meetup1\",\"start-at\":\"2017-12-23T05:27:58.000000+09:00\",\"end-at\":\"2017-12-23T05:27:58.000000+09:00\",\"venue\":{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},\"members\":[]}")
           (= status 200))))

(format t ";;; POST /members/:member-id/meetups/:event-id~%")

(ok (multiple-value-bind (str status)
        (dex:post "http://localhost:5000/members/1/meetups/1")
      (and (string= str "{\"event-id\":1,\"title\":\"meetup1\",\"start-at\":\"2017-12-23T05:27:58.000000+09:00\",\"end-at\":\"2017-12-23T05:27:58.000000+09:00\",\"venue\":{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}]}")
           (= status 200))))

(format t ";;; GET /groups~%")

(ok (multiple-value-bind (str status)
        (dex:get "http://localhost:5000/groups")
      (and (string= str "[{\"group-id\":1,\"group-name\":\"group1\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}],\"venues\":[{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},{\"venue-id\":2,\"venue-name\":\"venue2\",\"address\":{\"postal-code\":\"321\",\"prefecture\":\"Tokyo\",\"city\":\"Suginami\",\"address1\":\"wada\",\"address2\":\"piyo\"}}],\"meetups\":[{\"event-id\":1,\"title\":\"meetup1\",\"start-at\":\"2017-12-23T05:27:58.000000+09:00\",\"end-at\":\"2017-12-23T05:27:58.000000+09:00\",\"venue\":{\"venue-id\":1,\"venue-name\":\"venue1\",\"address\":{\"postal-code\":\"123\",\"prefecture\":\"Tokyo\",\"city\":\"Shibuya\",\"address1\":\"hoge\",\"address2\":\"mage\"}},\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}]}],\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"},{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"}]},{\"group-id\":2,\"group-name\":\"group2\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}],\"venues\":[],\"meetups\":[],\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"}]},{\"group-id\":3,\"group-name\":\"group3\",\"admin\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"},{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"},{\"member-id\":3,\"first-name\":\"Hanako\",\"last-name\":\"Yamada\",\"email\":\"hanako.yamada@hoge.com\"}],\"venues\":[],\"meetups\":[],\"members\":[{\"member-id\":1,\"first-name\":\"Satoshi\",\"last-name\":\"Imai\",\"email\":\"satoshi.imai@gmail.com\"},{\"member-id\":2,\"first-name\":\"Taro\",\"last-name\":\"Yamada\",\"email\":\"taro.yamada@hoge.com\"},{\"member-id\":3,\"first-name\":\"Hanako\",\"last-name\":\"Yamada\",\"email\":\"hanako.yamada@hoge.com\"}]}]")
           (= status 200))))

(stop)
(finalize)
