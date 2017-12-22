(in-package :situated-program-challenge)

(dex:get "http://localhost:5000/members")

(defparameter members-input-example
  (jojo:to-json '(:|first-name| "Satoshi"
                  :|last-name|  "Imai"
                  :|email|      "satoshi.imai@gmail.com")))

(dex:post "http://localhost:5000/members"
          :content members-input-example :headers '(("content-type" . "application/json")))

(dex:get "http://localhost:5000/members")

(defparameter members-input-example2
  (jojo:to-json '(:|first-name| "Takeshi"
                  :|last-name|  "Imai"
                  :|email|      "takeshi.imai@gmail.com")))

(dex:post "http://localhost:5000/members"
          :content members-input-example2 :headers '(("content-type" . "application/json")))

(dex:get "http://localhost:5000/members")

(defparameter groups-input-example
  "{ \"group-name\": \"group1\", \"admin-member-ids\": [4] }")

(dex:post "http://localhost:5000/groups"
          :content groups-input-example :headers '(("content-type" . "application/json")))

(dex:get "http://localhost:5000/groups")

(defparameter members-join-example
"{
  \"admin\": true
}")

(dex:post "http://localhost:5000/members/5/groups/19"
          :content members-join-example :headers '(("content-type" . "application/json")))

(defparameter members-input-example3
  (jojo:to-json '(:|first-name| "Setsuko"
                  :|last-name|  "Imai"
                  :|email|      "setsuko.imai@gmail.com")))

(dex:post "http://localhost:5000/members"
          :content members-input-example3 :headers '(("content-type" . "application/json")))

(dex:get "http://localhost:5000/members")

(defparameter members-join-example2
"{
  \"admin\": false
}")

(dex:post "http://localhost:5000/members/6/groups/19"
          :content members-join-example :headers '(("content-type" . "application/json")))

(dex:get "http://localhost:5000/groups")

(defparameter members-input-example4
  (jojo:to-json '(:|first-name| "hoge"
                  :|last-name|  "Imai"
                  :|email|      "hoge.imai@gmail.com")))

(dex:post "http://localhost:5000/members"
          :content members-input-example4 :headers '(("content-type" . "application/json")))

(dex:get "http://localhost:5000/members")

(dex:post "http://localhost:5000/members/7/groups/19"
          :content members-join-example2 :headers '(("content-type" . "application/json")))
