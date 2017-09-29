(in-package #:nexmo)

(load "key-secret.lisp") ; the file contains key info - see bellow
;;
;; (setf *server-callback* "http://..."
;;       *key* "..."
;;       *secret* "..."
;;       *app-id* "..."
;;       *key-file* "...")


(defun to-www-form(params)
  (format nil "~{~{~A=~A~}~^&~}" params) )

(defun jwt-key (&optional (app-id *app-id*) (key-file *key-file*))
  (UIOP:run-program
   (format nil "nexmo setup ~A ~A" *key* *secret*)
   :input nil :output '(:string :stripped t))
  (UIOP:run-program
   (format nil "nexmo jwt:generate ~A application_id=~A"key-file app-id)
   :input nil :output '(:string :stripped t)))

(defun nexmo-query (url method content-type headers body )
  (multiple-value-bind (response code headers)
      (drakma:http-request url
                           :method method
                           :want-stream nil
                           :content-type content-type
                           :additional-headers headers
                           :content body)
    (when response
      (jsown:parse
       (if (stringp response) (print response)
           (babel:octets-to-string response))))))

(defun send-sms(&key (to "359899041649")
                  (from "NEXMO")
                  (text "hello from nexmo!")
                  (callback (format nil "~A/sms" *server-callback*)))
  (nexmo-query "https://rest.nexmo.com/sms/json"
               :POST "application/x-www-form-urlencoded" nil
               (to-www-form `(("api_key" *api*)
                              ("api_secret" *secret*)
                              ("to" ,to)
                              ("from" ,from)
                              ("text" ,text)
                              ("callback" ,callback)))) )

(defun rest-call-new(&key (to "359899041649")
                  (from "NEXMO")
                  (text "hello from nexmo!")
                       (answer-url (format nil "~A/answer_url" *server-callback*)))
  (nexmo-query "https://rest.nexmo.com/call/json" :POST "application/x-www-form-urlencoded" nil
               (to-www-form `(("api_key" *key*)
                              ("api_secret" *secret*)
                              ("to" ,to)
                              ("from" ,from)
                              ("text" ,text)
                              ("answer_url" ,answer-url))) ))



(defun application-new (&key (name "application")
                          (type "voice") ;; the only type supported so far
                          (answer-url (format nil "~A/app-answer-url" *server-callback*))
                          (events-url (format nil "~A/app-events-url" *server-callback*)))
  (nexmo-query "https://api.nexmo.com/v1/applications" :POST "application/x-www-form-urlencoded" nil
               (to-www-form `(("api_key" *api*)
                              ("api_secret" *secret*)
                              ("name" ,name)
                              ("type" ,type)
                              ("answer_url" ,answer-url)
                              ("event_url" ,events-url)))))

(defun applications (&key (answer-url (format nil "~A/sms" *server-callback*))
                       (events-url (format nil "~A/sms" *server-callback*)))
  (nexmo-query (format nil "https://api.nexmo.com/v1/applications/?~A" (to-www-form `(("api_key" *api*)
                                                                                      ("api_secret" *secret*))))
               :get nil nil nil nil ))

(defun call-new (&key (to "359899041649")
                   (from "359899041649")
                   (call-id nil)
                   (answer-url (list (to-url (add-handler (ncco (talk :text "Start typing dtmf" :barge-in :null :repeat 1)
                                                                (input #'(lambda(url params body)
                                                                           (let ((res (call-dtmf :id call-id :digits  "1234567890#")))
                                                                             (hunchentoot:log-message* 'DTMFPLAY "play-dtmf to ~A conversation:~A:body ~A" call-id res body)
                                                                             (funcall (ncco (talk :text "Dtmf completed.Waiting for response from another party"
                                                                                                  :barge-in :null  :repeat 50))
                                                                                      url params body)
                                                                             ))
                                                                       :timeout 1)
                                                                )
                                                          "active-answer"))))
                   (event-url (list (to-url (add-handler (app-event "active-event") "active-event")))))
  (let ((res (nexmo-query "https://api.nexmo.com/v1/calls"
                          :post "application/json" `(("Authorization" . ,(format nil "Bearer ~A" (jwt-key))))
                          (format nil "{\"to\":[{\"type\": \"phone\",\"number\": ~A}],
      \"from\": {\"type\": \"phone\",\"number\": ~A},
      \"answer_url\":[~{~S~^,~}],
      \"event_url\":[~{~S~^,~}]
}" to from answer-url event-url))))
    (setf call-id (jsown:val res "uuid")
          *last* (jsown:val res "uuid"))
    res))

(defun call-dtmf (&key (id "call-id")
                    (digits "359899041649"))
  (format t "https://api.nexmo.com/v1/calls/~A/dtmf" id)
  (nexmo-query (format nil "https://api.nexmo.com/v1/calls/~A/dtmf" id)
               :put "application/json" `(("Authorization" . ,(format nil "Bearer ~A" (jwt-key))))
               (jsown:to-json (jsown:new-js ("digits" digits)))))

(defun call(&key (id "call-id") )
  (nexmo-query (format nil "https://api.nexmo.com/v1/calls/~A" id)
               :get "application/json" `(("Authorization" . ,(format nil "Bearer ~A" (jwt-key))))
               ()))

(defun call-all( )
  (nexmo-query (format nil "https://api.nexmo.com/v1/calls")
               :get "application/json" `(("Authorization" . ,(format nil "Bearer ~A" (jwt-key))))
               ()))

(hunchentoot:define-easy-handler (sms :uri "/sms") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A"data))

(hunchentoot:define-easy-handler (call :uri "/call") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (format t "~%call:~A~%" (babel:octets-to-string (hunchentoot:raw-post-data)))
  (format nil "call:~A" data))

(hunchentoot:define-easy-handler (events :uri "/call-events") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (format t "~%call-events:~A~%"(babel:octets-to-string (hunchentoot:raw-post-data)))
  (format nil "event:~A" (babel:octets-to-string (hunchentoot:raw-post-data))))

(defmacro nexmo-handler(url &optional (answer (format nil "[
    {
        \"action\": \"talk\",
        \"voiceName\": \"Russell\",
        \"bargeIn\":true,
        \"text\": \"Hi, this is Russell. You are listening to a text-to-speech Call made with Nexmo's Voice API\"
    },
{
    \"action\": \"input\",
\"headers\":[\"some\":\"data\"],
    \"eventMethod\":\"GET\",
    \"eventUrl\": [\"~A/ivr\"]
  }
]" *server-callback*)))
  `(hunchentoot:define-easy-handler (,url :uri (format nil "/~(~A~)" ',url)) (data)
    (setf (hunchentoot:content-type*) "text/plain")
    (hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" ',url (hunchentoot:query-string*)
            (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data))))
    (hunchentoot:log-message* 'answ "~A" ,answer)
    ,answer))

(nexmo-handler app-event "")
(nexmo-handler app-answer "[
    {
      \"action\": \"talk\",
      \"voiceName\": \"Jennifer\",
      \"text\": \"Hello, thank you for calling. This is Jennifer from Nexmo. Ciao.\"
    }
  ]" )


(nexmo-handler app-events-url "[]")
(nexmo-handler app-answer-url "")
(nexmo-handler event-url "[]")
(nexmo-handler answer-url (format nil "[
    {
        \"action\": \"talk\",
        \"voiceName\": \"Russell\",
        \"bargeIn\":true,
        \"text\": \"Hi, this is Russell. You are listening to a text-to-speech Call made with Nexmo's Voice API\"
    },
{
    \"action\": \"input\",
    \"eventMethod\":\"GET\",
    \"eventUrl\": [\"~A/ivr?urls=ncco1,ncco2\"]
  }
]" *server-callback*))

(hunchentoot:define-easy-handler (ivr :uri (format nil "/ivr")) (urls)
  (setf (hunchentoot:content-type*) "text/plain")
  (format t "~%Urls:~(~A~) query-params:~A~%Body~A~%" urls (hunchentoot:query-string*)
          (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data))))
  (format nil "[]" ))

(nexmo-handler ivr (format nil "[
    {
        \"action\": \"talk\",
        \"voiceName\": \"Russell\",
        \"bargeIn\":true,
        \"text\": \"Hi, this is Russell. You are listening to a text-to-speech Call made with Nexmo's Voice API\"
    },
{
    \"action\": \"input\",
    \"eventUrl\": [\"~A/ivr?url1=google.com&url2=yahoo.com\"]
  }
]" *server-callback*))

(hunchentoot:define-easy-handler (app :uri "/app") (data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((body (when (hunchentoot:raw-post-data)(babel:octets-to-string (hunchentoot:raw-post-data)))))
    (hunchentoot:log-message* 'requ "~(~A~):~A Body:~A" "app" (hunchentoot:query-string*) body)
    (let ((cmd (hunchentoot:parameter "c")))
      (if (and cmd (gethash cmd *handlers*))
          (let ((answer (funcall (gethash cmd *handlers*)
                                 "app" (hunchentoot:get-parameters*) body)))
            (hunchentoot:log-message* 'answ "~A" (jsown:to-json answer))
            (jsown:to-json answer))
          (hunchentoot:log-message* 'error "Handler for the request not found") )
      )
     ))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))

(defun start-server()
  (add-handler (ncco (talk :text "Hello,type your number."
                           :barge-in t :repeat 5)
                     (input #'(lambda(url params body)
                                (funcall (ncco (talk :text (format nil "You have pressed ~A" (jsown:val (jsown:parse body) "dtmf"))
                                                     :barge-in :null))
                                         url params body))
                            :submit-on-hash t
                            :max-digits 20
                            :timeout 58))
               "answer")

  (add-handler (app-event "event") "event")
  (hunchentoot:start *server*) )
