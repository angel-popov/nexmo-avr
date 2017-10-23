(in-package :nexmo)

(defun to-url(cmd) (format nil "~A/app?c=~A" *server-callback* cmd) )

(defun add-handler(fn &optional (id (string (gensym))))
  (setf (gethash id *handlers*) fn)
  id)

(defun app-event(id)
  #'(lambda(url params body)
      (hunchentoot:log-message* 'event "url:~A,params:~A,body:~A" url params body)
      ;; (let((res (jsown:parse body)))
      ;;   (when (equal (jsown:val res "status") "started")
      ;;     (let ((result (call-dtmf :id (jsown:val res "uuid"))))
      ;;       (hunchentoot:log-message* 'EventDTMFResult "play-dtmf conversation:~A" result))
      ;;     )
      ;;   )
      ;;(add-handler (app-event id) id) ;; reregister the event
      ))

(defun talk(&key (text "hello") (barge-in t)(repeat 1))
  #'(lambda(url params body)
      (jsown:new-js ("text" text)
                    ("action" "talk")
                    ("loop" repeat)
                    ("bargeIn" barge-in))))

(defun input(handler &key (timeout 3)
                       (submit-on-hash :null)
                       (max-digits :null))
  #'(lambda(url params body)
      (jsown:new-js ("action" "input")
                    ("timeOut" timeout)
                    ("maxDigits" max-digits)
                    ("submitOnHash" submit-on-hash)
                    ("eventUrl" (list (to-url (add-handler #'(lambda(url params body)(funcall handler url params body)))))))))

(defun ncco( &rest items)
  #'(lambda(url params body)
      (list* (remove-if #'null (mapcar #'(lambda(f)(funcall f url params body)) items)))))

(defun play-dtmf(text)
  #'(lambda(url params body)
      (hunchentoot:log-message* 'DTMF "play-dtmf conversation:~A ~S" (hunchentoot:get-parameter "conversation_uuid") params)
      (let ((res (call-dtmf :id (hunchentoot:get-parameter "conversation_uuid")
                            :digits text)))
        (hunchentoot:log-message* 'DTMFResult "play-dtmf conversation:~A" res))

      nil))
;;;;
