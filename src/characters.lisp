(in-package :tootstest.web)
(syntax:use-syntax :annot)

(defroute characters
    ("/characters/"
     :method :get :accept '("application/json")) ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute characters/by-id
    ("/characters/:character-id"
     :method :get :accept '("application/json"))
  (&key character-id)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute put/characters
    ("/characters"
     :method :put :accept '("application/json")) ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute post/characters/by-id
    ("/characters/:character-id"
     :method :post :accept '("application/json"))
  (&key character-id)
  (assert-my-character character-id)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute delete/characters/by-id
    ("/characters/:character-id"
     :method :delete :accept '("application/json"))
  (&key character-id)
  (assert-my-character character-id)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute characters.html
    ("/characters/"
     :method :get :accept '("text/html")) ()
  "Error")

(defroute characters/by-id.html
    ("/characters/:character-id"
     :method :get :accept '("text/html"))
  (&key character-id)
  "Error")
