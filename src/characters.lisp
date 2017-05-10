(in-package :tootstest.web)
(syntax:use-syntax :annot)

(defroute characters
    ("/characters/"
     :method :get :accept '("application/json")) ()
  "TODO.  Should   obtain  a  list   of  characters  available   to  the
logged-in user."
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute characters/by-id
    ("/characters/:character-id"
     :method :get :accept '("application/json"))
  (&key character-id)
  "TODO. Should obtain a character's information, given its ID."
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute put/characters
    ("/characters"
     :method :put :accept '("application/json")) ()
  "TODO. Should allow the player to create a new Toots character."
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute post/characters/by-id
    ("/characters/:character-id"
     :method :post :accept '("application/json"))
  (&key character-id)
  "TODO. Should allow the player to update a character's information."
  (assert-my-character character-id)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute delete/characters/by-id
    ("/characters/:character-id"
     :method :delete :accept '("application/json"))
  (&key character-id)
  "TODO. Should allow the player to delete a Toots character."
  (assert-my-character character-id)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))

(defroute characters.html
    ("/characters/"
     :method :get :accept '("text/html")) ()
  "TODO. Should render an HTML page with the player's characters."
  "Error")

(defroute characters/by-id.html
    ("/characters/:character-id"
     :method :get :accept '("text/html"))
  (&key character-id)
  "TODO. Should render an HTML page with a character's information."
  "Error")
