;; -*- lisp -*-

(defvar *canvas* (by-id :canvas))
(setf (@ *canvas* width)   (@ *canvas* client-width)
      (@ *canvas* height)  (* 9/16 (@ *canvas* width)))
(let ((max-height (- (@ screen height) (min 150 (* 1/10 (@ screen height))))))
  (when (> (@ *canvas* height) max-height)
    (setf (@ *canvas* height) max-height
          (@ *canvas* width) (* 16/9 max-height)))
  (console-log (concat "Canvas size = " (@ *canvas* width) "✕" (@ *canvas* height))))

(defun get-web-g-l ()
  (let ((gl))
    (try
     (setf gl ((@ *canvas* get-context) "webgl"))
     (:catch (x) (setf gl null)))
    (if (equal null gl)
        (try (setf gl ((@ *canvas* get-context) "experimental-webgl"))
             (:catch (x) (setf gl null))))
    gl))

(defmacro ua-got (string)
  `(> ((@ navigator user-agent index-of) ,string) -1))

(when (equal null (get-web-g-l))
  (setf (@ navigator probably-bad) t)
  (if (y-or-n :no-webgl-wanna-learn)
      (cond
        ((and (ua-got "Opera")
              (or (ua-got "Linux")
                  (ua-got "Win32")
                  (ua-got "Win64")
                  (ua-got "Mac")))
         (open-help "help/Opera_enableWebGL.html"))
        ((and (ua-got "MSIE")
              (or (ua-got "Win32")
                  (ua-got "Win64")))
         (open-help "help/MSIE_IEWebGL_Plugin.html"))
        ((or (ua-got "Firefox")
             (ua-got "Chrome"))
         (open-help "http://get.webgl.org/")))))

(unless (or (ua-got "MSIE") (ua-got "Trident"))
  (setf (@ (by-id "getfirefox") style display) "none"))

(defun look-at (origin point)
  (let* ((coord (list (- (elt origin 0)
                         (elt point 0))
                      (- (elt origin 1)
                         (elt point 1))
                      (- (elt origin 2)
                         (elt point 2))))
         (zvec   ((@ *g-l-g-e to-unit-vec3) coord))
         (xvec   ((@ *g-l-g-e to-unit-vec3) ((@ *g-l-g-e cross-vec3) (list 0 1 0) zvec)))
         (yvec   ((@ *g-l-g-e to-unit-vec3) ((@ *g-l-g-e cross-vec3) zvec xvec))))
    (list  (elt xvec 0)   (elt yvec 0)   (elt zvec 0)   0
           (elt xvec 1)   (elt yvec 1)   (elt zvec 1)   0
           (elt xvec 2)   (elt yvec 2)   (elt zvec 2)   0
           0              0              0              1)))

(defvar *game-scene* null)
(defvar *game-renderer* null)
(defvar *camera-position* (list 0 20))
(defvar *frame-time* (create :last 0
                             :accum 0
                             :count 0
                             :start (get-unix-time)))

(defun render-frame ()
  (let ((now ((@ (new *date) get-time)))
        (last-frame (@ *frame-time* last)))
    (when (zerop (round (mod (- now last-frame) 10)))
      (setf (@ *frame-time* accum)
            ((@ *math round) (/ (+ (/ 1000 (- now last-frame))
                                   (* 9 (@ *frame-time* accum))) 10)))))
  ;; Don't slow down the worst-case by overwriting these
  ;; values. Let the faster ones get slowed down a
  ;; bit, instead.

  (let ((rate "<big>☹☹☹</big> Poor")
        (rate-colour "#f88")
        (slow t))
    (cond
      ((<= 60 (@ *frame-time* accum))
       (setf rate "<big>☺☺☺</big> Great!"
             rate-colour "#88f"
             slow f))

      ((<= 24 (@ *frame-time* accum))
       (setf rate "<big>☺</big> Good"
             rate-colour "#8f8"
             slow f))

      ((<= 12 (@ *frame-time* accum))
       (setf rate "<big>☹</big> Fair"
             rate-colour "#ff8"
             slow t))))

  (let ((el (by-id "framerate")))
    (if (< (@ *frame-time* count) 50)
        (progn (incf (@ *frame-time* count))
               (setf (@ el inner-h-t-m-l) "Warming up…"
                     slow f))          
        ;; Probably just warming up the accumulator
        (setf (@ el inner-h-t-m-l)
              (concat "Graphics: " rate " <BR>@ " (@ *frame-time* accum) " Hz")))
    (setf (@ el style color) rate-colour))
  (setf (@ (by-id "webgl_slow") style visibility) (if slow :visible :hidden))
  (setf (@ *frame-time* last) now)
  ((@ *game-renderer* render)))

(defun stared-at-goats ()
  (report "If you would eMail this report, we would appreciate it.") ;
  (let ((p ((@ document create-element) :p))
        (gl (get-web-g-l))
        (dur (- (new *date) (@ *frame-time* start)))
        (results (concat "Results of frametest.

  (Feel free to leave me a note above this line!)

--------------------

duration=" dur "ms

demo version=" (@ document version) "

player name=" (@ game-state player-name) "

appCodeName=" (@ navigator app-code-name) "

window size=" (@ window inner-width) "×" (@ window inner-height) "

screen size=" (@ screen width) "×" (@ screen height) "

*canvas* size=" (@ *canvas* width) "×" (@ *canvas* height) "

platform=" (@ navigator platform) "

WebGL vendor=" ((@ gl get-parameter) (@ gl -v-e-n-d-o-r)) "

WebGL version=" ((@ gl get-parameter) (@ gl -v-e-r-s-i-o-n)) "

WebGLSL version=" ((@ gl get-parameter) (@ gl
                                           -s-h-a-d-i-n-g_-l-a-n-g-u-a-g-e_-v-e-r-s-i-o-n)) "

WebGL renderer=" ((@ gl get-parameter) (@ gl -r-e-n-d-e-r-e-r)) "

frameRate=" (@ *frame-time* accum)  " f/s

user agent string=(" (@ navigator user-agent) ")")))
    (setf (@ p inner-h-t-m-l)
          (concat "You have run this test for "
                  ((@ Math round) (/ dur 1000))
                  " seconds.
<a href=\"mailto:violetvolts-frametest@star-hope.org?subject=Frametest%20results%20for%20"
                  (encode-u-r-i-component
                   (concat (@ navigator app-code-name)
                           " browser on "
                           (@ navigator platform)))
                  "&body="
                  (encode-u-r-i-component results)
                  "\">Click here to eMail your results</a> <pre>"
                  results
                  "</pre>"))
    (chain (by-id "container") (append-child p))))

(defun scene-ready-go (xml)
  (let ((scene           ((@ xml get-element) :main-scene))
        (camera         ((@ xml get-element) :main-camera))
        (camera-offset  ((@ xml get-element) :camera-offset)))
    (unless (and scene camera camera-offset)
      (report :failed-to-read-xml))
    (console-log "Ready? Go: Scene =" scene)
    (let ((model ((@ xml get-element) :model)))
      ;; set the camera rotation
      ((@ camera set-rot-matrix) (look-at (list 0 (elt *camera-position* 1) 0)
                                          (list 0 2 (- (elt *camera-position* 2)))))
      ((@ scene set-camera) camera)
      ;; load animations after model
      ((@ model add-event-listener) :loaded (lambda (data))))

    ;; draw grid

    (let ((positions (list)))

      (do ((x -50 (incf x))) ((> x 50))
        (when (not (= 0 x))
          (chain (list x 0 -50 x 0 50 50 0 x -50 0 x) (push positions))))

      (let ((line (chain (new (@ *g-l-g-e *object))
                         (set-draw-type (@ *g-l-g-e *d-r-a-w_-l-i-n-e-s)))))
        ((@ line set-mesh) (chain (new (@ *g-l-g-e *mesh))
                                  (set-positions positions)))
        ((@ line set-material) ((@ xml get-element) :lines))
        ((@ scene add-object) line)))

    (setf *game-scene* scene))
  ((@ *game-renderer* set-scene) *game-scene*))

(defun start-render ()
  (setf *game-renderer* (new ((@ *g-l-g-e *renderer) *canvas*)))
  (let ((xml (new (@ *g-l-g-e *document)))
        (camera nil)
        (camera-offset nil))
    (setf (@ xml on-load) (lambda ()
                            (scene-ready-go xml)
                            (setf start-time (0+ (new *date)))
                            (set-interval render-frame 1)
                            (set-interval check-keys 1)
                            (done-loading :xml))))

  (when *rate-test*
    (set-timeout (lambda ()
                   (defvar button ((@ document create-element) :div))
                   (setf (@ button inner-h-t-m-l)
                         "<button onClick=\"staredAtGoats();\">Report Test Results</button>")
                   (chain (by-id :container)
                          (append-child button)))
                 60000))
  
  (start-loading :xml)
  ((@ xml parse-script) "glge_document"))

(when *rate-test*
  (report "This is a simple framerate test; you won't see much.

Click \"log in,\" and then wait about a minute.

Thanks for participating!"))
