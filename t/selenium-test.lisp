;;; -*- lisp -*- tootstest UI Tests

(defpackage tootstest.selenium
  (:use :cl :prove :selenium))

(in-package :tootstest.selenium)

(setf *default-reporter* :fiveam) ; no ANSI color in logs



(defun selenium::do-get-new-browser-session (browser url &optional more)
  (selenium::execute *selenium-driver-url*
                     (selenium::marshall-request "getNewBrowserSession"
                                                 browser url more)
                     'string))

(dolist (browser '("android" "chrome" "firefox" "htmlunit"
                   "edge" "internet explorer" "iPhone" "iPad" "opera" "safari"))
  (dolist (platform '("LINUX" "MAC" "WINDOWS" "UNIX" "ANDROID" "ANY")) 
    (let ((*selenium-driver-url*
           (if (uiop:getenv "TRAVIS")
               (concatenate 'string 
                            "http://"
                            (uiop:getenv "SAUCE_USERNAME")
                            ":" 
                            (uiop:getenv "SAUCE_ACCESS_KEY")
                            "@localhost:4445/wd/hub")
               "http://localhost:4444/wd/hub")))
      (with-selenium-session (*selenium-session* :remote "https://www.google.com/")
        (unless *selenium-session*
          (error "Selenium session not started"))
        ;; username = os.environ["SAUCE_USERNAME"]
        ;; access_key = os.environ["SAUCE_ACCESS_KEY"]
        ;; capabilities["tunnel-identifier"] = os.environ["TRAVIS_JOB_NUMBER"]
        ;; hub_url = "%s:%s@localhost:4445" % (username, access_key)
        ;; driver = webdriver.Remote(desired_capabilities=capabilities, command_executor="http://%s/wd/hub" % hub_url)
        ;; capabilities["build"] = os.environ["TRAVIS_BUILD_NUMBER"]
        ;; capabilities["tags"] = [os.environ["TRAVIS_PYTHON_VERSION"], "CI"]
        (do-open "http://www.google.com/webhp?hl=en")
        (do-type "q" "hello world")
        (do-click "btnG")
        (do-wait-for-page-to-load "5000")
        (string= (do-get-title) "hello world - Google Search")))))



(finalize) ; run tests
