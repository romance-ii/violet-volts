;;;; login.lisp — Login page services
(in-package :tootstest.web)

(defroute route-/login "/tootstest/login" ()
  "The  login  page   itself  is  hosted  here   to  ensure  same-domain
  interaction with  the API  calls. It's  mostly a  static page,  but we
  render it through the template engine.

This page should have some more precise headers to assist in caching."
  (render #P"login.html"))
