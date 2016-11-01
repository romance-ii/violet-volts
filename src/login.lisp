;;;; login.lisp — Login page services
(in-package :tootstest.web)

(defroute route-/login "/tootstest/login" ()
          "The  login  page   itself  is  hosted  here   to  ensure  same-domain
interaction with the API calls. It's mostly a static page, but we render
it through the template engine.

There is a  lot of “stuff” happening  on this page —  optimizing it will
probably be a Godsend at some point. XXX

This page should have some more precise headers to assist in caching."
          (render #P"login.html"))
