(load (merge-pathnames #p".sbclrc" (user-homedir-pathname)))
(unless (find-package :sb-introspect)
  (load #p"SYS:CONTRIB;**;sb-introspect.fasl.NEWEST"))
(unless (find-package :sb-rotate-byte)
  (load #p"SYS:CONTRIB;**;sb-rotate-byte.fasl.NEWEST"))

