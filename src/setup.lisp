(load (merge-pathnames #p".sbclrc" (user-homedir-pathname)))
(load #p"SYS:CONTRIB;**;sb-introspect.fasl.NEWEST")
(load #p"SYS:CONTRIB;**;sb-rotate-byte.fasl.NEWEST")
