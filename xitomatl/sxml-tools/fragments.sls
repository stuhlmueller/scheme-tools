#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools fragments (2008 06 27))
  (export
    sxml:display-fragments
    SRV:send-reply
    sxml:display-feed
    sxml:clean-fragments
    sxml:clean-feed
    sxml:shtml->http
    sxml:fragments->http)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl sxml-tools sxml-tools))

  (include/resolve ("xitomatl" "sxml-tools") "fragments.scm")
)
