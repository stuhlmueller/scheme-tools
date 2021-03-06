#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl file-system paths)
  (srfi :78 lightweight-testing))

(path-style 'posix)

(check (path? "")
       => #F)
(check (path? 1)
       => #F)
(check (path? 'oops)
       => #F)
(check (path? "a")
       => #T)
(check (path? " ")
       => #T)
(check (path? "/")
       => #T)
(check (path? "/foo")
       => #T)

(check (absolute-path? "")
       => #F)
(check (absolute-path? "a")
       => #F)
(check (absolute-path? " a ")
       => #F)
(check (absolute-path? "a/ b")
       => #F)
(check (absolute-path? "a// //b")
       => #F)
(check (absolute-path? "a////b///")
       => #F)
(check (absolute-path? "/")
       => #T)
(check (absolute-path? "///// //")
       => #T)
(check (absolute-path? "/a")
       => #T)
(check (absolute-path? "/a/b")
       => #T)
(check (absolute-path? "//a/b")
       => #T)
(check (absolute-path? "/////a //b//")
       => #T)

(check (relative-path? "")
       => #F)
(check (relative-path? " ")
       => #T)
(check (relative-path? " a ")
       => #T)
(check (relative-path? "a/b")
       => #T)
(check (relative-path? "a// // b ")
       => #T)
(check (relative-path? "a////b///")
       => #T)
(check (relative-path? "/")
       => #F)
(check (relative-path? "///// //")
       => #F)
(check (relative-path? "/a")
       => #F)
(check (relative-path? "/a/b")
       => #F)
(check (relative-path? "//a/b")
       => #F)
(check (relative-path? "/////a //b//")
       => #F)

(check (path-join)
       => "")
(check (path-join "a")
       => "a")
(check (path-join "/")
       => "/")
(check (path-join "/" "a")
       => "/a")
(check (path-join "/a")
       => "/a")
(check (path-join " " "a")
       => " /a")
(check (path-join "" "a")
       => "a")
(check (path-join "" "/a")
       => "/a")
(check (path-join "a" "b")
       => "a/b")
(check (path-join "a" "" "b" "")
       => "a/b")
(check (path-join "" "a" "b")
       => "a/b")
(check (path-join "a/b")
       => "a/b")
(check (path-join "/a/b")
       => "/a/b")
(check (path-join "" "/a/b")
       => "/a/b")
(check (path-join "//a///b")
       => "/a/b")
(check (path-join "a " " b")
       => "a / b")
(check (path-join "/a" "b")
       => "/a/b")
(check (path-join "/" "a" "b")
       => "/a/b")
(check (path-join "/" "a/b")
       => "/a/b")
(check (path-join "a/b" "/")
       => "a/b")
(check (path-join "/" "a/b" "/")
       => "/a/b")
(check (path-join "/////a/////" "b" "c")
       => "/a/b/c")
(check (path-join "" "/////a/////" "b" "c")
       => "/a/b/c")
(check (path-join "/////a/////" "/b/" "c////")
       => "/a/b/c")
(check (path-join "" "/////a/////" "///b" "///c")
       => "/a/b/c")
(check (path-join "a" "b" "c" "d" "e" "f")
       => "a/b/c/d/e/f")
(check (path-join "a" "b/c/d" "e/f")
       => "a/b/c/d/e/f")
(check (path-join "a" "/b//c///d" "e//////f")
       => "a/b/c/d/e/f")
(check (path-join "/a" "b" "c" "d" "e" "f")
       => "/a/b/c/d/e/f")
(check (path-join "//////" "a" "b" "c" "d" "e" "f")
       => "/a/b/c/d/e/f")
(check (path-join "//////" "//////a" "b////////" "c/" "/d//" "//e" "f////////////////")
       => "/a/b/c/d/e/f")
(check (path-join "/// ///" "//////a" "b b////////" "c/" "/d//" "//e" "f////////////////  ")
       => "/ /a/b b/c/d/e/f/  ")

(check (path-split "")
       => '())
(check (path-split "/")
       => '("/"))
(check (path-split "a")
       => '("a"))
(check (path-split "ab cd")
       => '("ab cd"))
(check (path-split "a/b")
       => '("a" "b"))
(check (path-split "a/b/")
       => '("a" "b"))
(check (path-split "aa a/b bb bbb")
       => '("aa a" "b bb bbb"))
(check (path-split "/a/b")
       => '("/" "a" "b"))
(check (path-split " /a/b")
       => '(" " "a" "b"))
(check (path-split " /a / b")
       => '(" " "a " " b"))
(check (path-split "//// a / b bbbbb ")
       => '("/" " a " " b bbbbb "))

(check (normalize-path "")
       => "")
(check (normalize-path "/")
       => "/")
(check (normalize-path "//")
       => "/")
(check (normalize-path "////////////")
       => "/")
(check (normalize-path "a")
       => "a")
(check (normalize-path "ab cd")
       => "ab cd")
(check (normalize-path "a/b")
       => "a/b")
(check (normalize-path "a//b")
       => "a/b")
(check (normalize-path "a/b/")
       => "a/b")
(check (normalize-path "a/////////////b")
       => "a/b")
(check (normalize-path "a////b//////")
       => "a/b")
(check (normalize-path "aa a/b bb bbb")
       => "aa a/b bb bbb")
(check (normalize-path "/a/b")
       => "/a/b")
(check (normalize-path " /a/b")
       => " /a/b")
(check (normalize-path "//a")
       => "/a")
(check (normalize-path "//////a//////")
       => "/a")
(check (normalize-path "//////a////bb//ccc///////dddd/eeeee//")
       => "/a/bb/ccc/dddd/eeeee")

(check (path=? "/" "//" "///")
       => #T)
(check (path=? "/a" "////a" "//a/")
       => #T)
(check (path=? "//////a////bb//ccc///////dddd/eeeee//"
               "/a/bb/ccc/dddd/eeeee"
               "///a/bb///ccc///dddd//eeeee")
       => #T)
(check (path=? "a/bb/ccc/" "a//bb///ccc")
       => #T)
(check (path=? "a/bb/ccc/" "a//bb///ccc" "a///bb//ccc" "a//bb///ccc/" "a//bb///ccc//"
               "a//bb///ccc" "a//bb///ccc" "a/bb/ccc" "a//bb//ccc" "a/bb/ccc/")
       => #T)
(check (path=? "foo/bar" "foo/baz")
       => #F)
(check (path=? "/a/b/c/d" "/a/b/c/")
       => #F)


(path-style 'windows)

(check (path? "a")
       => #T)
(check (path? "foo\\bar")
       => #T)
(check (path? "\\foo")
       => #T)
(check (path? "C:\\")
       => #T)
(check (path? "C:foo")
       => #T)
(check (path? "C:")
       => #F)
(check (path? "")
       => #F)

(check (absolute-path? "C:")
       => #F)
(check (absolute-path? "foo\\bar")
       => #F)
(check (absolute-path? "C:foo")
       => #F)
(check (absolute-path? "\\")
       => #T)
(check (absolute-path? "\\foo")
       => #T)
(check (absolute-path? "C:\\")
       => #T)
(check (absolute-path? "C:\\foo")
       => #T)

(check (relative-path? "C:")
       => #F)
(check (relative-path? "foo\\bar")
       => #T)
(check (relative-path? "C:foo")
       => #T)
(check (relative-path? "\\")
       => #F)
(check (relative-path? "\\foo")
       => #F)
(check (relative-path? "C:\\")
       => #F)
(check (relative-path? "C:\\foo")
       => #F)

(check (path-join)
       => "")
(check (path-join "foo")
       => "foo")
(check (path-join "foo" "bar")
       => "foo\\bar")
(check (path-join "C:" "foo" "bar")
       => "C:\\foo\\bar")
(check (path-join "\\foo" "bar")
       => "\\foo\\bar")
(check (path-join "foo" "\\bar")
       => "foo\\bar")
(check (path-join "C:\\" "foo" "bar")
       => "C:\\foo\\bar")
(check (path-join "C:\\\\" "foo" "bar")
       => "C:\\foo\\bar")
(check (path-join "C:foo" "bar")
       => "C:foo\\bar")
(check (path-join "\\" "foo" "bar")
       => "\\foo\\bar")
(check (path-join "\\" "C:" "bar")
       => "\\C:\\bar")
(check (path-join "C:\\foo" "bar")
       => "C:\\foo\\bar")
(check (path-join "C:\\\\foo\\" "\\bar\\")
       => "C:\\foo\\bar")
(check (path-join "C:" "\\foo" "bar")
       => "C:\\foo\\bar")
(check (path-join "\\")
       => "\\")
(check (path-join "C:\\")
       => "C:\\")
(check (path-join "C:\\\\")
       => "C:\\")
(check (path-join "" "\\foo" "bar")
       => "\\foo\\bar")
(check (path-join "" "C:\\" "bar")
       => "C:\\bar")
(check (path-join "//foo/" "/bar" "//zab/")
       => "//foo/\\/bar\\//zab/")

(check (path-split "")
       => '())
(check (path-split "a")
       => '("a"))
(check (path-split "C:a\\")
       => '("C:a"))
(check (path-split "a\\b")
       => '("a" "b"))
(check (path-split "C:a\\b")
       => '("C:a" "b"))
(check (path-split "\\")
       => '("\\"))
(check (path-split "\\a")
       => '("\\" "a"))
(check (path-split "\\foo\\bar")
       => '("\\" "foo" "bar"))
(check (path-split "C:\\")
       => '("C:\\"))
(check (path-split "C:\\a")
       => '("C:\\" "a"))
(check (path-split "C:\\foo\\bar")
       => '("C:\\" "foo" "bar"))
(check (path-split "C:\\\\foo\\\\bar\\\\")
       => '("C:\\" "foo" "bar"))
(check (path-split "C:/foo/bar/")
       => '("C:/foo/bar/"))

(check (normalize-path "")
       => "")
(check (normalize-path "a")
       => "a")
(check (normalize-path "\\")
       => "\\")
(check (normalize-path "C:\\")
       => "C:\\")
(check (normalize-path "foo\\\\bar")
       => "foo\\bar")
(check (normalize-path "\\\\")
       => "\\")
(check (normalize-path "\\\\foo")
       => "\\foo")
(check (normalize-path "foo\\")
       => "foo")
(check (normalize-path "\\\\\\foo\\\\bar\\")
       => "\\foo\\bar")
(check (normalize-path "C:\\\\")
       => "C:\\")
(check (normalize-path "C:\\\\foo")
       => "C:\\foo")
(check (normalize-path "C:\\\\foo\\\\bar\\zab\\\\")
       => "C:\\foo\\bar\\zab")

(check (path=? "" "")
       => #T)
(check (path=? "\\" "\\\\")
       => #T)
(check (path=? "foo\\" "foo")
       => #T)
(check (path=? "C:\\\\\\" "C:\\\\")
       => #T)
(check (path=? "C:\\foo\\\\bar\\" "C:\\\\foo\\bar\\\\")
       => #T)
(check (path=? "C:foo\\bar" "C:foo\\\\bar")
       => #T)
(check (path=? "\\" "")
       => #F)
(check (path=? "\\foo" "foo")
       => #F)
(check (path=? "C:\\" "C:")
       => #F)
(check (path=? "C:\\foo" "C:foo")
       => #F)


(path-style 'windows-/)

(check (path? "a")
       => #T)
(check (path? "foo/bar")
       => #T)
(check (path? "/foo")
       => #T)
(check (path? "C:/")
       => #T)
(check (path? "C:foo")
       => #T)
(check (path? "C:")
       => #F)
(check (path? "")
       => #F)

(check (absolute-path? "C:")
       => #F)
(check (absolute-path? "foo/bar")
       => #F)
(check (absolute-path? "C:foo")
       => #F)
(check (absolute-path? "/")
       => #T)
(check (absolute-path? "/foo")
       => #T)
(check (absolute-path? "C:/")
       => #T)
(check (absolute-path? "C:/foo")
       => #T)

(check (relative-path? "C:")
       => #F)
(check (relative-path? "foo/bar")
       => #T)
(check (relative-path? "C:foo")
       => #T)
(check (relative-path? "/")
       => #F)
(check (relative-path? "/foo")
       => #F)
(check (relative-path? "C:/")
       => #F)
(check (relative-path? "C:/foo")
       => #F)

(check (path-join)
       => "")
(check (path-join "foo")
       => "foo")
(check (path-join "foo" "bar")
       => "foo/bar")
(check (path-join "C:" "foo" "bar")
       => "C:/foo/bar")
(check (path-join "/foo" "bar")
       => "/foo/bar")
(check (path-join "foo" "/bar")
       => "foo/bar")
(check (path-join "C:/" "foo" "bar")
       => "C:/foo/bar")
(check (path-join "C://" "foo" "bar")
       => "C:/foo/bar")
(check (path-join "C:foo" "bar")
       => "C:foo/bar")
(check (path-join "/" "foo" "bar")
       => "/foo/bar")
(check (path-join "/" "C:" "bar")
       => "/C:/bar")
(check (path-join "C:/foo" "bar")
       => "C:/foo/bar")
(check (path-join "C://foo/" "/bar/")
       => "C:/foo/bar")
(check (path-join "C:" "/foo" "bar")
       => "C:/foo/bar")
(check (path-join "/")
       => "/")
(check (path-join "C:/")
       => "C:/")
(check (path-join "C://")
       => "C:/")
(check (path-join "" "/foo" "bar")
       => "/foo/bar")
(check (path-join "" "C:/" "bar")
       => "C:/bar")
(check (path-join "\\\\foo\\" "\\bar" "\\\\zab\\")
       => "\\\\foo\\/\\bar/\\\\zab\\")

(check (path-split "")
       => '())
(check (path-split "a")
       => '("a"))
(check (path-split "C:a/")
       => '("C:a"))
(check (path-split "a/b")
       => '("a" "b"))
(check (path-split "C:a/b")
       => '("C:a" "b"))
(check (path-split "/")
       => '("/"))
(check (path-split "/a")
       => '("/" "a"))
(check (path-split "/foo/bar")
       => '("/" "foo" "bar"))
(check (path-split "C:/")
       => '("C:/"))
(check (path-split "C:/a")
       => '("C:/" "a"))
(check (path-split "C:/foo/bar")
       => '("C:/" "foo" "bar"))
(check (path-split "C://foo//bar//")
       => '("C:/" "foo" "bar"))
(check (path-split "C:\\foo\\bar\\")
       => '("C:\\foo\\bar\\"))

(check (normalize-path "")
       => "")
(check (normalize-path "a")
       => "a")
(check (normalize-path "/")
       => "/")
(check (normalize-path "C:/")
       => "C:/")
(check (normalize-path "foo//bar")
       => "foo/bar")
(check (normalize-path "//")
       => "/")
(check (normalize-path "//foo")
       => "/foo")
(check (normalize-path "foo/")
       => "foo")
(check (normalize-path "///foo//bar/")
       => "/foo/bar")
(check (normalize-path "C://")
       => "C:/")
(check (normalize-path "C://foo")
       => "C:/foo")
(check (normalize-path "C://foo//bar/zab//")
       => "C:/foo/bar/zab")

(check (path=? "" "")
       => #T)
(check (path=? "/" "//")
       => #T)
(check (path=? "foo/" "foo")
       => #T)
(check (path=? "C:///" "C://")
       => #T)
(check (path=? "C:/foo//bar/" "C://foo/bar//")
       => #T)
(check (path=? "C:foo/bar" "C:foo//bar")
       => #T)
(check (path=? "/" "")
       => #F)
(check (path=? "/foo" "foo")
       => #F)
(check (path=? "C:/" "C:")
       => #F)
(check (path=? "C:/foo" "C:foo")
       => #F)


(check-report)
