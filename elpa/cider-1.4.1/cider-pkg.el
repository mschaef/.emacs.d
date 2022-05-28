(define-package "cider" "1.4.1" "Clojure Interactive Development Environment that Rocks"
  '((emacs "26")
    (clojure-mode "5.14")
    (parseedn "1.0.6")
    (queue "0.2")
    (spinner "1.7")
    (seq "2.22")
    (sesman "0.3.2"))
  :commit "11156e7b0cab470f4aab39d3af5ee3cb1e0b09d0" :authors
  '(("Tim King" . "kingtim@gmail.com")
    ("Phil Hagelberg" . "technomancy@gmail.com")
    ("Bozhidar Batsov" . "bozhidar@batsov.dev")
    ("Artur Malabarba" . "bruce.connor.am@gmail.com")
    ("Hugo Duncan" . "hugo@hugoduncan.org")
    ("Steve Purcell" . "steve@sanityinc.com"))
  :maintainer
  '("Bozhidar Batsov" . "bozhidar@batsov.dev")
  :keywords
  '("languages" "clojure" "cider")
  :url "http://www.github.com/clojure-emacs/cider")
;; Local Variables:
;; no-byte-compile: t
;; End:
