(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "caa92f1d64fc25480551757d854b4b49981dfa6b") ; 2.4.1
 ("elfeed" "https://github.com/skeeto/elfeed"
  "362bbe5b38353d033c5299f621fea39e2c75a5e0"  ; 3.4.0
  :removal '("^web$"))
 ("youtube-dl" "https://github.com/skeeto/youtube-dl-emacs"
  "af877b5bc4f01c04fccfa7d47a2c328926f20ef4") ; 2018-10-12T15:08:06Z
 ("org-bullets" "https://github.com/sabof/org-bullets.git"
  "b70ac2ec805bcb626a6e39ea696354577c681b36")
 ("dash" "https://github.com/magnars/dash.el.git"
  "dc61f4641779616122692e34a32ba2a158ee034c")
 ("s" "https://github.com/magnars/s.el.git"
  "08661efb075d1c6b4fa812184c1e5e90c08795a9")
 ("elfeed-org" "https://github.com/remyhonig/elfeed-org.git"
  "268efdd0121fa61f63b722c30e0951c5d31224a4"
  :removal '("Cask" "Makefile" "README.md"))) ; Depends: dash s

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "lib"))
