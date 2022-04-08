(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "caa92f1d64fc25480551757d854b4b49981dfa6b") ; 2.4.1
 ("elfeed" "https://github.com/skeeto/elfeed"
  "362bbe5b38353d033c5299f621fea39e2c75a5e0"  ; 3.4.0
  :removal '("^web$"))
 ("youtube-dl" "https://github.com/skeeto/youtube-dl-emacs"
  "af877b5bc4f01c04fccfa7d47a2c328926f20ef4") ; 2018-10-12T15:08:06Z
 ("evil" "https://github.com/emacs-evil/evil"
  "067a29214d9d4e1fc4b4d29fc596ad4cefe7e492"  ; 2022-04-03T14:59:29Z
  :removal '("evil-tests.el" "^ert.el")))

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "evil" "lib"))
