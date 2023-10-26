(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "caa92f1d64fc25480551757d854b4b49981dfa6b"
  :removal '("doc")) ; 2.4.1
 ("elfeed" "https://github.com/skeeto/elfeed"
  "362bbe5b38353d033c5299f621fea39e2c75a5e0"  ; 3.4.0
  :removal '("^web$"))
 ("youtube-dl" "https://github.com/skeeto/youtube-dl-emacs"
  "af877b5bc4f01c04fccfa7d47a2c328926f20ef4") ; 2018-10-12T15:08:06Z
 ("evil" "https://github.com/emacs-evil/evil"
  "067a29214d9d4e1fc4b4d29fc596ad4cefe7e492"  ; 2022-04-03T14:59:29Z
  :removal '("evil-tests.el" "^ert.el"))
 ("fasm-mode" "https://github.com/the-little-language-designer/fasm-mode.git"
  "cb4b9bf48c7f05530e140456eb1f4a54716470f9")

 ;; magit & deps
 ("compat" "https://github.com/emacs-compat/compat.git"
  "74300f16a1630a33a86710aa20c1fc26f5f89f75") ; 29.1.4.2
 ("dash" "https://github.com/magnars/dash.el.git"
  "39d067b9fbb2db65fc7a6938bfb21489ad990cb4"
  :removal '("doc" "dev" "Makefile" "Cask")) ; 2.19.1
 ("transient" "https://github.com/magit/transient.git"
  "8cf1238181d57504e68f42fa0d4ef66784b197a9") ; 0.4.3
 ("with-editor" "https://github.com/magit/with-editor.git"
  "d5f3f06cb830311e60c58f656988ef37c05a99e0") ; 3.3.2
 ("magit" "https://github.com/magit/magit.git"
  "f44f6c14500476d918e9c01de8449edb20af4113") ; 3.3.0

;; treemacs & deps
 ("s" "https://github.com/magnars/s.el.git" "1.13.0")
 ("ht" "https://github.com/Wilfred/ht.el.git" "2.3")
 ("ace-window" "https://github.com/abo-abo/ace-window.git" "0.10.0")
 ("all-the-icons" "https://github.com/domtronn/all-the-icons.el.git" "5.0.0")
 ("pfuture" "https://github.com/Alexander-Miller/pfuture.git" "1.10.3")
 ("hydra" "https://github.com/abo-abo/hydra.git" "0.15.0")
 ("cfrs" "https://github.com/Alexander-Miller/cfrs.git" "1.6.0")
 ("projectile" "https://github.com/bbatsov/projectile.git" "v2.8.0")
 ("persp-mode" "https://github.com/Bad-ptr/persp-mode.el.git" "v2.9.8")
 ("perspective" "https://github.com/nex3/perspective-el.git" "2.18")
 ("buttercup" "https://github.com/jorgenschaefer/emacs-buttercup.git" "v1.32")
 ("el-mock" "https://github.com/rejeep/el-mock.el.git" "v1.25.1")
 ("ert-runner" "https://github.com/rejeep/ert-runner.el.git" "v0.8.0")
 ("treemacs" "https://github.com/Alexander-Miller/treemacs.git"
  "3.1"
  :removal '("Makefile" "Cask$" ".org$" "screemshots" "src/scripts$")))

 (add-to-list 'load-path (gpkg-path "evil" "lib"))
 (add-to-list 'load-path (gpkg-path "dash"))
 (add-to-list 'load-path (gpkg-path "transient" "lisp"))
 (add-to-list 'load-path (gpkg-path "with-editor" "lisp"))
 (add-to-list 'load-path (gpkg-path "magit" "lisp"))
 (add-to-list 'load-path (gpkg-path "treemacs" "src" "elisp"))
 (add-to-list 'load-path (gpkg-path "treemacs" "src" "extra"))
