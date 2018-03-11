(DEFPACKAGE :ROSWELL.INIT.CL-WEB-2D-GAME
  (:USE :CL))
(IN-PACKAGE :ROSWELL.INIT.CL-WEB-2D-GAME)
(DEFVAR *PARAMS*
  '(:FILES
    ((:NAME "static/css/viewer.css" :METHOD "copy" :REWRITE
      "{{name}}/static/css/viewer.css")
     (:NAME "src/game/_name_-state.lisp" :METHOD "djula" :REWRITE
      "{{name}}/src/game/{{name}}-state.lisp")
     (:NAME "_name_-test.asd" :METHOD "djula" :REWRITE
      "{{name}}/{{name}}-test.asd")
     (:NAME "_name_.asd" :METHOD "djula" :REWRITE "{{name}}/{{name}}.asd")
     (:NAME "qlfile.lock" :METHOD "djula" :REWRITE "{{name}}/qlfile.lock")
     (:NAME "README.markdown" :METHOD "djula" :REWRITE
      "{{name}}/README.markdown")
     (:NAME ".gitignore" :METHOD "djula" :REWRITE "{{name}}/.gitignore")
     (:NAME "qlfile" :METHOD "djula" :REWRITE "{{name}}/qlfile")
     (:NAME "t/_name_.lisp" :METHOD "djula" :REWRITE
      "{{name}}/t/{{name}}.lisp")
     (:NAME "src/_name_.lisp" :METHOD "djula" :REWRITE
      "{{name}}/src/{{name}}.lisp")
     (:NAME "src/game/game.lisp" :METHOD "djula" :REWRITE
      "{{name}}/src/game/game.lisp")
     (:NAME "src/server.lisp" :METHOD "djula" :REWRITE
      "{{name}}/src/server.lisp"))))
(DEFUN CL-WEB-2D-GAME (_ &REST R)
  (ASDF/OPERATE:LOAD-SYSTEM :ROSWELL.UTIL.TEMPLATE :VERBOSE NIL)
  (FUNCALL (READ-FROM-STRING "roswell.util.template:template-apply") _ R
           *PARAMS*))
