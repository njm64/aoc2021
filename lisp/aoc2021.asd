(defsystem :aoc2021
  :author "Nick Maher"
  :description "Advent of Code 2021"
  :depends-on (:alexandria
               :str
               :split-sequence
               :trivia
               :arrow-macros
               :cl-ppcre
               :metabang-bind
               :cl-heap)
  :components (
               (:file "aoc")
               (:file "util")
               (:file "day1" :depends-on ("util"))
               (:file "day2" :depends-on ("util"))
               (:file "day3" :depends-on ("util"))
               (:file "day4" :depends-on ("util"))
               (:file "day5" :depends-on ("util"))
               (:file "day6" :depends-on ("util"))
               (:file "day7" :depends-on ("util"))
               (:file "day8" :depends-on ("util"))
               (:file "day9" :depends-on ("util"))
               (:file "day10" :depends-on ("util"))
               (:file "day11" :depends-on ("util"))
               (:file "day12" :depends-on ("util"))
               (:file "day13" :depends-on ("util"))
               (:file "day14" :depends-on ("util"))
               (:file "day15" :depends-on ("util"))
               (:file "day16" :depends-on ("util"))))
