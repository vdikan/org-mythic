;;; org-mythic.el --- Mythic GM emulator minor for Org-mode


;;; Commentary:
;; Chaos rank is 0 to 8, not 1 to 9, for consistency.
;;

(require 'org)
(require 'ido)


;;; Code:

(defconst org-mythic-fate-chart
  '(( 50   25  15  10  5    5   0   0 -20 )
    ( 75   50  35  25  15  10   5   5   0 )
    ( 85   65  50  45  25  15  10   5   5 )
    ( 90   75  55  50  35  20  15  10   5 )
    ( 95   85  75  65  50  35  25  15  10 )
    ( 95   90  85  80  65  50  45  25  20 )
    (100   95  90  85  75  55  50  35  25 )
    (105   95  95  90  85  75  65  50  45 )
    (115  100  95  95  90  80  75  55  50 )
    (125  110  95  95  90  85  80  65  55 )
    (145  130 100 100  95  95  90  85  80 ))
  "This two dimensional list cross references the odds of the acting rank versus the difficulty rank.")


(defconst org-mythic-odds
  '(("I" . "Impossible")
    ("N" . "No way")
    ("U_" . "Very unlikely")
    ("u" . "Unlikely")
    ("d" . "50/50 (default)")
    ("D_" . "Somewhat likely")
    ("l" . "Likely")
    ("L_" . "Very Likely")
    ("s" . "Near sure thing")
    ("S_" . "A sure thing")
    ("H" . "Has to be"))
  "Odds chart for core Mythic rules.")


(defun org-mythic-select (code clist)
  "Get the cons cell corresponding to CODE from CLIST."
  (cond
    ((null clist) '("~" . "Undefined"))
    ((equal code (caar clist)) (car clist))
    (t (org-mythic-select code (cdr clist)))))


(defun org-mythic-get-odds (code)
  "Retrn odds cons cell that corresponds to the literal CODE."
  (org-mythic-select code org-mythic-odds))


(defun org-mythic-odds-rank (odds)
  "Find a fate-chart rank for ODDS cons cell, if it's legal ODDS."
  (funcall
   (lambda (elem list)
     "...later I found out about `cl-position', but to hell with it!"
     (let ((mem (member elem list)))
       (if mem
           (- (length list) (length mem))
         -1)))
   odds org-mythic-odds))


(defun org-mythic-answer-roll (result target)
  "Get Mythic answer for roll RESULT against Fate chart TARGET."
  (cond
    ((<= result (/ target 5)) "Exceptional Yes")
    ((<= result target) "Yes")
    ((< result (1+ (- 100 (/ (- 100 target) 5)))) "No")
    (t "Exceptional No")))


(defun org-mythic-find-target (odds-rank chaos-rank)
  "Find target value on the fate chart with respect to ODDS-RANK and CHAOS-RANK levels."
  (nth (- 8 chaos-rank)  ; fate chart is reversed with respect to chaos
       (nth odds-rank org-mythic-fate-chart)))


(defconst org-mythic-events
  '((7   . "Remote event")
    (28  . "NPC action")
    (35  . "Introduce a new NPC")
    (45  . "Move towards a thread")
    (52  . "Move away from a thread")
    (55  . "Close a thread")
    (67  . "PC negative")
    (75  . "PC positive")
    (83  . "Ambiguous event")
    (92  . "NPC negative")
    (100 . "NPC positive"))
  "Random events' focuses and thresholds.")


(defconst org-mythic-event-actions
  '("Attainment"  "Starting"       "Neglect"     "Fight"
    "Recruit"     "Triumph"        "Violate"     "Oppose"
    "Malice"      "Communicate"    "Persecute"   "Increase"
    "Decrease"    "Abandon"        "Gratify"     "Inquire"
    "Antagonise"  "Move"           "Waste"       "Truce"
    "Release"     "Befriend"       "Judge"       "Desert"
    "Dominate"    "Procrastinate"  "Praise"      "Separate"
    "Take"        "Break"          "Heal"        "Delay"
    "Stop"        "Lie"            "Return"      "Imitate"
    "Struggle"    "Inform"         "Bestow"      "Postpone"
    "Expose"      "Haggle"         "Imprison"    "Release"
    "Celebrate"   "Develop"        "Travel"      "Block"
    "Harm"        "Debase"         "Overindulge" "Adjourn"
    "Adversity"   "Kill"           "Disrupt"     "Usurp"
    "Create"      "Betray"         "Agree"       "Abuse"
    "Oppress"     "Inspect"        "Ambush"      "Spy"
    "Attach"      "Carry"          "Open"        "Carelessness"
    "Ruin"        "Extravagance"   "Trick"       "Arrive"
    "Propose"     "Divide"         "Refuse"      "Mistrust"
    "Deceive"     "Cruelty"        "Intolerance" "Trust"
    "Excitement"  "Activity"       "Assist"      "Care"
    "Negligence"  "Passion"        "Work hard"   "Control"
    "Attract"     "Failure"        "Pursue"      "Vengeance"
    "Proceedings" "Dispute"        "Punish"      "Guide"
    "Transform"   "Overthrow"      "Oppress"     "Change")
  "List of actions for random events.")


(defconst org-mythic-event-subjects
  '("Goals"          "Dreams"            "Environment"       "Outside"
    "Inside"         "Realities"         "Allies"            "Enemies"
    "Evil"           "Good"              "Emotions"          "Opposition"
    "War"            "Peace"             "The innocent"      "Love"
    "The spiritual"  "The intellectual"  "New ideas"         "Joy"
    "Messages"       "Energy"            "Balance"           "Tension"
    "Friendship"     "The physical"      "A project"         "Pleasures"
    "Pain"	     "Possessions"       "Benefits"          "Plans"
    "Expectations"   "Legal matters"     "Bureaucracy"       "Lies"
    "Business"       "A plan"            "News"              "Exterior factors"
    "Advice"         "A plot"            "Competition"       "Prison"
    "Illness"        "Food"              "Attention"         "Success"
    "Failure"        "Travel"            "Jealously"         "Dispute"
    "Home"           "Investment"        "Suffering"         "Wishes"
    "Tactics"        "Stalemate"         "Randomness"        "Misfortune"
    "Death"          "Disruption"        "Power"             "A burden"
    "Intrigues"      "Fears"             "Ambush"            "Rumour"
    "Wounds"         "Extravagance"      "A representative"  "Adversities"
    "Opulance"       "Liberty"           "Military"          "The mundane"
    "Trials"         "Masses"            "Vehicle"           "Art"
    "Victory"        "Dispute"           "Riches"            "Status quo"
    "Technology"     "Hope"              "Magic"             "Illusions"
    "Portals"        "Danger"            "Weapons"           "Animals"
    "Weather"        "Elements"          "Nature"            "The public"
    "Leadership"     "Fame"              "Anger"             "Information")
  "List of subjects for random events.")


(defun org-mythic-d100 ()
  "Return a random value between 1 and 100."
  (1+ (random 99)))


(defun org-mythic-threshold (num clist)
  "Get the threshold value corresponding to NUM from CLIST."
  (cond
    ((null clist) "Undefined")
    ((< num (caar clist)) (cdar clist))
    (t (org-mythic-threshold num (cdr clist)))))


(defun org-mythic-get-focus ()
  "Shortcut for random event focus."
  (org-mythic-threshold (org-mythic-d100)
                        org-mythic-events))


(defun org-mythic-random-element (list)
  "Return a random element from the LIST."
  (nth (random (1- (length list))) list))


(defun org-mythic-get-action ()
  "Shortcut for random action."
  (org-mythic-random-element org-mythic-event-actions))


(defun org-mythic-get-subject ()
  "Shortcut for random subject."
  (org-mythic-random-element org-mythic-event-subjects))


(defun org-mythic-construct-event ()
  "Construct an org property-drawer content of a random event."
  `((:key "FOCUS" :value ,(org-mythic-get-focus))
    (:key "ACTION" :value ,(org-mythic-get-action))
    (:key "SUBJECT" :value ,(org-mythic-get-subject))))


(defun org-mythic-begins-list (tag)
  "Return list of entryies' begin-points for a TAG apart of `org_mythic'."
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (hl)
      (and (member "org_mythic" (org-element-property :tags hl))
           (member tag (org-element-property :tags hl))
           (org-element-property :begin hl)))))


(defun org-mythic-boundary (num nlist)
  "Get the left boundary of an interval containing NUM from NLIST.
If not found, jump to the end."
  (cond
    ((null (cdr nlist)) (car nlist))
    ((and (<= (car nlist) num)
          (> (cadr nlist) num)) (car nlist))
    (t (org-mythic-boundary num (cdr nlist)))))


(defun org-mythic-find-previous-scene ()
  "Parse element tree to get previous Mythic scene.
You need to have root headline of a scenario."
  (let ((begins-list (org-mythic-begins-list "scene"))
        (this-begin (org-element-property :begin
                                          (org-element-at-point))))
    (let ((prev-scene-begin (org-mythic-boundary
                             this-begin begins-list)))
      (if (numberp prev-scene-begin)
          (progn (goto-char prev-scene-begin)
                 (org-element-at-point))
        (progn                          ; workaround for a first scene
          (message "First scene: Place in the root Headline of a Scenario")
          (org-element-at-point))))))


(defun org-mythic-insert-next (endpoint title level tags property-list placeholder)
  (progn
    (goto-char endpoint)
    (newline)
    (insert (org-element-interpret-data
             `(headline (:title ,title :level ,level :tags ,tags)
                        (property-drawer nil
                                         ,(mapcar #'(lambda (x) `(node-property ,x))
                                                  property-list))
                        (,placeholder))))))


(defun org-mythic-insert-random-event (endpoint level)
  (org-mythic-insert-next endpoint "_Random Event_" level '("org_mythic" "event")
                          (org-mythic-construct-event) "~~ event desccription ~~"))


(defun org-mythic-add-scene (title)
  "Add new scene to the current scenario."
  (interactive "*sScene Setup: ")
  (let ((endpoint (org-element-property
                   :end (org-element-at-point)))  ; Remember endpoint we'll start at
        (prev-scene (org-mythic-find-previous-scene)))  ; Find previous scene (moves point)
    (let ((chaos-factor (if (org-element-property :CHAOS prev-scene)                     ;^
                            (string-to-int (org-element-property                         ;|
                                            :CHAOS prev-scene))                          ;|
                          4))  ; Default chaos factor level                              ;|
          (chaos-roll (random 9))                                                        ;|
          (level (or (org-current-level) 1)))  ; On the level with previous scene -------;|

      (cond

        ((> chaos-roll
            chaos-factor)
         (progn
           (message (format "Chaos at bay (rolled %d)" chaos-roll))
           (org-mythic-insert-next endpoint title level
                                   '("org_mythic" "scene")
                                   `((:key "CHAOS" :value ,chaos-factor)
                                     (:key "CHAOS_ROLL" :value ,chaos-roll))
                                   "~~ scene description ~~")))

        ((and (<= chaos-roll chaos-factor)
              (= (% chaos-roll 2) 1))
         (let ((altered-title (read-string
                               (format "Scene Alters! Develop your idea: %s - "
                                       title))))
           (message (format "Chaos alteres scene %s (rolled %d)"
                            title chaos-roll))
           (org-mythic-insert-next endpoint altered-title level
                                   '("org_mythic" "scene")
                                   `((:key "CHAOS" :value ,chaos-factor)
                                     (:key "CHAOS_ROLL" :value ,chaos-roll))
                                   "~~ scene description ~~")))

        (t
         (progn
           (message (format "Chaos Widens! Scene interrupted! (rolled %d)" chaos-roll))
           (org-mythic-insert-random-event endpoint (1+ level))
           (org-mythic-insert-next endpoint (format "%s *(interrupted)*" title)
                                   level '("org_mythic" "scene")
                                   `((:key "CHAOS" :value ,chaos-factor)
                                     (:key "CHAOS_ROLL" :value ,chaos-roll))
                                   "~~ scene description ~~")))))))


(defun org-mythic-pick-odds ()
  "Prompt user to pick question odds from a list."
  (interactive)
  (let ((choices (mapcar 'car org-mythic-odds)))
    (ido-completing-read "Pick odds for a question: "
                         choices t t nil nil)))


(defun org-mythic-eventp (roll chaos-rank)
  (if (= (% roll 11) 0)
      (if (<= (/ roll 11) chaos-rank) t)))


(defun org-mythic-answer-question ()
  "Turn current headline into a Mythic question."
  (interactive)
  (let ((question (org-element-at-point))
        (startpoint (org-element-property :begin (org-element-at-point)))
        (endpoint (org-element-property :end (org-element-at-point)))
        (level (org-current-level))
        (scene (org-mythic-find-previous-scene)))
    (let ((chaos-rank (string-to-int (org-element-property :CHAOS scene)))
          (odds-rank  (org-mythic-odds-rank
                       (org-mythic-get-odds (org-mythic-pick-odds)))))
      (let ((roll (org-mythic-d100))
            (target (org-mythic-find-target odds-rank chaos-rank)))
        (goto-char startpoint)                 ; return back to the question
        (org-set-tags-to "org_mythic:question")
        (org-entry-put startpoint "ROLL" (number-to-string roll))
        (org-entry-put startpoint "TARGET" (number-to-string target))
        (org-entry-put startpoint "ANSWER" (org-mythic-answer-roll roll target))
        (goto-char endpoint)
        (if (org-mythic-eventp roll chaos-rank)
            (progn
              (message (format "Chaos Invades! New Random Event! (rolled %d)" roll))
              (org-mythic-insert-random-event endpoint (1+ level))
              (goto-char endpoint)))))))


(defvar org-mythic-mode-map (make-sparse-keymap)
  "Keymap for `org-mythic-mode', a minor mode for Org.")


(define-minor-mode org-mythic-mode
  "Minor simple Mythic RPG scenarios mode for Org."
  :lighter " Mythic" :keymap org-mythic-mode-map
  ;; (define-key org-mythic-mode-map "a" #'org-mythic-add-scene)
  ;; (add-hook 'org-mythic-mode-hook 'org-mode)
  )


(provide 'org-mythic)

;;; org-mythic.el ends here
