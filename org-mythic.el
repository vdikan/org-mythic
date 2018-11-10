;; Chaos rank is 0 to 8, not 1 to 9, for consistency.


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
    ("U" . "Very unlikely")
    ("u" . "Unlikely")
    ("d" . "50/50 (default)")
    ("D" . "Somewhat likely")
    ("l" . "Likely")
    ("L" . "Very Likely")
    ("s" . "Near sure thing")
    ("S" . "A sure thing")
    ("H" . "Has to be"))
  "Odds chart for core Mythic rules.")


(defun org-mythic-select (code clist)
  "Get the cons cell corresponding to CODE from CLIST."
  (cond
    ((null clist) '("~" . "Undefined"))
    ((equal code (caar clist)) (car clist))
    (t (org-mythic-select code (cdr clist)))))


(defun org-mythic-get-odds (code)
  "Retrn odds cons cell that corresponds to the literal code."
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
  "Find target value on the fate chart with respect to ODDS and CHAOS levels."
  (nth (- 8 chaos-rank)  ; fate chart is reversed with respect to chaos
       (nth odds-rank org-mythic-fate-chart)))


;; (org-mythic-odds-rank
;;  (org-mythic-get-odd  "d"))


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
