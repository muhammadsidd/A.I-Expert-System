
;;;======================================================
;;;   PC DIAGNOSTIC Expert System DESIGNED BY MUHAMMAD SIDDIQUI AND ARASAY PEREZ
;;;
;;;     This expert system to troubleshoot a computer and tell the user the Repair needed.
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;;***************
;;;* CLASSES *
;;;***************

;HARDWARE Class to monitor the hardware problems associated with it such as light fannoise coilwhine

(defclass HARDWARE (is-a USER)
	
	(slot light)
	(slot fannoise)
	(slot coilwhine)
	(slot problem)
	)

;SOFTWARE class to monitor the software problems associated with the it features problem such as blue screen and flickering 

(defclass SOFTWARE(is-a USER)
	
	(slot bluescreen)
	(slot flickering)
	(slot bootfailure)
	)

;Following are the two instances of the object listed to meet the requirements.

(definstances HARDWARE-INSTANCES
	(firstsystem of HARDWARE (light red) (fannoise yes))
	(secondsystem of HARDWARE (light yellow) (coilwhine yes))
)
(definstances SOFTWARE-INSTANCES
	(firstapp of SOFTWARE (bluescreen yes) (flickering yes))
	(secondapp of SOFTWARE (bluescreen no) (flickering yes))
)


;;****************
;;* DEFFUNCTIONS *
;;****************

;Function allows user to answer the questions it can be a vlue answer or a yes or no
;both functions are listed below

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* QUERY RULES *
;;;***************

;if the computer does not turn on then repair should be done by a technician

(defrule determine-computer-state ""
   (not (computer-starts ?))
   (not (repair ?))
   =>
   (assert (computer-starts (yes-or-no-p "Does the computer turn on (yes/no)? ")))
   )

;if the computer runs normally no additional questions will be asked 
   
(defrule determine-runs-normally ""
   (computer-starts yes)
   (not (repair ?))
   =>
   (assert (runs-normally (yes-or-no-p "Does the computer run normally (yes/no)? ")))
   )

;few Query Question to determine the stages and problem associated with the PC by a forward chaining system
  
(defrule determine-coilwhine ""
   (runs-normally no)
   (not (repair ?))
   =>
   (assert (coilwhine (yes-or-no-p "Does the coilwhine (yes/no)? ")))
   )


;Using assert functions, these rules are asserted into fact list and based on that dicisions are taken 

(defrule determine-light ""
   (runs-normally no)
   (not (repair ?))
   =>
   (assert (light (ask-question "What is the light of the state (red/yellow)? " red yellow)))
	
	)
;Rule to for level of fan noise determined by user input 
(defrule determine-fannoise ""
   (runs-normally no)
   (not (repair ?))
   =>
   (assert (fannoise (yes-or-no-p "Is the fan noise loud (yes/no)? ")))
   )

;Rule for blue screen of death determined by user input 
(defrule determine-bluescreen ""
   (runs-normally no)
   (not (repair ?))
   =>
   (assert (bluescreen (yes-or-no-p "Does the Computer show blue screen on loading (yes/no)? ")))
   )

;Rule for a flickering screen determined by user input 
(defrule determine-flickering ""
   (runs-normally no)
   (not (repair ?))
   =>
   (assert (flickering (yes-or-no-p "Does the Computer screen flicker on loading (yes/no)? ")))
   )
   
;Bootfailure depends on the previous 2 rules 
(defrule determine_boot_failure
	(runs-normally no)
	(bluescreen yes)
	(not (repair ?))  
   =>
   (assert (bootfailure (yes-or-no-p "Does the Computer fail to boot (yes/no)? ")))
   )
   
;System failure depends on the previous rules 
(defrule system_failure
    (runs-normally no)
	(bootfailure no)
	(not (repair ?))  
   =>
   (assert (systemfailure (yes-or-no-p "Does the system i/o fail (yes/no)? ")))
   )

;RULES FOR THE OBJECT INSTANCES BASED ON USER INPUT -> WILL DIRECTLY LEAD TO CONCLUSION IF RAN SIMPULATION

;RULE FOR AUTO SHUTDOWN WILL BE DETERMINED BY THE OBJECT WHICH IS CREATED BY THE USER HIMSELF
(defrule determine_auto_shutdown
   ?ins <- (object (is-a HARDWARE) (light red) (coilwhine yes) (fannoise yes))

   =>
   (send ?ins put-problem severe)
   (assert (autoshutdown yes))
   )

;RULE FOR AUTO RESTART IS ALSO OBJECT BASED DEFINED BY THE USER 
   
(defrule determine_auto_restart
   ?ins <- (object (is-a HARDWARE) (light yellow) (fannoise no)) 
   =>
   (send ?ins put-problem minor)
   (assert (autorestart yes))
   )

;GLITCH OBJECT RULE
(defrule determine_glitches 
  ?ins <- (object (is-a SOFTWARE) (bluescreen yes) (flickering yes))
   
  =>
  (send ?ins put-systemfailure yes)
  (assert (glitches yes))
  ) 


;LAG OBJECT RULE 
 (defrule determine_lag 
  ?ins <- (object (is-a SOFTWARE) (bluescreen no) (flickering yes))
  =>
  (send ?ins put-systemfailure no)
  (assert (lag yes))
  )

;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule normal-computer-state-conclusions ""
   (runs-normally yes)
   (not (repair ?))
   =>
   (assert (repair "No repair needed."))
   )

(defrule autoshutdown ""
	(systemfailure yes)
	(not (repair ?))
   =>
   (assert (repair "Change RAM DIMMS."))
   ) 

(defrule autorestart ""
   (autorestart yes)
   (not (repair ?))
   =>
   (assert (repair "PC Components Loose."))
   )     

(defrule lag ""
   (lag yes)
   (not (repair ?))
   =>
   (assert (repair "Clear Cache."))
   )

(defrule bluescreen ""
   (glitches yes)
   (not (repair ?))
   =>
   (assert (repair "Clean Disk Drive"))
   )

(defrule bootfailure ""
   (bootfailure yes)
   (light yellow)
   (not (repair ?))
   =>
   (assert (repair "Bios reset"))
   )
   
(defrule flickering ""
   (flickering yes)
   (light red)
   (not (repair ?))
   =>
   (assert (repair "OVERHEATING -->> Underclock the system NOW"))
   )
   
(defrule flickering2 ""
   (flickering yes)
   (light yellow)
   (not (repair ?))
   =>
   (assert (repair "Overclock the system"))
   )

(defrule systemfailure ""
   (systemfailure yes)
   (not (repair ?))
   =>
   (assert (repair "Reinstall the Operating System."))
   )

(defrule no-repairs ""
  (declare (salience -10))
  (not (repair ?))
  =>
  (assert (repair "Take your PC to a technician."))
  )

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The Computer Troubleshooting Expert System")
  (printout t crlf crlf))

(defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested Repair:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))

