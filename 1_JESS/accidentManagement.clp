; KaiMAX - Mission-Critical Road Accident Management Expert System
; Templates used to compute the severity of victim's condition by the road accident
(deftemplate victim
    ; Input the name of the victim
    (slot victimName)
    ; Input the age of the victim
    (slot age (type INTEGER))
    ; Either Male or Female
    (slot sex (allowed-values M F))
    ; Systolic is the higher number in B.P. reading
    (slot bloodPressureSystolic (type INTEGER))
    ; Diastolic is the lower number in B.P. reading
    (slot bloodPressureDiastolic (type INTEGER))
    ; Average Heart Rate
    (slot avgHR (type INTEGER))
    ; Tells whether the victim is unconscious or not (Yes/No)
    (slot unconsc (allowed-values Y N))
    ; Signifies the severity of bleeding (rate of blood loss)
    (slot bleeding-intensity (type INTEGER))
    ; Indicates the criticality of injury occured
    (slot criticalinjury-degree (type INTEGER))
    ; Degree of Mental Trauma the victim is suffering from
    (slot mentalTrauma (type INTEGER)))

(deftemplate severity
    (slot score))

(deftemplate recommendation
    (slot severity-score)
    (slot explanation))

(deftemplate question
    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer
    (slot ident)
    (slot text))

;Function to interact with the healthcare professional to get the necessary details for evaluation
(deffunction is-of-type (?answer ?type)
    "Check that the answer has the right form"
    (if (eq ?type Yes-No) then
        (return (or (eq ?answer Yes) (eq ?answer No)))
        else (if (eq ?type number) then
            (return (numberp ?answer))
            else (return (> (str-length ?answer) 0)))))

(deffunction ask-user (?question ?type)
    "Ask a question, and return the answer"
    (bind ?answer "")
    (while (not (is-of-type ?answer ?type)) do
        (printout t ?question " ")
        (if (eq ?type yes-no) then
            (printout t "(Yes or No) "))
        (bind ?answer (read)))
    (return ?answer))

(deffunction safetyAdvice(?num)
    (if (< ?num 6478) then
        (return "Drive defensively! Risk takers are collision makers!")
        else (if (< ?num 13114) then
            (return "Be courteous towards fellow road users - keep your temper and resist the temptation to retaliate.")
            else (if (< ?num 26207) then
                (return "Avoid all distractions - Keep both eyes on the road and hands on the steering wheel!")
                else (if (< ?num 39332) then
                    (return "Be especially alert when approaching traffic lights, intersections and level crossings.")
                    else (if (< ?num 45786) then
                        (return "Headlights should be dipped well before an approaching vehicle is within range of the main beam.")
                        else (if (< ?num 55424) then
                            (return "Maintain at least a 3 second following distance - this distance should be increased at night, in foggy or rainy conditions and when the road is wet.")
                            else (if (< ?num 69420) then
                                (return "All lights and indicators, windscreen wipers, brakes, steering, exhaust system and tyres should be carefully examined for faults before embarking on your holiday travels")
                                else
                                (return "Infants between 0 months and one year of age, or up to 10kg in weight, should travel in a rear facing car seat in the back of a car. In the event of an accident, the impact will be on the seat and not on the infant.")))))))))

(deffunction getBldPressRtng(?hgh ?low)
    (if (and (< ?hgh 120) (> ?low 80)) then
        (printout t crlf "The victim's blood pressure levels are normal. No action required on this aspect at least." crlf)
        else (if (and (< ?hgh 139) (> ?low 89)) then
            (printout t crlf "The victim is experiencing pre-hypertension. The condition may become worse if not taken care of at this moment." crlf)
            else (if (and (< ?hgh 159) (> ?low 99)) then
                (printout t crlf "The victim is suffering from stage-1 hypertension. Proper medication is of utmost importance right now." crlf)
                    else (if (and (> ?hgh 160) (> ?low 100)) then
                        (printout t crlf "The victim is experiencing stage-2 hypertension. This is an emergency and he/she should be rushed to the nearest hospital at this instant." crlf)
                    	else (printout t crlf "The victim's blood pressure levels are abnormal. These readings should be notified to the concerned doctor as soon as possible." crlf))))))

(deffunction getavgHR(?avgHR)
    (if (and (< ?avgHR 100) (> ?avgHR 60)) then
        (printout t "The victim's average Heart Rate is in the normal range. No action required on this aspect at least." crlf)
        else (printout t "The victim is suffering from arrythmia. Abnormalities in the heart rate indicate risks of an heart attack and should be brought to notice of the healthcare professional." crlf)))

;Module containing the rule to assert the answers given by the user to the question asked
(defmodule ask)
(defrule ask::ask-question-by-id
    "Ask a question and assert the answer"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?id) (text ?text) (type ?type))
    (not (MAIN::answer (ident ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (ask-user ?text ?type))
    (assert (MAIN::answer (ident ?id) (text ?answer)))
    (retract ?ask)
    (return))

; Startup module for the application that prints the Welcome message
(defmodule application-startup)
(defrule welcome-user
    =>
    (printout t crlf "********************* Welcome to KaiMAX *********************" crlf)
    (printout t "The Mission-critical Road Accident management eXpert system!" crlf crlf)
    (printout t "Please type the name of the victim and press <Enter> " crlf)
    (bind ?name (read))
    (printout t "Please input the age of the victim and press <Enter> " crlf)
    (bind ?age (read))
    (printout t "Please input the gender of the victim and press <Enter> (M or F) " crlf)
    (bind ?sex (read))
    (printout t crlf "Let us assess severity of the accident and begin the evaluation of physical condition for " ?name "." crlf)
    (printout t "Please provide the required information and KaiMAX will recommend you the further steps that should be followed " crlf crlf))

;Facts containing the questions to be asked
(deffacts questions
    "The questions that are asked to the user by the system."
    (question (ident avgHR) (type number)
        (text "What is the average heart rate of the victim now?"))
    (question (ident bloodPressureSystolic) (type number)
        (text "Please tell me the higher reading of the victim's blood pressure? "))
    (question (ident bloodPressureDiastolic) (type number)
        (text "Please tell me the lower reading of the victim's blood pressure? "))
    (question (ident unconsc) (type Yes-No)
        (text "Is the victim unconscious or unable to move? (Yes or No) "))
    (question (ident bleeding-intensity) (type number)
        (text "How intensively is the victim bleeding right now (0-10)?"))
    (question (ident criticalinjury-degree) (type number)
        (text "Has the victim badly injured the sensitive parts of his/her body (head, neck, spine, etc)? Please indicate the severity of critical injury (0-10)?"))
    (question (ident mentalTrauma) (type number)
        (text "Specify the degree of Mental Trauma experienced by the victim? (0-10)")))

/* Module containing rules to request the various details and assert the answers 
based on the different questions asked */
(defmodule request-victim-details)
(defrule request-avgHR
    (declare (salience 27))
    =>
    (assert (ask avgHR)))

(defrule request-bloodPressureSystolic
    (declare (salience 28))
    =>
    (assert (ask bloodPressureSystolic)))

(defrule request-bloodPressureDiastolic
    (declare (salience 29))
    =>
    (assert (ask bloodPressureDiastolic)))

(defrule request-unconsc
    (declare (salience 23))
    =>
    (assert (ask unconsc)))

(defrule request-bleeding-intensity
    (declare (salience 25))
    =>
    (assert (ask bleeding-intensity)))

(defrule request-criticalinjury-degree
    (declare (salience 26))
    =>
    (assert (ask criticalinjury-degree)))

(defrule request-mentalTrauma
    (declare (salience 24))
    =>
    (assert (ask mentalTrauma)))

(defrule assert-victim-fact
    (answer (ident avgHR) (text ?a))
    (answer (ident bloodPressureSystolic) (text ?hbp))
    (answer (ident bloodPressureDiastolic) (text ?lbp))
    (answer (ident unconsc) (text ?u))
    (answer (ident bleeding-intensity) (text ?b))
    (answer (ident criticalinjury-degree) (text ?c))
    (answer (ident mentalTrauma) (text ?mt))
    =>
    (assert (victim (avgHR ?a)  
            (bloodPressureSystolic ?hbp) 
            (bloodPressureDiastolic ?lbp) 
            (unconsc ?u) 
            (bleeding-intensity ?b) 
            (criticalinjury-degree ?c) 
            (mentalTrauma ?mt))))

/* Module containing rules that determine what accident severity and recommendation 
the victim would get depending on the values entered and the various combinations 
of these values in the answers */
(defmodule accident-severity)
(defrule calculate-severity-score
    (victim (avgHR ?a)
        (bloodPressureSystolic ?hbp)
        (bloodPressureDiastolic ?lbp)
        (unconsc ?u)
        (bleeding-intensity ?b)
        (criticalinjury-degree ?c)
        (mentalTrauma ?mt))
    =>
    (bind ?calculated-rating (float (+ (* 0.55 ?c) (* 0.25 ?b) (* 0.2 ?mt))))
    (if (= (str-compare ?u "Yes") 0) then
        (bind ?calculated-rating (+ ?calculated-rating 0.65)))
    (if (and (> ?calculated-rating 6) (<= ?calculated-rating 7)) then
        (assert (recommendation (severity-score ?calculated-rating) 
                (explanation "The victim's condition is satisfactory. However, he/she should get themselves diagnosed thoroughly")))
        elif(and (> ?calculated-rating 7) (<= ?calculated-rating 8)) then
        (assert (recommendation (severity-score ?calculated-rating)
                (explanation "Physical injury and mental trauma of an acceptable degree have been encountered by the victim. He/she will become fit as a fiddle with proper dressing of wounds, painkillers, and some counselling")))
        elif(and (> ?calculated-rating 8) (<= ?calculated-rating 9)) then
        (assert (recommendation (severity-score ?calculated-rating)
                (explanation "Several injuries of extreme nature have been incurred by the victim. He/she must be attended straightaway at the hospital")))
        elif(and (> ?calculated-rating 9) (<= ?calculated-rating 10)) then
        (assert (recommendation (severity-score ?calculated-rating)
                (explanation "Call 911 Urgently! The victim's condition is tremendously severe. Medical aid required promptly; otherwise the condition may further worsen.")))
        elif(and (> ?calculated-rating 10) (<= ?calculated-rating 11)) then
        (assert (recommendation (severity-score ?calculated-rating)
                (explanation "It's an Emergency!.. Immediately rush the victim to an intensive care unit of the nearest hospital or else it can prove to be fatal!")))
        else(assert (recommendation (severity-score ?calculated-rating)
                (explanation "Your condition is tolerable. I recommend you to consult your physician for proper medication if required")))))

; Module that contains the rules to print out the final result of the evaluation
(defmodule result)

; Rule to check if the victim has a high/low blood pressure
(defrule checkBldPressLvls
    (declare (salience 22))
	?v <- (victim (victimName ?name))
    =>
    (getBldPressRtng ?v.bloodPressureSystolic ?v.bloodPressureDiastolic))

; Rule to check the average heart rate of the victim
(defrule checkavgHR
    (declare (salience 22))
	?v <- (victim (victimName ?name))
    =>
    (getavgHR ?v.avgHR))

; Function to print the final accident severity score
(defrule print-result
    (declare (salience 21))
    ?p1 <- (recommendation (severity-score ?r1) (explanation ?ex))
    =>
    (printout t crlf "*** The road accident severity score for this victim is: " ?r1 crlf)
    (printout t "Recommendation: " ?ex crlf crlf)
    (printout t "Road Safety Tip: " (safetyAdvice (random)) crlf)
    (printout t "Be Healthy. Stay Safe!" crlf "Hope to not see you again!" crlf "Thank you for using KaiMAX... " crlf))

;Function to run the various modules of the application in the correct order
(deffunction run-application ()
    (reset)
    (focus application-startup request-victim-details accident-severity result)
    (run))

/* Run the above function in a loop to get back the prompt every time we have to 
enter the values for another victim or re-run the program */
(while TRUE 
    (run-application))
;    (run-application)
