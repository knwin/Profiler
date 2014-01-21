;; written by Kyaw Naing Win (kyawnaingwinknw@gmail.com)
;; survey-gis-specialist, Wa Alternative Development Project, UNODC, Mong Pawk, Wa Sepcial Region (2)
;; Jan - 2001
;; this program is for profile drawing by clicking a polyline
;; **WARNING** it does not work with 2D 3D polyline
;; Modification - 17 Jan 2014
;; As recommended by friends, a datum line (top grid line) is placed at suitable height
;;


(defun c:dp ()
	(setq oldCmdecho (getvar "CMDECHO"))
	(setq oldAngbase (getvar "ANGBASE"))
	(setq oldAngdir (getvar "ANGDIR"))
	(setvar "cmdecho" 0)
	(setq txt(getvar "textsize"))
	(setq txtind(/ txt 2))	;indent of label text
	(setq decPrec (getvar "luprec")) ; decimal places for linear unit precision
	(setq selectPlineList (entget (car (entsel))))
	(setq scale (getreal "\n..Please enter V scale factor used in your drawing..>>"))
	(setq elist selectPlineList)
	
	(defun lw_Vertex ()
		(setq i 0) ;counter
		(setq TEMP elist)
		; drawing three horizontal lines at the base of profile
		(while TEMP
			(setq ELEMENT (list (car TEMP)))
			(setq TEMP (cdr TEMP)) ; the rest part of the list without first entity 
			(if (assoc 10 ELEMENT)	;(= (car (assoc 10 ELEMENT)) 10)
				(progn
					(setq PlVertex (cdr (assoc 10 ELEMENT)))
					(setq X (car PlVertex))
					(setq Y (cadr PlVertex))
					(setq i (1+ i))
				)
			)
			(if (= i 1)
				(progn ;starting vertex
					(setq startX x)
					(setq maxY y)
					(setq minY y)
				)
				(progn ; the rest vertexes
					;(if (/= x startX) ;here endX may be negative value
					(setq endX x) ; to catch the X coordinate of end vertex
					;)
					(if (> y maxY)
						(setq maxY y) ; the highets point from which the maximum lenght of the label will be calculated to define the spacing
									  ; - between horizontal grid lines
					;)
					;(if (< y minY)
						(setq minY y) ; the lowest point along the profile so that it make sure labels are alwasys below this point
					)
				)
			)
		)
		(princ (strcat "\n..The lowest R.L in your profile is .. " (rtos(/ minY scale)2 decPrec)))
		(setq minY (*(getreal "\n..Please enter you prefer datum value..>>") scale))
				
		;start and end point of first grid line which is set by user defined height (datum)
		(setq startX (- startX (+(*(strlen (rtos (/ maxY scale) 2 decPrec)) txt) (* 7 txt)))) ; string lenght + 7 (Datumm: ); move to left for datum text
		(setq p1 (list startX minY ))
		(setq p2 (list endX minY))
		
		;start and end point of middle grid line by offesting with a value defined by R.L text length
		
		(setq rowHigh1 (* (strlen (rtos (/ maxY scale) 2 decPrec)) txt))
		(setq p3 (list startX (- minY rowHigh1)))
		(setq p3 (list startX (- minY rowHigh1)))
		(setq p4 (list endX (- minY rowHigh1)))
		
		;start and end point of base grid line by offesting with a value defined by R.L text length & chainage text length
		(setq rowHigh2 (* (strlen (rtos endX 2 decPrec)) txt))
		(setq p5 (list startX (- minY (+ rowHigh1 rowHigh2))))
		(setq p6 (list endX (- minY (+ rowHigh1 rowHigh2))))
			
		;(rtos endX 2 5) "," (rtos maxY 2 5))))
		;(command "rectang" p1 p2); when coordiantes are in exponential format program cannot work any longer 
		
		(command "line" p1 p2 "")
		(command "line" p3 p4 "")
		(command "line" p5 p6 "")
		(command "line" p1 p5 "")
		(setvar "angbase" 0)
		(setvar "angdir" 0)
		(command "text" "j" "BL" (list (+ startX txtind) (+ minY txtind)) txt "0" (strcat "Datum:" (rtos (/  minY scale)2 decPrec))); datum text
		(command "text" "j" "ML" (list (+ startX txtind) (+ minY txtind (/ rowHigh1 -2) )) txt "0" "R.L"); Reduce Level text
		(command "text" "j" "ML" (list (+ startX txtind) (+ minY txtind (/ rowHigh1 -1) (/ rowHigh2 -2) )) txt "0" "Length"); Length text
		(command "text" "j" "BL" (list (+ startX txtind) (- minY (+ txtind txtind txt rowHigh1 rowHigh2))) txt "0" (strcat "Vertical exaggeration factor = " (rtos scale 2 0)));
		(setq TEMP elist)
		; labelling R.L and chaninage
		(while TEMP
			(setq ELEMENT (list (car TEMP)))
			(setq TEMP (cdr TEMP))
			(if (assoc 10 ELEMENT)	;
				(progn
					(setq PlVertex (cdr (assoc 10 ELEMENT)))
					(setq x (car PLVertex))
					(setq y (cadr PLVertex))
					(command "line"
						PlVertex
						(list x (- minY  (+ rowHigh1 rowHigh2)))
						""
					)
					(command "text" "j" "BR" (list x (- minY txtind)) txt "90" (rtos (/ y scale)2 decPrec));High labelling
					(command "text" "j" "BR" (list x (- minY (+ txtind rowHigh1))) txt "90" (rtos x 2 decPrec));Chainage labelling
				)
			)
		)
		(setvar "ANGBASE" oldAngbase)
		(setvar "ANGDIR" oldAngdir)
		(setvar "CMDECHO" oldCmdecho)
		(princ)
	)					; defun Lw_Vertx

	

	;; End of Code to Extract Vertices from LWPOLYLINE


	(if (= (cdr (assoc 0 selectPlineList)) "LWPOLYLINE")
		(lw_vertex)
		(alert "...This Program cannot work with simple Line,2D/3D Polyline \nPlease covert into LWPOLYLINE"\n)
	)
)	; defun dp() end

(alert "loaded from Profile.lsp...

main command: DP
command to set decimal places: LUPREC
command to change textsize: TEXTSIZE
----------------------------------------------
Written by:
	Kyaw Naing Win
	Survey-GIS Specialist, UNODC Wa Project
	Jan - 2001
	email: kyawnaingwinknw@gmail.com")
(princ "...type DP to use...(credit:Kyaw Naing Win)\n....use LUPREC command for decimal precision and TEXTSIZE for text size....")
