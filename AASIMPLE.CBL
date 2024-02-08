       IDENTIFICATION DIVISION.
           PROGRAM-ID. AASIMPLE.
           AUTHOR. KSWILSON.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNITLIST.
           02 UNITS PIC X(17) OCCURS 14 TIMES

       01 BATTLEINFO
            02 BATTLETYPE  PIC X.
               88 LAND-TRUE VALUE '1'.
               88 SEA-TRUE  VALUE  '2'.
      * Note: if sea units are added in a land battle, assumes amphib assault
      * If land units added in sea battle, assumed carried on transport
            02 SIMBATTLE
                03 SIMBATTLEROUNDS PIC 999999999.
                03 ATTKVICTORIES   PIC 999999999.
                03 DEFVICTORIES    PIC 999999999.
                03 AVATTKCOST      PIC 99999.
                03 AVDEFCOST       PIC 99999.
      * To track metrics on large numbers of simulated battles
            02 INDVBATTLE
                03 ATTKCOST PIC 99999.
                03 DEFCOST  PIC 99999.
      * Working-memory to track incurred costs each battle


       01 ATTACKARMY
           02 TROOPS OCCURS 14 TIMES.
      * In order as appears on unit card
      * Infantry, Artillery, Tank, AAgun, Fighter, Bomber, Submarine, Transport,
      * Destroyer, Cruiser, ACCarier, Battleship
               03 TROOPCOUNT PIC 99
           02 POLICY PIC 99 OCCURS 14 TIMES.
           02 SPECIAL PIC X
      * Whether tech advancements have been made which affect unit strength/cost
               03 AARTILLERY PIC X.
                   88 AARTILLERY-TRUE VALUE 'Y'.
               03 SSUBMARINE PIC X.
                   88 SSUBMARINE-TRUE VALUE 'Y'.
               03 JETFIGHTER PIC X.
                   88 JETFIGHTER-TRUE VALUE 'Y'.
               03 IMPRVDSHIP PIC X.
                   88 IMPRVDSHIP-TRUE VALUE 'Y'.
               03 AAGUNRADAR PIC X.
                   88 AAGUNRADAR-TRUE VALUE 'Y'.
               03 HEAVYBOMBR PIC X.
                   88 HEAVYBOMBR-TRUE VALUE 'Y'.

       01 DEFENDARMY
           02 TROOPS OCCURS 14 TIMES.
               03 TROOPCOUNT PIC 99
           02 POLICY PIC 99 OCCURS 14 TIMES.
           02 SPECIAL.
               03 AMPHIBIOUS PIC X.
                   88 AMPHIBIOUS-TRUE VALUE 'Y'.
               03 AARTILLERY PIC X.
                   88 AARTILLERY-TRUE VALUE 'Y'.
               03 SSUBMARINE PIC X.
                   88 SSUBMARINE-TRUE VALUE 'Y'.
               03 JETFIGHTER PIC X.
                   88 JETFIGHTER-TRUE VALUE 'Y'.
               03 IMPRVDSHIP PIC X.
                   88 IMPRVDSHIP-TRUE VALUE 'Y'.
               03 AAGUNRADAR PIC X.
                   88 AAGUNRADAR-TRUE VALUE 'Y'.
               03 HEAVYBOMBR PIC X.
                   88 HEAVYBOMBR-TRUE VALUE 'Y'.

       01 ARMYINUSE
           02 TROOPS OCCURS 14 TIMES.
               03 TROOPCOUNT PIC 99
           02 POLICY PIC 99 OCCURS 14 TIMES.
           02 SPECIAL.
               03 AARTILLERY PIC X.
                   88 AARTILLERY-TRUE VALUE 'Y'.
               03 SSUBMARINE PIC X.
                   88 SSUBMARINE-TRUE VALUE 'Y'.
               03 JETFIGHTER PIC X.
                   88 JETFIGHTER-TRUE VALUE 'Y'.
               03 IMPRVDSHIP PIC X.
                   88 IMPRVDSHIP-TRUE VALUE 'Y'.
               03 AAGUNRADAR PIC X.
                   88 AAGUNRADAR-TRUE VALUE 'Y'.
               03 HEAVYBOMBR PIC X.
                   88 HEAVYBOMBR-TRUE VALUE 'Y'.

       77 CHOOSEVAR PIC 9.
       77 IDX       PIC 99.
       77 UIDX      PIC 99.

       02 YNVAR PIC X.
           88 YNVAL YNVAL 'Y'

       PROCEDURE DIVISION.
       MOVE 'INFANTRY' TO UNITS(1)
       MOVE 'ARTILLERY' TO UNITS(2)
       MOVE 'TANK' TO UNITS(3)
       MOVE 'AA GUN' TO UNITS(4)
       MOVE 'FIGHTER' TO UNITS(5)
       MOVE 'BOMBER' TO UNITS(6)
       MOVE 'SUBMARINE' TO UNITS(7)
       MOVE 'TRANSPORT' TO UNITS(8)
       MOVE 'DESTROYER' TO UNITS(9)
       MOVE 'CRUISER' TO UNITS(10)
       MOVE '2HP AC CARRIER' TO UNITS(11)
       MOVE '1HP AC CARRIER' TO UNITS(12)
       MOVE '2HP BATTLESHIP' TO UNITS(13)
       MOVE '1HP BATTLESHIP' TO UNITS(14)

       MAIN.


       BUILDARMY.
           DISPLAY "ENTER TECHNOLOGICAL ADVANCES REACHED"
           PERFORM GETSPECIAL.
           DISPLAY "ENTER UNIT NUMBER VALUES"
           PERFORM GETUNITS VARYING IDX FROM 1 BY 1 UNTIL IDX > 14.
           PERFORM GETORDER.

       GETSPECIAL.
           DISPLAY "ADVANCED ARTILLERY ACHIEVED? (Y/N)"
           DISPLAY "> "
           ACCEPT AARTILLERY IN ARMYINUSE.
           DISPLAY "SUPER SUBMARINES ACHIEVED? (Y/N)"
           DISPLAY "> "
           ACCEPT SSUBMARINE IN ARMYINUSE.
           DISPLAY "BETTER JET FIGHTERS ACHIEVED? (Y/N)"
           DISPLAY "> "
           ACCEPT JETFIGHTER IN ARMYINUSE.
           DISPLAY "IMPROVED SHIPYARDS ACHIEVED? (Y/N)"
           DISPLAY "> "
           ACCEPT IMPRVDSHIP IN ARMYINUSE.
           DISPLAY "AA GUN RADAR ACHIEVED? (Y/N)"
           DISPLAY "> "
           ACCEPT AAGUNRADAR IN ARMYINUSE.
           DISPLAY "HEAVY BOMBERS ACHIEVED? (Y/N)"
           DISPLAY "> "
           ACCEPT HEAVYBOMBR IN ARMYINUSE.


       GETUNITS.
           DISPLAY "HOW MANY"UNITS(IDX)"UNITS? "
           DISPLAY "> "
           ACCEPT TROOPS(IDX) IN ARMYINUSE

       GETORDER.
           DISPLAY "CHOOSE POLICY FOR REMOVING UNITS".
           DISPLAY "1. LOWEST ATTACK FIRST".
           DISPLAY "2. LOWEST DEFENSE FIRST".
           DISPLAY "3. LOWEST COST FIRST".
           DISPLAY "4. MANUAL ENTRY".
           DISPLAY "> ".
           ACCEPT CHOOSEVAR.

           EVALUATE CHOOSEVAR
                WHEN 1 MOVE '1113011202090703100506140804' TO POLICY IN ARMYINUSE
                WHEN 2 MOVE '1113070601020912031005140804' TO POLICY IN ARMYINUSE
                WHEN 3 MOVE '1113010203070905100612140804' TO POLICY IN ARMYINUSE
                WHEN 4 PERFORM MANUALENTORDER
           END-EVALUATE.

       MANUALENTORDER.
           PERFORM MANENTHELPER VARYING IDX FROM 1 BY 1 UNTIL IDX > 12
           MOVE '08' TO POLICY(13)
           MOVE '04' TO POLICY(14)

       MANENTHELPER.
           PERFORM PRINTUNAME.
           DISPLAY "> "
           ACCEPT POLICY(IDX).
           EVALUATE POLICY(IDX)
                WHEN '08' PERFORM MANENTHELPER
                WHEN '04' PERFORM MANENTHELPER
           END-EVALUATE.

           SET UIDX TO 1.
           SEARCH POLICY VARYING UIDX
           END-SEARCH.
           IF UIDX = IDX
                CONTINUE
           ELSE
                PERFORM MANENTHELPER
           END-IF.


       PRINTUNAME.
           PERFORM PRINTUNAMEHELPER VARYING UIDX FROM 1 BY 1 UNTIL UIDX > 14.

       PRINTUNAMEHELPER.
           DISPLAY UIDX ". " UNITS(UIDX).
