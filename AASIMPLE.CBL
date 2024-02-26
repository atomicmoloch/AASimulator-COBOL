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
               88 SEA-TRUE  VALUE '2'.
               88 AMPH-TRUE VALUE '3'.
      * Note: If land units added in sea battle, assumed carried on transport
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
           02 TROOPS PIC 999 OCCURS 14 TIMES.
           02 POLICY PIC 99 OCCURS 14 TIMES.
           02 SPECIAL.
               03 LEAVE1LAND PIC X.
                   88 LEAVE1LAND-TRUE VALUE 'Y'.
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
           02 TROOPS PIC 999 OCCURS 14 TIMES.
           02 POLICY PIC 99 OCCURS 14 TIMES.
           02 SPECIAL.
               03 LEAVE1LAND PIC X.
                   88 LEAVE1LAND-TRUE VALUE 'Y'.
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
           02 TROOPS PIC 999 OCCURS 14 TIMES.
           02 POLICY PIC 99 OCCURS 14 TIMES.
           02 SPECIAL.
               03 LEAVE1LAND PIC X.
                   88 LEAVE1LAND-TRUE VALUE 'Y'.
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


       01 WORKING-MEMORY
          02 MENUVARS.
            03 CHOOSEVAR     PIC 99.
            03 MAINCHOOSEVAR PIC 99 VALUE 1.
            03 ARMYCHOOSEVAR PIC 99 VALUE 1.
            03 BATTCHOOSEVAR PIC 99 VALUE 1.
            03 YNVAR PIC X.
                88 YNVAL YNVAL 'Y'
          02 INDEXVARS.
      * UIDX - for bottom level iterations, like displaying unit lists
            03 IDX           PIC 99.
            03 UIDX          PIC 99.
          02 UNIT-INFO.
      * To be used for storing information about a specific unit
                03 UNITID        PIC 99.
                03 UNITCOST      PIC 99.
                03 UNITATTK      PIC 99.9.
                03 UNITDEF       PIC 99.
            02 MISC          PIC 99.
            02 MISCDEC       PIC 99.99.
            02 DICE.
                03 RANDOMRAW     PIC 9.99.
                03 DICEROLL      PIC 9.
            02 PREDICTBATTLE.
                03 UNROUNDED 9999.99.
                03 ROUNDED   9999.

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

       PERFORM MAIN UNTIL MAINCHOOSEVAR = 0.

       MAIN.
            DISPLAY "0. EXIT PROGRAM"
            DISPLAY "1. ATTACKING ARMY MENU"
            DISPLAY "2. DEFENDING ARMY MENU"
            DISPLAY "3. SWAP ATTACKING AND DEFENDING ARMIES"
            DISPLAY "4. BATTLE MENU"
            DISPLAY "> "
            ACCEPT MAINCHOOSEVAR.

            EVALUATE MAINCHOOSEVAR
                WHEN 1
                    SET ARMYCHOOSEVAR TO 1
                    MOVE ATTACKARMY TO ARMYINUSE
                    PERFORM ARMYMENU UNTIL ARMYCHOOSEVAR = 0
                    MOVE ARMYINUSE TO ATTACKARMY
                WHEN 2
                    SET ARMYCHOOSEVAR TO 1
                    MOVE DEFENDARMY TO ARMYINUSE
                    PERFORM ARMYMENU UNTIL ARMYCHOOSEVAR = 0
                    MOVE ARMYINUSE TO DEFENDARMY
                WHEN 3
                    MOVE DEFENDARMY TO ARMYINUSE
                    MOVE ATTACKARMY TO DEFENDARMY
                    MOVE ARMYINUSE TO ATTACKARMY
                    DISPLAY "ARMIES SWITCHED"
                WHEN 4
                    SET BATTCHOOSEVAR TO 1
                    PERFORM BATTLEMENU UNTIL BATTCHOOSEVAR = 0
            END EVALUATE.


       ARMYMENU.
         DISPLAY "0. GO BACK"
         DISPLAY "1. BUILD ARMY MANUALLY"
         DISPLAY "2. LOAD FROM FILE (COMING SOON)"
         DISPLAY "3. SAVE TO FILE (COMING SOON)"
         DISPLAY "4. VIEW ARMY TROOPCOUNTS"
         DISPLAY "5. VIEW ARMY TECHNOLOGY/SPECIAL POLICIES"
         DISPLAY "6. VIEW ARMY REMOVAL PRIORITIZATION ORDER"
         DISPLAY "7. EDIT ARMY TROOPCOUNTS"
         DISPLAY "8. EDIT ARMY TECHNOLOGY/SPECIAL POLICIES"
         DISPLAY "9. EDIT ARMY REMOVAL PRIORITIZATION ORDER"
         DISPLAY "10. RESEARCH DICE SIMULATOR"
         DISPLAY "> ".
         ACCEPT ARMYCHOOSEVAR.

         EVALUATE ARMYCHOOSEVAR
            WHEN 1
                PERFORM BUILDARMY
            WHEN 4
                PERFORM PRINTARMYUNITS
            WHEN 5
                PERFORM PRINTSPECIAL
            WHEN 6
                PERFORM PRINTARMYORDER
            WHEN 7
                PERFORM GETUNITS
            WHEN 8
                PERFORM GETSPECIAL
            WHEN 9
                PERFORM GETORDER
            WHEN 10
                PERFORM RESEARCHDICE
        END-EVALUATE.

       RESEARCHDICE.
            DISPLAY "ENTER NUMBER OF RESEARCH DICE PURCHASED"
            DISPLAY "> "
            ACCEPT MISC
            PERFORM VARYING UIDX FROM 1 BY 1 UNTIL UIDX > 8
                COMPUTE MISCDEC = (1 - ((6-MISC)/6)**UIDX)
                DISPLAY 'CHANCE AFTER 'UIDX' ROUNDS: 'MISCDEC
            END-PERFORM
            DISPLAY "PRESS ENTER TO CONTINUE..."
            ACCEPT MISC


       BUILDARMY.
           PERFORM GETUNITS.
           PERFORM GETSPECIAL.
           PERFORM GETORDER.

       GETSPECIAL.
           DISPLAY "LEAVE ONE LAND UNIT ALIVE AT ALL COSTS (NEEDED FOR
      -    "CAPTURING LAND TERRITORY)? (Y/N)"
           DISPLAY "> "
           ACCEPT  LEAVE1LAND IN ARMYINUSE.
           DISPLAY "ENTER TECHNOLOGICAL ADVANCES REACHED"
           DISPLAY "ADVANCED ARTILLERY ACHIEVED? (Y/N)".
           DISPLAY "> ".
           ACCEPT AARTILLERY IN ARMYINUSE.
           DISPLAY "SUPER SUBMARINES ACHIEVED? (Y/N)".
           DISPLAY "> ".
           ACCEPT SSUBMARINE IN ARMYINUSE.
           DISPLAY "BETTER JET FIGHTERS ACHIEVED? (Y/N)".
           DISPLAY "> ".
           ACCEPT JETFIGHTER IN ARMYINUSE.
           DISPLAY "IMPROVED SHIPYARDS ACHIEVED? (Y/N)".
           DISPLAY "> ".
           ACCEPT IMPRVDSHIP IN ARMYINUSE.
           DISPLAY "AA GUN RADAR ACHIEVED? (Y/N)".
           DISPLAY "> ".
           ACCEPT AAGUNRADAR IN ARMYINUSE.
           DISPLAY "HEAVY BOMBERS ACHIEVED? (Y/N)".
           DISPLAY "> ".
           ACCEPT HEAVYBOMBR IN ARMYINUSE.


       GETUNITS.
           DISPLAY "ENTER UNIT TROOPCOUNT VALUES".
           PERFORM GETUNIT-HELPER VARYING IDX FROM 1 BY 1 UNTIL IDX > 14.
       GETUNIT-HELPER.
           DISPLAY "HOW MANY " UNITS(IDX)" UNITS? ".
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

       PRINTARMYUNITS.
            PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 14
                DISPLAY "NUMBER OF " UNITS(IDX) ": " TROOPS(IDX) IN ARMYINUSE
            END-PERFORM.

       PRINTARMYORDER.
            DISPLAY "ORDER OF TROOPS TO REMOVE"
            PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 14
                DISPLAY IDX ". " TROOPS(POLICY(IDX)) IN ARMYINUSE
            END-PERFORM.

       PRINTSPECIAL.
            IF LEAVE1LAND-TRUE IN ARMYINUSE
                DISPLAY "1 LAND UNIT WILL BE KEPT ALIVE AT ALL COSTS"
            END-IF.
            DISPLAY "TECHNOLOGY:"
            IF AARTILLERY-TRUE IN ARMYINUSE
                DISPLAY "ADVANCED ARTILLERY"
            END-IF.
            IF SSUBMARINE-TRUE IN ARMYINUSE
                DISPLAY "SUPER SUBMARINES"
            END-IF.
            IF JETFIGHTER-TRUE IN ARMYINUSE
                DISPLAY "IMPROVED JET FIGHTERS"
            END-IF.
            IF IMPRVDSHIP-TRUE IN ARMYINUSE
                DISPLAY "IMPROVED SHIPYARDS"
            END-IF.
            IF AAGUNRADAR-TRUE IN ARMYINUSE
                DISPLAY "AA GUN RADAR"
            END-IF.
            IF HEAVYBOMBR-TRUE IN ARMYINUSE
                DISPLAY "HEAVY BOMBERS"
            END-IF.

       PRINTUNAME.
           PERFORM PRINTUNAMEHELPER VARYING UIDX FROM 1 BY 1 UNTIL UIDX > 14.

       PRINTUNAMEHELPER.
           DISPLAY UIDX ". " UNITS(UIDX).

       BATTLEMENU.
           DISPLAY "0. GO BACK"
           DISPLAY "1. PREDICTED BATTLE"
           DISPLAY "2. SIMULATED BATTLE"
           DISPLAY "> "
           ACCEPT BATTCHOOSEVAR.

           EVALUATE BATTCHOOSEVAR
              WHEN 1
                PERFORM PREDICTBATTLE
              WHEN 2
                PERFORM SIMBATTLE
           END-EVALUATE.

       PREDICTBATTLE.
            DISPLAY "1. LAND BATTLE"
            DISPLAY "2. SEA BATTLE"
            DISPLAY "3. AMPHIBIOUS ASSAULT"
            DISPLAY "> "
            ACCEPT BATTLETYPE


        GETCOST.
            EVALUATE UNITID
                WHEN 1
                    SET UNITCOST TO 3
                WHEN 2
                    SET UNITCOST TO 4
                WHEN 3
                    SET UNITCOST TO 5
                WHEN 4
                    SET UNITCOST TO 6
                WHEN 5
                    SET UNITCOST TO 10
                WHEN 6
                    SET UNITCOST TO 12
                WHEN 7
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 5
                    ELSE
                        SET UNITCOST TO 6
                    END-IF
                WHEN 8
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 6
                    ELSE
                        SET UNITCOST TO 7
                    END-IF
                WHEN 9
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 7
                    ELSE
                        SET UNITCOST TO 8
                    END-IF
                WHEN 10
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 12
                    ELSE
                        SET UNITCOST TO 10
                    END-IF
                WHEN 11
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 11
                    ELSE
                        SET UNITCOST TO 14
                    END-IF
                WHEN 12
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 11
                    ELSE
                        SET UNITCOST TO 14
                    END-IF
                WHEN 13
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 20
                    ELSE
                        SET UNITCOST TO 17
                    END-IF
                WHEN 14
                    IF IMPRVDSHIP-TRUE IN ARMYINUSE THEN
                        SET UNITCOST TO 20
                    ELSE
                        SET UNITCOST TO 17
                    END-IF
            END-EVALUATE.

        GETATTACK.
            EVALUATE UNITID
                WHEN 1
                    SET UNITATTK TO 1
                WHEN 2
                    SET UNITATTK TO 2
                WHEN 3
                    SET UNITATTK TO 3
                WHEN 4
                    SET UNITATTK TO 0
                WHEN 5
                    IF JETFIGHTER-TRUE IN ARMYINUSE THEN
                        SET UNITATTK TO 4
                    ELSE
                        SET UNITATTK TO 3
                    END-IF
                WHEN 6
                    IF HEAVYBOMBR-TRUE IN ARMYINUSE THEN
                        SET UNITATTK TO 5.3
                    ELSE
                        SET UNITATTK TO 4
                WHEN 7
                    IF SSUBMARINE-TRUE IN ARMYINUSE THEN
                        SET UNITATTK TO 3
                    ELSE
                        SET UNITATTK TO 2
                    END-IF
                WHEN 8
                    SET UNITATTK TO 0
                WHEN 9
                    SET UNITATTK TO 2
                WHEN 10
                    SET UNITATTK TO 3
                WHEN 11
                    SET UNITATTK TO 1
                WHEN 12
                    SET UNITATTK TO 1
                WHEN 13
                    SET UNITATTK TO 4
                WHEN 14
                    SET UNITATTK TO 4
            END-EVALUATE.

        GETDEFENSE.
            EVALUATE UNITID
                WHEN 1
                    SET UNITDEF TO 2
                WHEN 2
                    SET UNITDEF TO 2
                WHEN 3
                    SET UNITDEF TO 3
                WHEN 4
                    IF AAGUNRADAR-TRUE IN ARMYINUSE THEN
                        SET UNITDEF TO 2
                    ELSE
                        SET UNITDEF TO 1
                    END-IF
                WHEN 5
                    SET UNITDEF TO 4
                WHEN 6
                    SET UNITDEF TO 1
                WHEN 7
                    SET UNITDEF TO 1
                WHEN 8
                    SET UNITDEF TO 0
                WHEN 9
                    SET UNITDEF TO 2
                WHEN 10
                    SET UNITDEF TO 3
                WHEN 11
                    SET UNITDEF TO 2
                WHEN 12
                    SET UNITDEF TO 2
                WHEN 13
                    SET UNITDEF TO 4
                WHEN 14
                    SET UNITDEF TO 4
            END-EVALUATE.




        CALCULATECOST.


        CALCULATEATTACK.

        CALCULATEDEFENSE.

        REMOVELANDUNIT.


        REMOVESEAUNITS.


        REMOVETRANSPORTUNITS.
            IF (TROOPS(01) IN ARMYINUSE > 0) THEN
                SUBTRACT 1 FROM TROOPS(01)
                PERFORM REMOVELANDUNIT
            ELSE
                PERFORM REMOVELANDUNIT 2 TIMES
            END-IF.


       SEAATTACK.


       GETDICEROLL.
            COMPUTE RANDOMRAW = FUNCTION RANDOM
            COMPUTE DICEROLL ROUNDED = RANDOMRAW TIMES 6

