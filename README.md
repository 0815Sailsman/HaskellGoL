# HaskellGoL
epic game of life implementation in haskell for wprog project

1. Regeln angucken für Game of life
2. Überlegen, wie man die Daten ablegen will
	→ Idee: belegt Felder als Tupel in Liste
3. Überlegenwo man das Standard Feld abspeichern und auslesen will
	→ auch einfach als Liste von Tupel
	→ sollte aber persitent und mutable irgendwo liegen
3. Game of life Funktion die das Feld bekommt und ein Feld ein Zug später returned

Idee: Maps benutzen um Zellen zu speichern
Ziel sprint 1: „per Hand“ das feld mehrmals in die game of life methode packen und gucken ob es funktioniert

Nächstes Ziel:
Main method zu haben, die 1/s GoL funktion aufruft und feld ausprintet.
Erstmal mit festgelegter view (20x20 felder mit 0-0 in der mitte)

Epik:
GUI-App, in welcher man die Funktion hat, verschiedene Start-Felder zu erstellen, indem man eine „Schachbrett“-Ansicht von einem Koordinatensystem hat und Felder, die belegt sein sollen einfach anklicken kann, dass ganze wird dann persistent irgendwo abgelegt und kann wieder verwendet werden. Danach kann man ein Startfeld auswählen und das Game of Life starten.
Im Spiel kann man dann den angezeigten Bereich beliebig verschieben und die Geschwindigkeit anpassen.

Daten repräsentation:
Zelle = (x:int, y:int, isAlive:bool)

Auswertung:
Als erstes per List comprehension alle Lebenden Zellen und alle Zellen, welche an diese angrenzen in eine Liste packen.
Dann aus dieser Liste alle doppelten Elemente löschen.
Danach diese Liste durchgehen und entscheiden, was mit jeder einzelnen Zelle passiert:
	Für Effizienz kann man zur auswertung nur die Ursprüngliche Liste aller lebenden Zellen
	betrachten, da tote Zellen für die Auswertung nicht relevant sind.







