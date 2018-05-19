# Game-about-squares

Proiectul urmărește implementarea jocului Game about Squares, și a unui mecanism de rezolvare a oricărui nivel, utilizând căutare
leneșă în spațiul stărilor. În acest sens, se va întrebuința o versiune specială a parcurgerii în adâncime, numită adâncire
iterativă.

Reprezentarea și afișarea nivelurilor

În fișierul GAS.hs, sunt definite tipurile Position, Color, Heading, Object, pentru un obiect oarecare (pătrat/ cerc/ săgeată),
și Level, pentru un întreg nivel al jocului. Pentru reprezentarea unui nivel am utilizat o structură Map.

De asemenea, am construit reprezentările textuale ale valorilor celor două tipuri, instanțiind clasa Show. 
Convenții de afișare a unui nivel:

    Un pătrat este reprezentat prin două caractere: inițiala majusculă a culorii și orientarea. Exemplu:
        Un pătrat albastru (blue) orientat către nord este redat prin B^.
    Un cerc este reprezentat printr-un singur caracter, inițiala minusculă a culorii. Exemplu:
        Un cerc albastru (blue) este redat prin b.
    O săgeată este reprezentată printr-un singur caracter, orientarea. Exemplu:
        O săgeată orientată spre est este redată prin >.
    O poziție a tablei poate conține cel mult două obiecte, întrucât un pătrat poate ajunge pe aceeași poziție cu un cerc sau o 
    săgeată. Prin urmare, vom utiliza trei caractere pentru reprezentarea unei poziții: primele două, aferente eventualului 
    pătrat, iar ultimul, corespunzător eventualului cerc/ săgeată. Lipsa obiectelor se desemnează prin spații. Exemple (un punct 
    reprezintă un spațiu):
        Un pătrat albastru (blue) orientat către nord este redat prin B^.. Observați spațiul lipsă de la final, pentru că nu 
        avem și cerc/ săgeată.
        Un cerc albastru (blue) este redat prin ..b. Observați cele două spații lipsă de la început, pentru că nu avem și pătrat.
        Suprapunerea celor două obiecte de mai sus este redată prin B^b.
    În cadrul unui nivel, coloanele sunt separate prin |, iar liniile, prin linie nouă.

Integrând aceste convenții, reprezentarea textuală a nivelului din imaginea de mai sus este următoarea (R/r = Red, G/g = Gray):

Rv |   |  r|  g
   |   |   |   
  >|G> |  ^|

Având în vedere că, în urma mutării unor pătrate, suprafața activă de joc se poate micșora sau mări, reprezentarea textuală va 
ocupa întotdeauna numărul minim de linii și coloane. Spre exeplu, pornind de la tabla

Rv 
  r

deplasarea pătratului roșu spre sud va conduce la tabla Rvr, fără alte linii suplimentare.

Funcțiile addSquare, addCircle și addArrow adaugă obiectul corespunzător la o anumită poziție dintr-un nivel.
Funcția move, permite realizarea unei mutări.
Fișierul Levels.hs conține definițiile a cinci niveluri.

Rezolvarea nivelurilor

Fișierul Search.hs conține o mică bibliotecă pentru parcurgerea în adâncime cu adâncire iterativă, într-o manieră independentă 
de problemă. Transparența este asigurată de clasa ProblemState, din fișierul omonim, care expune funcțiile successors și isGoal.

Am implementat și funcțiile limitedDfs, iterativeDeepening și extractPath.  De asemenea, am implementat tipul Node s a, care 
reține informații despre desfășurarea procesului de căutare.
