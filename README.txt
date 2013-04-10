AVRAM Marius 324CB - Tema 2 PP 

Expresii regulate 

Pentru rezolvarea temei am creeat un tip de date numit State. Acesta este un tip de arbore binar care contine in fiecare nod cate un caracter, iar in vectorul de caractere atasat fiecarui nod pot sta informatii despre expresiile de genul celor incadrate de paranteze patrate []. Pentru ca un string sa indeplineasca conditia impusa de "|" am creat un nod care are drept caracter "|" iar descendentii sai reprezinta cele doua cai pe care poate merge stringul. Daca dupa parcurgerea uneia din cai se ajunge la sfarsit (End) atunci stringul sa potriveste cu expresia regulata data. Pentru '|' incadrate arborele se va ramifica destul de mult si vor fi nevoie de cautari extensive insa in cele din urma rezultatul va fi aflta. 

In cazul in care avem un caracter sau o paranteza care trebuie sa indeplineasca conditia impusa de "*" se construieste un Sub-arbore care are ca vecin stang portiunea ce trebuie repetata. Aceasta portiune se termina cu Redo in loc de End. Iar la dreata nodului "*" se afla portiunea ce trebuie urmata dupa ce s-a parcurs portiunea ce trebuie repetata (de 0 sau mai multe ori). Pentru a verifica un strig cu acest arbore a fost nevoie sa fac o functie auxiliara care parcurge arborele de la nodul "*". Era nevoie de acest lucru pentru ca o data ajuns la Redo nu mai puteam sa ma intorc. Insa cu o functie auxiliara stiam sau nu daca am ajuns la Redo. Iar in cazul in care am ajuns mai incercam o data (tot cu functia auxiliara) sau daca nu s-a ajuns la Redo atunci ma duceam pe ramura dreapta.

Problema cu aceasta abordare a fost ca este greu sa implementez un regex care sa suporte expresii de genul "(12(34)*)*".

Astfel programul face matchin pe orice tip de expresie cu exceptia expresiilor care contin secvente de repetitie incadrate. Ca cea de mai sus. 
