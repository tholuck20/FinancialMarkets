// Convertisseur de bases
// Created by mneri092115 on 18/01/2016.

#include <cmath>
#include <string>
#include <vector>

/* Algorithme:
 * Variables: Nombre, Base, a, b, x
 *  - I. Trouve "a" : la puissance la plus grande dont le résultat : x =< Nombre, x = Base^a
 *  - II. Trouve le plus grand multiplicateur : b, dont le résultat : y =< Nombre, y = b * Base^a = b * x
 *  - III. Soustraire : Nombre = Nombre - y
 *  - IV. Revenir à la première étape en utilisant la nouvelle valeur de Nombre (reste après première puissance)
 *      -> tester a-1
 *  - V. La puissance donne la position du multiplicateur
 *      -> a donne la position de b
 */
/* Algorithme 2:
 * - I. Nombre Mod Base = Reste1
 * - II. (Nombre-Reste) / Base = NewNombre
 * - III. NewNombre Mod Base = Reste2
 * - ...
 * Les restes sont les chiffres du nombre dans la nouvelle base (Reste1 est le premier (unité), ResteN est le dernier)
*/

// Pas 2 bases car la base initiale est toujours 10 dans cette fonction.
// Une autre fonction permet de passer de la base 'a' à la base 10
// Ainsi, pour passer d'une base 'a' à une base 'b', le programme exécute deux fonctions.

string Base10ToBaseX(double Nombre, int Base)
{
    string c;

    if (Nombre == 0)
        return 0;
    else if (Nombre == 1)
        return "1";

    int a = 0;
    while (pow(Base, a) < Nombre) {
        ++a;
    }
    if (pow(Base,a) > Nombre) {a-=1;}

    int temp = a;
    for (int i = 0; i <= temp; ++i)
    {
        int b = 0;
        while (b * pow(Base, a) < Nombre) {
            ++b;
        }
        if (b * pow(Base,a) > Nombre) {b-=1;}

        c += to_string(b);

        Nombre -= b * pow(Base,a);
        --a;
    }
    return c;
}

int BaseXToBase10(double Nombre, int Base) // or return stoi(to_string(Nombre), nullptr, Base);
{
    // Convert Nombre (double) en Nombre (string)
    string temp = Base10ToBaseX(Nombre, 10);
    int c;  ////////

    int b = temp.size()-1;
    for (int i = 0; i < temp.size(); ++i)
    {
        int a = (int)temp[i] - 48;
        c += a * pow(Base,b);
        --b;
    }
    return c;
}

int BaseConvert(double Nombre, int Base, int BaseRech)
{
    if (BaseRech == 10)
        return BaseXToBase10(Nombre,Base); // int
    else if (Base == 10)
        return stoi(Base10ToBaseX(Nombre,BaseRech)); // stoi(string)
    else if (Base == 16) // Hex to Dec
        return stoi(to_string(BaseXToBase10(Nombre,Base)),nullptr,16); // stoi(string)
    else
    {
        int Nmb = BaseXToBase10(Nombre, Base);
        return stoi(Base10ToBaseX(Nmb,BaseRech)); // stoi(string)
    }
}

int main()
{
    double Nombre;
    int Base;
    int BaseRech;

    cout << "Entrez un nombre : " << endl;
    cin >> Nombre;
    cout << "Entrez la base de ce nombre : " << endl;
    cin >> Base;
    cout << "Entrez la base voulu pour ce nombre : " << endl;
    cin >> BaseRech;

    cout << "Le nombre " << Nombre << " (base " << Base << ") vers la base " << BaseRech << " est : " \
        << BaseConvert(Nombre, Base, BaseRech) << endl;
}
