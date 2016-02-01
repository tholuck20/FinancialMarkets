//
// Created by mneri092115 on 01/02/2016.
//

#include "MonteCarlo.hpp"


/* 1. Tirer au hasard une trajectoire de S dans l'univers risque-neutre
 * 2. Calculer le flux payé à l'échéance par l'actif dérivé pour cette trajectoire
 * 3. Répéter les étapes 1 et 2 de façon à disposer d'un grand nombre de flux terminaux
      de l'actif dérivé dans l'univers risque-neutre
 * 4. Calculer la moyenne des flux terminaux pour obtenir une estimation de l'espérance du flux terminal dans l'univers
      risque-neutre
 * 5. Actualiser le flux terminal espéré au taux d'intérêt sans risque pour obtenir une estimation de la valeur
      de l'actif dérivé.
*/


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

const uint64_t RR = RAND_MAX*RAND_MAX;

double MonteCarlo(int n) {
  int k=0;
  for (int i=0; i<n; ++i) {
    uint64_t x=rand(), y=rand();
    if (x*x + y*y <= RR) k++;
  }
  return 4.0*k/n;
}

double Abso(double x) {return (x<0)? -x : x;}

int main() {
  printf("RAND_MAX = %u",RAND_MAX);

  printf("\n\nMonteCarlo:\n");
  for (int n=10; n<=100000000; n*=10) printf("  n=%9i: pi=%.14f\n",n,MonteCarlo(n));
  printf("prÃ©cis:        pi=3.14159265358979");

  printf("\n\nMonteCarlo:  srand(478)\n");
  for (int n=10; n<=100000000; n*=10) {
    srand(478);
    printf("  n=%9i: pi=%.14f\n",n,MonteCarlo(n));
  }
  printf("prÃ©cis:        pi=3.14159265358979");

  getchar();
}
