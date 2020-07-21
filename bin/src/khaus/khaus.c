/*
 *	khaus.C
 *	INPUT	ifile.txt m ofile.txt
 *		ifile.txt:	contiene due vettori colonna (space-separated) R1, Rw ognuno dei quali è un rankvector
 *			   	(gli elementi a cui è assegnato un certo rank-value sono in corrispondenza biunivoca con le righe)
 *		m		numero di elementi ranked (numero di righe)
 *		ofile.txt	contiene il valore di K_haus(R1, R2)
 */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
	
	FILE 			*fp, *outfp;
	unsigned int		m, i, j;
	unsigned short		*ranking, r1_i, r1_j, r2_i, r2_j;
	int			diff_1, diff_2;
	unsigned long long int	S, T, U;
	long double		k_haus;


	if (argc < 4) { /* no parameters */
		printf("usage: $s <ifile.txt> <int: number of elements> <ofile.txt>\n", argv[0]);
		exit(EXIT_FAILURE);
	}


	/* READING ARGS */
	m = (unsigned int) atoi(argv[2]);
	/*printf("m: %u\n", m);*/

	fp = fopen(argv[1], "r");
	if (fp == NULL) { /* error */
		printf("ERROR: unable to open $s\n", argv[1]);
		exit(EXIT_FAILURE);
	}


	/* ALLOCATE m*2 unsigned short array dynamically */
	ranking = (unsigned short *)malloc(2*m*sizeof(unsigned short));
	if (ranking == NULL) { /* error */
		printf("ERROR: unable to dynamically m-alloc-ate memory.");
		exit(EXIT_FAILURE);	
	}

	
	/* READ from fp */
	i = 0;
	while(fscanf(fp, "%u %u", &ranking[2*i], &ranking[2*i+1]) != EOF)
		i++;


	/* Computing K_haus... */
	S=0; T=0; U=0;
	for (i=0; i<(m-1); ++i)
		for (j=i+1; j<m; ++j) {

			r1_i = ranking[2*i]; r2_i = ranking[2*i+1];
			r1_j = ranking[2*j]; r2_j = ranking[2*j+1];

/*			printf("r1_i=%u\tr2_i=%u\nr1_j=%u\tr2_j=%u\n\n", r1_i, r1_j, r2_i, r2_j);*/

			if ((r1_i == r1_j) && (r2_i != r2_j))
				S++;
			else if ((r1_i != r1_j) && (r2_i == r2_j))
				T++;
			else if ((r1_i != r1_j) && (r2_i != r2_j)) {

				diff_1 = ((int ) r1_i) - ((int ) r1_j); 
				diff_2 = ((int ) r2_i) - ((int ) r2_j); 
				if ((diff_1 * diff_2) < 0)
					U++;
			} 
		} 
	
/*
	printf("S = %u\n", S);
	printf("T = %u\n", T);
	printf("U = %u\n", U);
 */
	U = U + (S >= T ? S : T);	
	k_haus = (double)U;
	k_haus = k_haus / ((double)m * (m-1) / 2);

/*
	printf("k_haus = %Lf\n", k_haus);
*/

	outfp = fopen(argv[3], "w");
	if (!outfp) {
		printf("ERROR: unable to open output file $s\n", argv[3]);
		printf("K_haus is: %Lf\n", k_haus);
	}

	fprintf(outfp, "%Lf\n", k_haus);

/*      
 *	***** DEBUGGING *****
	for (i=0; i<m; ++i)
		printf("%u\t%u\n", ranking[2*i], ranking[2*i+1]);	
 *	*********************
 */

	fclose(fp);
	fclose(outfp);

	return(0);	
}