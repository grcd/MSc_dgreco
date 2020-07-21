
/*
 *  rankgraph.C,       Greco Daniele <grcdaniele@gmail.com>
 *
 *  Given a finite ([N]) list of weights ([weights_file]), computes a ranking
 *  of these weights and write it out ([output_file]).
 */


#include <stdio.h>
#include <stdlib.h>

typedef struct record {
    long double     w;
    unsigned int    index;
} weight_t;


int mycmp(const void *x, const void *y) {

    // x and y are pointers to doubles.

    // Returns +1 if x < y              (non-decresing order)
    //          0 if x == y
    //         -1 if x > y

    weight_t    r1, r2;

    r1 = *(weight_t *)x;
    r2 = *(weight_t *)y;

    if (r1.w < r2.w) {
        return +1;
    } else if (r1.w > r2.w) {
        return -1;
    }
    return 0;
}


int main(int argc, char *argv[]) {

    unsigned int    N;              // number of input values
    weight_t        *weights;       // each input values has a struct weight_t
    unsigned int    *rank;          // and will be assigned a rank
    long double     w;
    unsigned int    r, i;

    FILE            *fp;            // input  file: weights
    FILE            *fo;            // output file: ranks

    if (argc < 4) {
        printf("usage: %s [weights_file] [output_file] [N]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    /*  Open input file containing weights_file */
    fp = fopen(argv[1], "r");
    if (!fp) {
        printf("** error: unable to open the weights file specified.\n");
        exit(EXIT_FAILURE);
    }

    /*  Open output file to write weights_file */
    fo = fopen(argv[2], "w");
    if (!fo) {
        printf("** error: unable to create the output file specified.\n");
        exit(EXIT_FAILURE);
    }

    N = atoi(argv[3]);

    weights = (weight_t *) malloc(N * sizeof(weight_t));
    if (weights == NULL) {
        printf("** error: unable to allocate memory for weights.\n");
        exit(EXIT_FAILURE);
    }

    rank = (unsigned int *) malloc(N * sizeof(unsigned int));
    if (rank == NULL) {
        printf("** error: unable to allocate memory for rank.\n");
        exit(EXIT_FAILURE);
    }

    i = 0;
    while (fscanf(fp, "%Lf", &w) != EOF) {
        weights[i].w = w;
        weights[i].index = i;
        i++;
    }

/*
    // DEBUGGING
    printf("BEFORE sorting...\n");
    for (i = 0; i < N; i++) {
        printf ("%d, weights[%d] = %Lf\n", weights[i].index, i, weights[i].w);
    }
*/

    qsort(weights, N, sizeof(weight_t), &mycmp);

/*
    // DEBUGGING
    printf("\nAFTER sorting...\n");
    for (i = 0; i < N; i++) {
        printf ("%d, weights[%d] = %Lf\n", weights[i].index, i, weights[i].w);
    }
*/

    r = 1;

    rank[weights[0].index] = r;
    for (i = 1; i < N; ++i) {
        if(mycmp(&weights[i], &weights[i-1]))     // if NOT equal
            r++;

        rank[weights[i].index] = r;
    }

    for (i=0; i<N; ++i)
        fprintf(fo, "%d\n", rank[i]);

    fclose(fp); fclose(fo);
    free(rank); free(weights);

    return EXIT_SUCCESS;
}
