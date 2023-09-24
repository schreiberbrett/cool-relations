#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

// A variable is just an int
typedef int variable;

// A polynomial is a sum of monomials
struct Polynomial
{
    struct Monomial *monomials;
    int number_of_monomials;
};

// A monomial is a nonempty product of (sorted) variables
struct Monomial
{
    variable *variables;
    int number_of_variables;
};

// A multivariate Horner scheme is of the form xA + B, where x is a variable factored out of A. And A and B are Horner schemes. When A or B is NULL, represents the constant 1.
struct HornerScheme
{
    variable factor;
    struct HornerScheme *multiplicand;
    struct HornerScheme *summand;
};

void print_variable(variable v) {
    char buffer[300];
    snprintf(buffer, sizeof buffer, "%d", v);

    printf("x");
    for (int i = 0; i < 300; i++) {
        switch (buffer[i]) {
            case '0': printf("₀"); break;
            case '1': printf("₁"); break;
            case '2': printf("₂"); break;
            case '3': printf("₃"); break;
            case '4': printf("₄"); break;
            case '5': printf("₅"); break;
            case '6': printf("₆"); break;
            case '7': printf("₇"); break;
            case '8': printf("₈"); break;
            case '9': printf("₉"); break;
            case 0: return;
            default: putchar(buffer[i]); break;
        }
    }
}

void print_Polynomial(struct Polynomial p) {
    for(int i = 0; i < p.number_of_monomials; i++) {
        if (i != 0) {
            printf(" + ");
        }

        struct Monomial m = p.monomials[i];
        for (int j = 0; j < m.number_of_variables; j++) {
            print_variable(m.variables[j]);
        }
    }
}

void print_HornerScheme(struct HornerScheme hs) {
    print_variable(hs.factor);

    if (hs.multiplicand != NULL) {
        printf("(");
        print_HornerScheme(*hs.multiplicand);
        printf(")");
    }

    if (hs.summand != NULL) {
        printf(" + ");
        print_HornerScheme(*hs.summand);
    }
}

struct HornerScheme *convert(struct Polynomial p)
{
    if (p.number_of_monomials == 0) {
        return NULL;
    }

    // First, find the largest variable index.
    variable largest_variable = -1;
    for (int i = 0; i < p.number_of_monomials; i++)
    {
        struct Monomial m = p.monomials[i];

        // m.variables always nonempty
        variable largest_v_in_m = m.variables[m.number_of_variables - 1];

        if (largest_v_in_m > largest_variable)
        {
            largest_variable = largest_v_in_m;
        }
    }

    // count[i] = # of monomials that variable i occurs in.
    int *count = calloc(largest_variable + 1, sizeof(int));
    for (int i = 0; i < p.number_of_monomials; i++)
    {
        struct Monomial m = p.monomials[i];

        for (int j = 0; j < m.number_of_variables; j++)
        {
            if (j == 0 || m.variables[j] != m.variables[j - 1])
            {
                variable v = m.variables[j];
                count[v]++;
            }
        }
    }

    // Find the variable that occurs in the most monomials
    variable most_frequent_variable = 0;
    for (variable v = 1; v < largest_variable + 1; v++)
    {
        if (count[v] > count[most_frequent_variable])
        {
            most_frequent_variable = v;
        }
    }

    // Split the polynomial in two.
    // Any monomials containing a copy of the most frequent variable become the multiplicand.
    // Any monomials without the most frequest variable become the summand.
    struct Polynomial multiplicand;
    struct Polynomial summand;

    multiplicand.number_of_monomials = 0;
    multiplicand.monomials = malloc(sizeof(struct Monomial) * p.number_of_monomials);

    summand.number_of_monomials = 0;
    summand.monomials = malloc(sizeof(struct Monomial) * p.number_of_monomials);

    for (int i = 0; i < p.number_of_monomials; i++) {
        struct Monomial m = p.monomials[i];

        bool m_contains_most_frequent_variable = false;
        for (int j = 0; j < m.number_of_variables; j++) {
            if (m.variables[j] == most_frequent_variable) {
                m_contains_most_frequent_variable = true;
                break;
            }
        }

        if (m_contains_most_frequent_variable) {
            if (m.number_of_variables > 1) {
                // Make a near-copy of m omitting the variable...
                struct Monomial new_m;
                new_m.number_of_variables = 0;
                new_m.variables = malloc(sizeof(struct Monomial) * (m.number_of_variables - 1));

                bool found = false;
                for (int j = 0; j < m.number_of_variables; j++) {
                    if (m.variables[j] == most_frequent_variable && !found) {
                        found = true;
                    } else {
                        new_m.variables[new_m.number_of_variables] = m.variables[j];
                        new_m.number_of_variables++;
                    }
                }

                // ... and add it to the multiplicand
                multiplicand.monomials[multiplicand.number_of_monomials] = new_m;
                multiplicand.number_of_monomials++;
            }
        } else {
            // Add the monomial, unchanged, to the summand
            summand.monomials[summand.number_of_monomials] = m;
            summand.number_of_monomials++;
        }
    }

    // These are causing a segfault, not sure why
    //multiplicand.monomials = realloc(multiplicand.monomials, multiplicand.number_of_monomials);
    //summand.monomials = realloc(summand.monomials, summand.number_of_monomials);

    struct HornerScheme *result = malloc(sizeof(struct HornerScheme));
    result->factor = most_frequent_variable;
    result->multiplicand = convert(multiplicand);
    result->summand = convert(summand);

    return result;
}

/*
Input:	x₀x₁ + x₀x₂
Output:	x₀(x₁ + x₂)

Input:	x₀x₁x₂x₃ + x₀x₁x₃x₅ + x₂x₃ + x₄x₅ + x₅x₆x₇
Output:	x₃(x₀(x₁(x₂ + x₅)) + x₂) + x₅(x₄ + x₆(x₇))
*/
int main() {
    struct Polynomial p1 = (struct Polynomial){
        .number_of_monomials = 2,
        .monomials = (struct Monomial[]){
            {
                .number_of_variables = 2,
                .variables = (variable[]){0, 1}
            },

            {
                .number_of_variables = 2,
                .variables = (variable[]){0, 2}
            }
        }
    };

    struct HornerScheme *hs1 = convert(p1);

    printf("Input:\t");
    print_Polynomial(p1);
    printf("\n");

    printf("Output:\t");
    print_HornerScheme(*hs1);
    printf("\n\n");


    struct Polynomial p2 = (struct Polynomial){
        .number_of_monomials = 5,
        .monomials = (struct Monomial[]){
            {
                .number_of_variables = 4,
                .variables = (variable[]){0, 1, 2, 3}
            },

            {
                .number_of_variables = 4,
                .variables = (variable[]){0, 1, 3, 5}
            },

            {
                .number_of_variables = 2,
                .variables = (variable[]){2, 3}
            },

            {
                .number_of_variables = 2,
                .variables = (variable[]){4, 5}
            },

            {
                .number_of_variables = 3,
                .variables = (variable[]){5, 6, 7}
            }
        }
    };

    struct HornerScheme *hs2 = convert(p2);

    printf("Input:\t");
    print_Polynomial(p2);
    printf("\n");

    printf("Output:\t");
    print_HornerScheme(*hs2);
    printf("\n\n");

    
    return 0;
}