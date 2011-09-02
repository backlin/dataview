#include "dataview.h"

SEXP get_char(){
    struct termios tio; 
    int initial_c_lflag;
    SEXP inchar;
    PROTECT(inchar = allocVector(INTSXP, 1));

    // Make sure terminal is in raw mode
    tcgetattr( 0, &tio ); 
    initial_c_lflag = tio.c_lflag;
    tio.c_lflag &= ~ICANON; 
    tcsetattr( 0, TCSANOW, &tio ); 

    INTEGER(inchar)[0] = getchar();
    __fpurge(stdin);

    // Reset terminal mode
    tio.c_lflag = initial_c_lflag;
    tcsetattr( 0, TCSANOW, &tio ); 

    UNPROTECT(1);
    return inchar;
    
}

