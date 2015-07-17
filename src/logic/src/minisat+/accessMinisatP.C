/**************************************************************************************************

accessMinisatP.c - modified Main.C from (C) Niklas Een, Niklas Sörensson, 2004

**************************************************************************************************/


#include <cstdarg>
#include <unistd.h>
#include <signal.h>
#include "MiniSat.h"
#include "PbSolver.h"

#include "accessMinisatP.h"

// convenient for some output
#include<iostream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using namespace std;

//=================================================================================================
// Options referenced by other files ...

bool     opt_satlive   = true;

bool     opt_ansi      = true;
char*    opt_cnf       = NULL;
int      opt_verbosity = 1;
SolverT  opt_solver        = st_MiniSat;
bool     opt_try       = false;     // (hidden option -- if set, then "try" to parse, but don't output "s UNKNOWN" if you fail
ConvertT opt_convert       = ct_Mixed;
ConvertT opt_convert_goal  = ct_Undef;
bool     opt_convert_weak  = true;
double   opt_bdd_thres     = 3;
double   opt_sort_thres    = 20;
double   opt_goal_bias     = 3;
Int      opt_goal          = Int_MAX;
bool     opt_branch_pbvars = false;
int      opt_polarity_sug  = 1;

char*    opt_input  = NULL;
char*    opt_result = NULL; 
//=================================================================================================

// these are signal handlers form main.c of minisat+ - no idea if they are used, lets see
static void SIGINT_handler(int signum) {
    reportf("\n");
    reportf("*** INTERRUPTED ***\n");
    SatELite::deleteTmpFiles();
    //_exit(0); 
}     // (using 'exit()' rather than '_exit()' sometimes causes the solver to hang (why?))


static void SIGTERM_handler(int signum) {
    reportf("\n");
    reportf("*** TERMINATED ***\n");
    //outputResult(*pb_solver, false);
    SatELite::deleteTmpFiles();
    //_exit(pb_solver->best_goalvalue == Int_MAX ? 0 : 10); 
}



PbSolver::solve_Command opt_command;
PbSolver*   pb_solver = NULL;
vec<Lit> out_ps; vec<Int> out_cs; 

//possibly get more options passed
//annoying: user has to pass number of constraints and variables
void initPbSolving(int n_var, int n_con, int cmd=0){
    //cout << " in init solving with " << n_var << " vars " << endl;
    pb_solver = NULL;
    pb_solver = new PbSolver();

    signal(SIGINT , SIGINT_handler);
    signal(SIGTERM, SIGTERM_handler);

    //pb_solver->verbosity=0;
    opt_verbosity=0;
    switch (cmd){
        case 1: opt_command = PbSolver::sc_FirstSolution;
        case 2: opt_command = PbSolver::sc_AllSolutions;
        default: opt_command = PbSolver::sc_Minimize;
    }
    pb_solver->allocConstrs(n_var, n_con);
}


void addConstraint(char *ps[], int *cs, int n_p, int rhs, int ineq){

    out_ps.clear(); // global var that is cleared instead of local one - 
    out_cs.clear(); // avoid repeated memory allocation
    //out_ps.push(Lit(solver.getVar(parseIdent(in, tmp)))); 
    //actually, they use solver.getVar(...) here ... should we rely on OCaml user?
    int i;
    //char string[50];
    for (i=0; i<n_p; i++){
        //cout << ps[i] <<endl;
        out_ps.push(Lit(pb_solver->getVar(ps[i])));
        out_cs.push(cs[i]);
    }
    if (ineq == 3) {
     pb_solver->addGoal(out_ps, out_cs);
    } else {
    pb_solver->addConstr(out_ps, out_cs, rhs, ineq);
    }
//    cout <<"Adding constraint w "<<n_p<<" components, rhs "<<rhs<<" of type "<<ineq<<endl;
}


int solvePb(char *result[]){
    int true_vars=0;
    bool optimum=true;

    pb_solver->solve(opt_command);

    if (pb_solver->goal == NULL && pb_solver->best_goalvalue != Int_MAX)
        opt_command = PbSolver::sc_FirstSolution;    // (otherwise output will be wrong)
    if (!pb_solver->okay())
        opt_command = PbSolver::sc_Minimize;         // (HACK: Get "UNSATISFIABLE" as output)


    if (opt_command == PbSolver::sc_FirstSolution)
        optimum=false;



    if (optimum){
        if (pb_solver->best_goalvalue == Int_MAX) 
	    { //printf("s UNSATISFIABLE\n");
	    true_vars=-1;
	    }
        else                             {/*printf("s OPTIMUM FOUND\n");*/}
    }else{
        if (pb_solver->best_goalvalue == Int_MAX) 
	    {
	    //printf("s UNKNOWN\n");
	    true_vars=-2;
	    }
        else                             {/*printf("s SATISFIABLE\n");*/ }
    }

    if (pb_solver->best_goalvalue != Int_MAX){
        //printf(" result found: ");
        for (int i = 0; i < pb_solver->best_model.size(); i++){
  //          printf(" %s%s", pb_solver->best_model[i]?"":"-", pb_solver->index2name[i]);
	    if (pb_solver->best_model[i]){
		//result[true_vars] = malloc(30);
	        result[true_vars++] = (char*) (pb_solver->index2name[i]);
		}
	    }
    //    printf("\n");
    }
    //delete (pb_solver);
    signal(SIGINT , SIG_DFL);
    signal(SIGTERM, SIG_DFL);
    return true_vars;
}



void reportf(const char* format, ...)
{
    static bool col0 = true;
    static bool bold = false;
    va_list args;
    va_start(args, format);
    char* text = vnsprintf(format, args);
    va_end(args);

    for(char* p = text; *p != 0; p++){
        if (col0 && opt_satlive)
            putchar('c'), putchar(' ');

        if (*p == '\b'){
            bold = !bold;
            col0 = false;
        }else{
            putchar(*p);
            col0 = (*p == '\n' || *p == '\r');
        }
    }
    fflush(stdout);
}
